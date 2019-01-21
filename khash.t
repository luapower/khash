
--Hashmap type for Terra.
--Written by Cosmin Apreutesei. Public Domain.
--Port of khash.h v0.2.8 from github.com/attractivechaos/klib (MIT License).
--Copyright (c) 2008, 2009, 2011 by Attractive Chaos <attractor@live.co.uk>.

--stdlib deps: realloc, memset, memcmp.

--[[  API

	local M = map{key_t=, [val_t=], hash=key_t.__hash|default,
		equal=val_t.__equal|default, size_t=int, C=require'low'}
	var m = map(key_t=, ...) -- preferred variant
	var m: M = nil   -- =nil is important!
	var m = M(nil)   -- (nil) is important!
	m:free()
	m:clear()
	m:preallocate(size) -> ok?
	m:shrink() -> ok?
	m.count

	m:get_index(k) -> i|-1
	m:put_key(k) -> m.PRESENT|ABSENT|DELETED|-1, i|-1
	m:del_at_index(i) -> found?
	m:has_at_index(i) -> ?
	m:key_at_index(i) -> k (unchecked!)
	m:noderef_key_at_index(i) -> k (unchecked!)
	m:val_at_index(i) -> v (unchecked!)
	m:eof() -> last_i+1
	m:next_index([last_i]) -> i|-1

	m:has(k) -> ?
	m:at(k) -> &v|nil
	m[:get](k, default_v) -> v
	m:put(k, v) -> i|-1
	m:putifnew(k, v) -> i|-1
	m:del(k) -> found?
	for &k,&v in m do ... end

	m:equal(a, b) -> ?
	m:hash(k) -> hash

	m:merge(m)
	m:update(m)

]]

if not ... then require'khash_test'; return end

--ternary operator `?:`.
local iif = macro(function(cond, t, f)
	return quote var v: t:gettype(); if cond then v = t else v = f end in v end
end)

--round up a 32bit number to the next number that is a power of 2.
local roundup32 = macro(function(x)
	return quote
		x = x - 1
		x = x or (x >>  1)
		x = x or (x >>  2)
		x = x or (x >>  4)
		x = x or (x >>  8)
		x = x or (x >> 16)
		x = x + 1
	end
end)

--interface to the 2-bit flags bitmap

local function getflag(flags, i, which)
	return `((flags[i>>4] >> ((i and 0xfU) << 1)) and which) ~= 0
end
local isempty  = macro(function(flags, i) return getflag(flags, i, 2) end)
local isdel    = macro(function(flags, i) return getflag(flags, i, 1) end)
local iseither = macro(function(flags, i) return getflag(flags, i, 3) end)
local function setflag_false(flags, i, which)
	return quote
		flags[i>>4] = flags[i>>4] and not ([uint64](which) << ((i and 0xfU) << 1))
	end
end
local function setflag_true(flags, i, which)
	return quote
		flags[i>>4] = flags[i>>4] or ([uint64](which)) << ((i and 0xfU) << 1)
	end
end
local set_isdel_false   = macro(function(flags, i) return setflag_false(flags, i, 1) end)
local set_isempty_false = macro(function(flags, i) return setflag_false(flags, i, 2) end)
local set_isboth_false  = macro(function(flags, i) return setflag_false(flags, i, 3) end)
local set_isdel_true    = macro(function(flags, i) return setflag_true (flags, i, 1) end)

local fsize = macro(function(m) return `iif(m < 16, 1, m >> 4) end)

--put_key() return codes.
local props = {}
props.PRESENT =  0 --key was already present
props.ABSENT  =  1 --key was added
props.DELETED =  2 --key was previously deleted
props.ERROR   = -1 --allocation error

local UPPER = 0.77

local function map_type(key_t, val_t, hash, equal, deref, deref_key_t, size_t, C)

	local is_map = val_t and true or false
	val_t = val_t or bool --optimized out

	--C dependencies.
	local realloc = C.realloc
	local memset = C.memset
	local free = macro(function(p) return `realloc(p, 0) end)

	hash = hash or (C.hash and macro(function(k)
		return `C.hash(size_t, k, sizeof(deref_key_t))
	end))

	equal = equal or macro(function(k1, k2)
		return `C.memcmp(k1, k2, sizeof(deref_key_t)) == 0
	end)

	--TODO: wrap this into an opaque struct like dynarray!
	local map = struct {
		n_buckets: size_t;
		count: size_t; --number of elements
		n_occupied: size_t;
		upper_bound: size_t;
		flags: &int32;
		keys: &key_t;
		vals: &val_t;
		userdata: &opaque; --to be used by deref
	}

	--publish enums as virtual fields of map
	map.metamethods.__entrymissing = macro(function(k, h)
		return props[k]
	end)

	--ctor & dtor

	function map.metamethods.__cast(from, to, exp)
		if from == niltype or from:isunit() then --nil or {}
			return `map {0, 0, 0, 0, nil, nil, nil}
		end
	end

	terra map.methods.free(h: &map) --can be reused after free
		free(h.keys)
		free(h.flags)
		free(h.vals)
		memset(h, 0, sizeof(map))
	end

	terra map.methods.clear(h: &map)
		if h.flags == nil then return end
		memset(h.flags, 0xaa, fsize(h.n_buckets) * sizeof(int32))
		h.count = 0
		h.n_occupied = 0
	end

	local pair_size = sizeof(key_t) + (is_map and sizeof(val_t) or 0) + 0.25
	terra map:__memsize(): size_t
		return self.n_buckets * pair_size
	end

	--low level (slot-based) API (and the actual algorithm).
	terra map.methods.get_index(h: &map, key: deref_key_t): size_t
		if h.n_buckets == 0 then return -1 end
		var mask: size_t = h.n_buckets - 1
		var k: size_t = hash(&key)
		var i: size_t = k and mask
		var last: size_t = i
		var step: size_t = 0
		while not isempty(h.flags, i) and (isdel(h.flags, i) or not equal(deref(h, h.keys+i), &key)) do
			step = step + 1
			i = (i + step) and mask
			if i == last then return -1 end
		end
		return iif(iseither(h.flags, i), -1, i)
	end

	terra map.methods.resize(h: &map, new_n_buckets: size_t): bool
		-- This function uses 0.25*n_buckets bytes of working space
		-- instead of (sizeof(key_t+val_t)+.25)*n_buckets.
		var new_flags: &int32 = nil
		var j: size_t = 1
		roundup32(new_n_buckets)
		if new_n_buckets < 4 then new_n_buckets = 4 end
		if h.count >= [size_t](new_n_buckets * UPPER + 0.5) then
			j = 0 -- requested size is too small
		else -- hash table size to be changed (shrink or expand); rehash
			new_flags = [&int32](realloc(nil, fsize(new_n_buckets) * sizeof(int32)))
			if new_flags == nil then return false end
			memset(new_flags, 0xaa, fsize(new_n_buckets) * sizeof(int32))
			if h.n_buckets < new_n_buckets then -- expand
				var new_keys = [&key_t](realloc(h.keys, new_n_buckets * sizeof(key_t)))
				if new_keys == nil then free(new_flags); return false end
				if is_map then
					var new_vals = [&val_t](realloc(h.vals, new_n_buckets * sizeof(val_t)))
					if new_vals == nil then free(new_keys); free(new_flags); return false end
					h.vals = new_vals
				end
				h.keys = new_keys
			end -- otherwise shrink
		end
		if j ~= 0 then -- rehashing is needed
			j = 0
			while j ~= h.n_buckets do
				if not iseither(h.flags, j) then
					var key: key_t = h.keys[j]
					var val: val_t
					var new_mask: size_t = new_n_buckets - 1
					if is_map then val = h.vals[j] end
					set_isdel_true(h.flags, j)
					while true do -- kick-out process; sort of like in Cuckoo hashing
						var k: size_t = hash(deref(h, &key))
						var i: size_t = k and new_mask
						var step: size_t = 0
						while not isempty(new_flags, i) do
							step = step + 1
							i = (i + step) and new_mask
						end
						set_isempty_false(new_flags, i)
						if i < h.n_buckets and not iseither(h.flags, i) then -- kick out the existing element
							do var tmp = h.keys[i]; h.keys[i] = key; key = tmp; end
							if is_map then var tmp = h.vals[i]; h.vals[i] = val; val = tmp; end
							set_isdel_true(h.flags, i) -- mark it as deleted in the old hash table
						else  -- write the element and jump out of the loop
							h.keys[i] = key
							if is_map then h.vals[i] = val end
							break
						end
					end
				end
				j = j + 1
			end
			if h.n_buckets > new_n_buckets then -- shrink the hash table
				var new_keys = [&key_t](realloc(h.keys, new_n_buckets * sizeof(key_t)))
				if new_keys == nil then free(new_flags); return false end
				if is_map then
					var new_vals = [&val_t](realloc(h.vals, new_n_buckets * sizeof(val_t)))
					if new_vals == nil then free(new_keys); free(new_flags); return false end
					h.vals = new_vals
				end
				h.keys = new_keys
			end
			free(h.flags) -- free the working space
			h.flags = new_flags
			h.n_buckets = new_n_buckets
			h.n_occupied = h.count
			h.upper_bound = h.n_buckets * UPPER + 0.5
		end
		return true
	end

	terra map.methods.preallocate(h: &map, n_buckets: size_t)
		return iif(n_buckets > h.n_buckets, h:resize(n_buckets), true)
	end

	terra map.methods.shrink(h: &map)
		return h:resize(h.count)
	end

	terra map.methods.put_key(h: &map, key: key_t): {int8, size_t}
		if h.n_occupied >= h.upper_bound then -- update the hash table
			if h.n_buckets > (h.count<<1) then
				if not h:resize(h.n_buckets - 1) then -- clear "deleted" elements
					return -1, -1
				end
			elseif not h:resize(h.n_buckets + 1) then -- expand the hash table
				return -1, -1
			end
		end -- TODO: implement automatic shrinking; resize() already supports shrinking
		var x: size_t
		do
			x = h.n_buckets
			var site: size_t = x
			var mask: size_t = x - 1
			var k: size_t = hash(deref(h, &key))
			var i: size_t = k and mask
			var step: size_t = 0
			var last: size_t
			if isempty(h.flags, i) then
				x = i -- for speed up
			else
				last = i
				while not isempty(h.flags, i)
					and (isdel(h.flags, i)
						or not equal(deref(h, h.keys+i), deref(h, &key)))
				do
					if isdel(h.flags, i) then site = i end
					step = step + 1
					i = (i + step) and mask
					if i == last then x = site; break; end
				end
				if x == h.n_buckets then
					x = iif(isempty(h.flags, i) and site ~= h.n_buckets, site, i)
				end
			end
		end
		if isempty(h.flags, x) then -- not present at all
			h.keys[x] = key
			set_isboth_false(h.flags, x)
			h.count = h.count + 1; h.n_occupied = h.n_occupied + 1
			return h.ABSENT, x
		elseif isdel(h.flags, x) then -- deleted
			h.keys[x] = key
			set_isboth_false(h.flags, x)
			h.count = h.count + 1
			return h.DELETED, x
		else -- present and not deleted
			return h.PRESENT, x
		end
	end

	terra map.methods.del_at_index(h: &map, i: size_t)
		if i ~= h.n_buckets and not iseither(h.flags, i) then
			set_isdel_true(h.flags, i)
			h.count = h.count - 1
			return true
		end
		return false
	end

	map.methods.eof           = macro(function(h) return `h.n_buckets end)
	map.methods.has_at_index  = macro(function(h, i)
		return `i >= 0 and i < h.n_buckets and not iseither(h.flags, i)
	end)
	map.methods.key_at_index  = macro(function(h, i) return `@deref(h, h.keys+i) end)
	map.methods.val_at_index  = macro(function(h, i) return `h.vals[i] end)
	map.methods.noderef_key_at_index = macro(function(h, i) return `h.keys[i] end)

	--returns -1 on eof, which is also the start index which can be omitted.
	map.methods.next_index = macro(function(h, i)
		i = i or -1
		return quote
			var r: size_t = -1
			var i: size_t = i
			while i < h:eof() do
				i = i + 1
				if h:has_at_index(i) then
					r = i; break
				end
			end
			in r
		end
	end)

	--hi-level (key/value pair-based) API

	terra map.methods.has(h: &map, key: deref_key_t)
		var i = h:get_index(key)
		return i ~= -1
	end

	terra map.methods.at(h: &map, key: deref_key_t): &val_t
		if not is_map then return nil end
		var i = h:get_index(key)
		if i == -1 then return nil end
		return &h.vals[i]
	end

	terra map.methods.get(h: &map, key: deref_key_t, default: val_t): val_t
		if not is_map then return default end
		var i = h:get_index(key)
		if i == -1 then return default end
		return h.vals[i]
	end
	map.metamethods.__apply = map.methods.get

	terra map.methods.put(h: &map, key: key_t, val: val_t)
		var ret, i = h:put_key(key)
		if i == -1 then return -1 end
		if is_map then h.vals[i] = val end
		return i
	end

	terra map.methods.putifnew(h: &map, key: key_t, val: val_t)
		var ret, i = h:put_key(key)
		if i == -1 or ret == h.PRESENT then return -1 end
		if is_map then h.vals[i] = val end
		return i
	end

	terra map.methods.del(h: &map, key: deref_key_t): bool
		var i = h:get_index(key)
		if i == -1 then return false end
		h:del_at_index(i)
		return true
	end

	function map.metamethods.__for(h, body)
		return quote
			for i = 0, h:eof() do
				if h:has_at_index(i) then
					[ body(`&h.keys[i], `iif(is_map, &h.vals[i], nil)) ]
				end
			end
		end
	end

	terra map:equal(k1: &deref_key_t, k2: &deref_key_t)
		return equal(k1, k2)
	end

	terra map:merge(m: &map) for k,v in m do self:putifnew(@k,@v) end end
	terra map:update(m: &map) for k,v in m do self:put(@k,@v) end end

	return map
end
map_type = terralib.memoize(map_type)

--specialization for different key and value types ---------------------------

local keytype = {}

local direct_cmp = macro(function(a, b) return `@a == @b end)
local identity_hash = macro(function(n) return `@n end)

keytype[int32] = {
	hash32 = identity_hash,
	hash64 = identity_hash,
	equal = direct_cmp,
}
keytype[uint32] = keytype[int32]

local K = 2654435769ULL --Knuth's
keytype[int64] = {
	hash32 = macro(function(n)
		return `([int32](@n) * K + [int32](@n >> 32) * K) >> 31
	end),
	hash64 = identity_hash,
	equal = direct_cmp,
}
keytype[uint64] = keytype[int64]

local function hash_and_equal(hash, equal, key_t, size_t)
	local key_tt = keytype[key_t]
	local hashname = sizeof(size_t) == 8 and 'hash64' or 'hash32'

	hash = hash
		or key_t:isstruct() and key_t.methods['__'..hashname]
		or key_tt and key_tt[hashname]

	equal = equal
		or key_t:isstruct() and key_t.methods.__equal
		or key_tt and key_tt.equal

	return hash, equal
end

local deref_pointer = macro(function(self, k) return `@k end)
local pass_through = macro(function(self, k) return k end)

local map_type = function(key_t, val_t, hash, equal, deref, deref_key_t, size_t, C)
	if terralib.type(key_t) == 'table' then
		local t = key_t
		key_t, val_t, hash, equal, deref, deref_key_t, size_t, C =
			t.key_t, t.val_t, t.hash, t.equal, t.deref, t.deref_key_t, t.size_t, C
	end
	assert(key_t)
	deref_key_t = deref_key_t or (key_t:ispointer() and key_t.type) or key_t
	deref = deref or (key_t:ispointer() and deref_pointer) or pass_through
	size_t = size_t or int --it's faster to use 64bit hashes for 64bit keys
	C = C or require'low'
	hash, equal = hash_and_equal(hash, equal, deref_key_t, size_t)
	return map_type(key_t, val_t, hash, equal, deref, deref_key_t, size_t, C)
end

return macro(
	--calling it from Terra returns a new map.
	function(key_t, val_t, hash, equal, deref, deref_key_t, size_t)
		key_t = key_t and key_t:astype()
		val_t = val_t and val_t:astype()
		size_t = size_t and size_t:astype()
		local map = map_type(key_t, val_t, hash, equal, deref, deref_key_t, size_t)
		return `map(nil)
	end,
	--calling it from Lua or from an escape or in a type declaration returns
	--just the type, and you can also pass a custom C namespace.
	map_type
)

