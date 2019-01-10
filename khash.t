
--Port of khash.h v0.2.8 to Terra.
--Written by Cosmin Apreutesei. Public Domain.
--C code from github.com/attractivechaos/klib (MIT License).
--Copyright (c) 2008, 2009, 2011 by Attractive Chaos <attractor@live.co.uk>.

if not ... then require'khash_test'; return end

local khash = {}
setmetatable(khash, khash)

--Lazy load the C namespace to allow the user to provide its own C functions.
--Usage: set `khash.C = {malloc = ..., ...}` after loading the module.
--Alternatively, a C module can be passed directly to map().
function khash:__index(k)
	if k == 'C' then
		self.C = {}
		local stdlib = terralib.includec'stdlib.h'
		local string = terralib.includec'string.h'
		self.C.malloc = stdlib.malloc
		self.C.realloc = stdlib.realloc
		self.C.free = stdlib.free
		self.C.memset = string.memset
		return self.C
	end
end

--ternary operator `?:` from 'low' module.
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

--flag bitmap interface

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
khash.PRESENT =  0 --key was already present
khash.ABSENT  =  1 --key was added
khash.DELETED =  2 --key was previously deleted
khash.ERROR   = -1 --allocation error

local function map(is_map, key_t, val_t, hash, equal, C, size_t, HASH_UPPER)

	size_t = size_t or int32
	HASH_UPPER = HASH_UPPER or 0.77

	--C dependencies.
	local malloc = C.malloc
	local realloc = C.realloc
	local free = C.free
	local memset = C.memset

	local map = struct {
		n_buckets: size_t;
		count: size_t; --number of elements
		n_occupied: size_t;
		upper_bound: size_t;
		flags: &int32;
		keys: &key_t;
		vals: &val_t;
	}

	--prepare the environment to pass to the hash() and equal() macros.
	local env = {size_t = size_t, C = C, map = map}

	--ctor & dtor

	function map.metamethods.__cast(from, to, exp)
		if from == (`{}):gettype() then --initalize with empty tuple
			return `map {0, 0, 0, 0, nil, nil, nil}
		end
	end

	terra map.methods.init(h: &map)
		memset(h, 0, sizeof(map))
	end

	terra map.methods.free(h: &map)
		free(h.keys)
		free(h.flags)
		free(h.vals)
		h:init()
	end

	terra map.methods.clear(h: &map)
		if h.flags == nil then return end
		memset(h.flags, 0xaa, fsize(h.n_buckets) * sizeof(int32))
		h.count = 0
		h.n_occupied = 0
	end

	--low level (slot-based) API (and the actual algorithm).

	terra map.methods.get_index(h: &map, key: key_t): size_t
		if h.n_buckets == 0 then return -1 end
		var mask: size_t = h.n_buckets - 1
		var k: size_t = hash(key, env)
		var i: size_t = k and mask
		var last: size_t = i
		var step: size_t = 0
		while not isempty(h.flags, i) and (isdel(h.flags, i) or not equal(h.keys[i], key, env)) do
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
		if h.count >= [size_t](new_n_buckets * HASH_UPPER + 0.5) then
			j = 0 -- requested size is too small
		else -- hash table size to be changed (shrink or expand); rehash
			new_flags = [&int32](malloc(fsize(new_n_buckets) * sizeof(int32)))
			if new_flags == nil then return false end
			memset(new_flags, 0xaa, fsize(new_n_buckets) * sizeof(int32))
			if h.n_buckets < new_n_buckets then -- expand
				var new_keys: &key_t = [&key_t](realloc(h.keys, new_n_buckets * sizeof(key_t)))
				if new_keys == nil then free(new_flags); return false end
				if is_map then
					var new_vals: &val_t = [&val_t](realloc(h.vals, new_n_buckets * sizeof(val_t)))
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
						var k: size_t = hash(key, env)
						var i: size_t = k and new_mask
						var step: size_t = 0
						while not isempty(new_flags, i) do
							step = step + 1
							i = (i + step) and new_mask
						end
						set_isempty_false(new_flags, i)
						if i < h.n_buckets and not iseither(h.flags, i) then -- kick out the existing element
							do var tmp: key_t = h.keys[i]; h.keys[i] = key; key = tmp; end
							if is_map then var tmp: val_t = h.vals[i]; h.vals[i] = val; val = tmp; end
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
			h.upper_bound = h.n_buckets * HASH_UPPER + 0.5
		end
		return true
	end

	terra map.methods.put_key(h: &map, key: key_t): {int8, size_t}
		if h.n_occupied >= h.upper_bound then -- update the hash table
			if h.n_buckets > (h.count<<1) then
				if not h:resize(h.n_buckets - 1) then -- clear "deleted" elements
					return khash.ERROR, -1
				end
			elseif not h:resize(h.n_buckets + 1) then -- expand the hash table
				return khash.ERROR, -1
			end
		end -- TODO: implement automatic shrinking; resize() already supports shrinking
		var x: size_t
		do
			x = h.n_buckets
			var site: size_t = x
			var mask: size_t = x - 1
			var k: size_t = hash(key, env)
			var i: size_t = k and mask
			var step: size_t = 0
			var last: size_t
			if isempty(h.flags, i) then
				x = i -- for speed up
			else
				last = i
				while not isempty(h.flags, i) and (isdel(h.flags, i) or not equal(h.keys[i], key, env)) do
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
			return khash.ABSENT, x
		elseif isdel(h.flags, x) then -- deleted
			h.keys[x] = key
			set_isboth_false(h.flags, x)
			h.count = h.count + 1
			return khash.DELETED, x
		else -- present and not deleted
			return khash.PRESENT, x
		end
	end

	terra map.methods.del_at(h: &map, i: size_t)
		if i ~= h.n_buckets and not iseither(h.flags, i) then
			set_isdel_true(h.flags, i)
			h.count = h.count - 1
		end
	end

	map.methods.has_at  = macro(function(h, i) return `not iseither(h.flags, i) end)
	map.methods.key_at  = macro(function(h, i) return `h.keys[i] end)
	map.methods.val_at  = macro(function(h, i) return `h.vals[i] end)
	map.methods.eof     = macro(function(h) return `h.n_buckets end)

	map.methods.next_index = macro(function(h, i)
		return quote
			if i < 0 then return -1 end
			while i < h:eof() do
				i = i + 1
				if h:has_at(i) then
					return i
				end
			end
			i = -1
		in
			i
		end
	end)

	--hi-level (key/value pair-based) API

	terra map.methods.put(h: &map, key: key_t, val: val_t)
		var ret, i = h:put_key(key)
		if i >= 0 then
			h.vals[i] = val
		end
		return i
	end

	terra map.methods.has(h: &map, key: key_t)
		var i = h:get_index(key)
		return i >= 0
	end

	local invalid_val = not val_t:ispointer() and 0 or `nil
	terra map.methods.get(h: &map, key: key_t): {bool, val_t}
		var i = h:get_index(key)
		if i < 0 then return false, invalid_val end
		return true, h.vals[i]
	end

	terra map.methods.del(h: &map, key: key_t): bool
		var i = h:get_index(key)
		if i < 0 then return false end
		h:del_at(i)
		return true
	end

	function map.metamethods.__for(h, body)
		return quote
			for i = 0, h:eof() do
				if h:has_at(i) then
					[ body(`h.keys[i], `h.vals[i]) ]
				end
			end
		end
	end

	map.methods.equal = macro(function(h, a, b) return `equal(a, b, env) end)
	map.methods.hash = macro(function(h, e) return `hash(e, env) end)

	return map
end
local map = terralib.memoize(map)

--specialization for different key and value types ---------------------------

khash.type = {}

--default (slow) hash and comparison macros for aggregate types.
khash.type.default = {
	hash = macro(function(e, env) --X31 hash
		local size_t = env:asvalue().size_t
		return quote
			var h: size_t = 0
			for i = 0, sizeof([e:gettype()]) do
				h = (h << 5) - h + [size_t](([&int8](&e))[i])
			end
			in h
		end
	end),
	equal = macro(function(a, b, env)
		return `env.C.memcmp(&a, &b, sizeof([a:gettype()])) == 0
	end),
}

local direct_cmp = macro(function(a, b) return `a == b end)

khash.type[int32] = {
	hash = macro(function(n) return n end),
	equal = direct_cmp,
}

local K = 2654435769ULL --Knuth's
khash.type[int64] = {
	hash = macro(function(n)
		--return `[uint64](n) * 11400714819323198485LLU --Fibonnacci
		return `([int32](n) * K + [int32](n >> 32) * K) >> 31 --Knuth
	end),
	equal = direct_cmp,
}

--null-terminated strings
khash.type.cstring = {
	type = &int8,
	hash = macro(function(s) --X31 hash
		return quote
			var h: size_t = @s
			if h ~= 0 then
				s = s + 1
				while @s ~= 0 do
					h = (h << 5) - h + [size_t](@s)
					s = s + 1
				end
			end
			in h
		end
	end),
	equal = macro(function(a, b, h)
		return `h.C.strcmp(a, b) == 0
	end),
}

local map = function(is_map, key_t, val_t, hash, equal, C, size_t, HASH_UPPER)
	local key_tt = khash.type[key_t]
	local val_tt = khash.type[val_t]
	key_t = key_tt and key_tt.type or key_t
	val_t = val_tt and val_tt.type or val_t
	hash = hash or key_tt and key_tt.hash
		or key_t:ispointer() and khash.type[int64].hash
		or khash.type.default.hash
	equal = equal or key_tt and key_tt.equal
		or key_t:ispointer() and hash.type[int64].equal
		or khash.type.default.equal

	--if hash() and/or equal() are terra functions, wrap them into macros
	--so that we can discard the surplus `env` argument before calling them.
	if terralib.type(hash) == 'terrafunction' then
		local userhash = hash
		hash = macro(function(k, env) return `userhash(k) end)
	end
	if terralib.type(equal) == 'terrafunction' then
		local userequal = equal
		equal = macro(function(k, env) return `userequal(k) end)
	end

	C = C or khash.C
	return map(is_map, key_t, val_t, hash, equal, C, size_t, HASH_UPPER)
end

function khash.map(...) return map(true, ...) end
function khash.set(...) return map(false, ...) end

return khash
