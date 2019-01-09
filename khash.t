
--Port of khash.h v0.2.8 to Terra.
--Written by Cosmin Apreutesei. Public Domain.
--C code from github.com/attractivechaos/klib (MIT License).
--Copyright (c) 2008, 2009, 2011 by Attractive Chaos <attractor@live.co.uk>.

if not ... then require'khash_test'; return end

local khash = {}
setmetatable(khash, khash)

khash.type = {} --support for different key and value types

--lazy load the C namespace to allow the user to provide its own C stdlib
--functions and also to not depend on the 'low' module from luapower.
--Usage: set khash.C = {malloc = ..., ...} after loading the module.
--Alternatively, a C module can be passed directly to map().
function khash:__index(k)
	if k == 'C' then
		local low = require'low'
		low.include'stdlib.h'
		low.include'string.h'
		self.C = low.C
		return low.C
	end
end

local iif = macro(function(cond, t, f) --ternary operator `?:` from 'low' module
	return quote var v: t:gettype(); if cond then v = t else v = f end in v end
end)

local khint = int32
local HASH_UPPER = 0.77

local eq = macro(function(a, b) return `a == b end)

khash.type[int32] = {
	hash = macro(function(key) return key end),
	equal = eq,
}

khash.type[int64] = {
	hash = macro(function(key) return `[int32](key >> 33 ^ key ^ key << 11) end),
	equal = eq,
}
khash.type[&opaque] = khash.type[int64]

local cs = {type = &int8}
cs.hash = terra(s: &int8): khint --X31 hash
	var h: khint = @s
	if h ~= 0 then
		s = s + 1
		while @s ~= 0 do
			h = (h << 5) - h + [khint](s[0])
			s = s + 1
		end
	end
	return h
end
cs.equal = macro(function(a, b, C)
	return `C.strcmp(a, b) == 0
end)
khash.type.cstring = cs

--hashmap implementation

local kroundup32 = macro(function(x)
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

local isempty           = macro(function(flag, i) return `((flag[i>>4] >> ((i and 0xfU) << 1)) and 2) ~= 0 end)
local isdel             = macro(function(flag, i) return `((flag[i>>4] >> ((i and 0xfU) << 1)) and 1) ~= 0 end)
local iseither          = macro(function(flag, i) return `((flag[i>>4] >> ((i and 0xfU) << 1)) and 3) ~= 0 end)
local set_isdel_false   = macro(function(flag, i) return quote flag[i>>4] = flag[i>>4] and not (1UL << ((i and 0xfU) << 1)) end end)
local set_isempty_false = macro(function(flag, i) return quote flag[i>>4] = flag[i>>4] and not (2UL << ((i and 0xfU) << 1)) end end)
local set_isboth_false  = macro(function(flag, i) return quote flag[i>>4] = flag[i>>4] and not (3UL << ((i and 0xfU) << 1)) end end)
local set_isdel_true    = macro(function(flag, i) return quote flag[i>>4] = flag[i>>4] or 1UL << ((i and 0xfU) << 1) end end)
local fsize             = macro(function(m) return `iif(m < 16, 1, m >> 4) end)

--put_key return codes.
khash.PRESENT =  0 --key was already present
khash.ABSENT  =  1 --key was added
khash.DELETED =  2 --key was previously deleted
khash.ERROR   = -1 --allocation error

local function map(is_map, key_t, val_t, hash, equal, C)

	--C dependencies
	local malloc = C.malloc
	local realloc = C.realloc
	local free = C.free
	local memset = C.memset

	local map = struct {
		n_buckets: khint;
		count: khint; --number of elements
		n_occupied: khint;
		upper_bound: khint;
		flags: &int32;
		keys: &key_t;
		vals: &val_t;
	}

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

	--low level (slot-based) API

	terra map.methods.get_index(h: &map, key: key_t): khint
		if h.n_buckets == 0 then return -1 end
		var mask: khint = h.n_buckets - 1
		var k: khint = hash(key)
		var i: khint = k and mask
		var last: khint = i
		var step: khint = 0
		while not isempty(h.flags, i) and (isdel(h.flags, i) or not equal(h.keys[i], key, C)) do
			step = step + 1
			i = (i + step) and mask
			if i == last then return -1 end
		end
		return iif(iseither(h.flags, i), -1, i)
	end

	terra map.methods.resize(h: &map, new_n_buckets: khint): bool
		-- This function uses 0.25*n_buckets bytes of working space instead of (sizeof(key_t+val_t)+.25)*n_buckets.
		var new_flags: &int32 = nil
		var j: khint = 1
		kroundup32(new_n_buckets)
		if new_n_buckets < 4 then new_n_buckets = 4 end
		if h.count >= [khint](new_n_buckets * HASH_UPPER + 0.5) then
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
					var new_mask: khint = new_n_buckets - 1
					if is_map then val = h.vals[j] end
					set_isdel_true(h.flags, j)
					while true do -- kick-out process; sort of like in Cuckoo hashing
						var k: khint = hash(key)
						var i: khint = k and new_mask
						var step: khint = 0
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

	terra map.methods.put_key(h: &map, key: key_t): {int8, khint}
		if h.n_occupied >= h.upper_bound then -- update the hash table
			if h.n_buckets > (h.count<<1) then
				if not h:resize(h.n_buckets - 1) then -- clear "deleted" elements
					return khash.ERROR, -1
				end
			elseif not h:resize(h.n_buckets + 1) then -- expand the hash table
				return khash.ERROR, -1
			end
		end -- TODO: implement automatic shrinking; resize() already supports shrinking
		var x: khint
		do
			x = h.n_buckets
			var site: khint = x
			var mask: khint = x - 1
			var k: khint = hash(key)
			var i: khint = k and mask
			var step: khint = 0
			var last: khint
			if isempty(h.flags, i) then
				x = i -- for speed up
			else
				last = i
				while not isempty(h.flags, i) and (isdel(h.flags, i) or not equal(h.keys[i], key, C)) do
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

	terra map.methods.del_at(h: &map, i: khint)
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

	return map
end
local map = terralib.memoize(map)

local map = function(is_map, key_t, val_t, hash, equal, C)
	local key_tt = khash.type[key_t]
	local val_tt = khash.type[val_t]
	key_t = key_tt and key_tt.type or key_t
	val_t = val_tt and val_tt.type or val_t
	hash = hash or key_tt and key_tt.hash
	equal = equal or key_tt and key_tt.equal
	C = C or khash.C
	return map(is_map, key_t, val_t, hash, equal, C)
end

function khash.map(...) return map(true, ...) end
function khash.set(...) return map(false, ...) end

return khash
