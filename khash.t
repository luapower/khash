--[[

	Hashmap type for Terra.
	Written by Cosmin Apreutesei. Public Domain.

	Port of khash.h v0.2.8 from github.com/attractivechaos/klib (MIT License).

	local M = map{key_t=,[val_t=],...}
	local M = map(key_t,...)
	var m   = map{key_t=,...}
	var m   = map(key_t,...)
	var m = M(nil)

	m:init()                                    initialize (for struct members)
	m:free()                                    free the hashmap
	m:clear()                                   clear but keep the buffers

	m.count                                     (read/only) number of pairs
	m.capacity                                  (read/write) grow/shrink hashmap
	m.min_capacity                              (write/only) grow hashmap

	m:index(k[,default]) -> i                   lookup key and return pair index
	m:setkey(k) -> m.PRESENT|ABSENT|DELETED|-1, i|-1   occupy a key
	m:del_at_index(i) -> found?                 remove pair
	m:has_at_index(i) -> found?                 check if index is occupied
	m:key_at_index(i) -> k                      (unchecked!) get key at i
	m:noderef_key_at_index(i) -> k              (unchecked!) get no-deref key at i
	m:val_at_index(i) -> v                      (unchecked!) get value at i
	m:next_index([last_i]) -> i|-1              next occupied index

	m:has(k) -> found?                          check if key is in map
	m:at(k[,default]) -> &v                     &value for key
	m[:get](k[,default]) -> v                   value for key
	m:set(k,v) -> i                             add or update pair
	m:set(k) -> &v                              add or update key and get &value
	m:add(k,v) -> i|-1                          add new pair
	m:add(k) -> &v|nil                          add new pair and get &value
	m:del(k) -> found?                          remove pair
	for &k,&v in m do ... end                   iterate pairs

	m:merge(m)                                  add new pairs from another map
	m:update(m)                                 update pairs, overriding values

]]

if not ... then require'khash_test'; return end

setfenv(1, require'low')

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

local UPPER = 0.77

local function map_type(key_t, val_t, user_hash, user_equal, deref, deref_key_t, size_t)

	local is_map = val_t and true or false
	val_t = val_t or bool --optimized out

	local hash = user_hash or hash
	local equal = user_equal or equal

	local struct map (gettersandsetters) {
		n_buckets: size_t;
		count: size_t; --number of pairs
		n_occupied: size_t;
		upper_bound: size_t;
		flags: &int32;
		keys: &key_t;
		vals: &val_t;
		userdata: &opaque; --to be used by deref
	}

	map.empty = `map{
		n_buckets = 0; count = 0; n_occupied = 0; upper_bound = 0;
		flags = nil; keys = nil; vals = nil; userdata = nil;
	}

	function map.metamethods.__typename(self)
		return 'map('..tostring(key_t)..'->'..tostring(val_t)..')'
	end

	function map.metamethods.__cast(from, to, exp)
		if to == map then
			if from == niltype then --makes [map(...)](nil) work in a constant()
				return map.empty
			end
		end
		assert(false, 'invalid cast from ', from, ' to ', to, ': ', exp)
	end

	--publish enums as virtual fields of map
	addproperties(map)
	map.properties.PRESENT =  0 --key was already present
	map.properties.ABSENT  =  1 --key was added
	map.properties.DELETED =  2 --key was previously deleted
	map.properties.ERROR   = -1 --allocation error

	map.metamethods.__apply = macro(function(self, i, default)
		if default then return `self:get(i, default) else return `self:get(i) end
	end)

	addmethods(map, function()

		--ctor & dtor

		terra map.methods.init(h: &map)
			@h = [map.empty]
		end

		terra map.methods.free(h: &map) --can be reused after free
			realloc(h.keys , 0)
			realloc(h.flags, 0)
			realloc(h.vals , 0)
			fill(h)
		end

		terra map.methods.clear(h: &map)
			if h.flags == nil then return end
			fill(h.flags, 0xaa, fsize(h.n_buckets))
			h.count = 0
			h.n_occupied = 0
		end

		local pair_size = sizeof(key_t) + (is_map and sizeof(val_t) or 0)
		terra map:__memsize(): size_t
			return self.count * pair_size
		end

		--low level (slot-based) API (and the actual algorithm).
		map.methods.index = overload'index'

		map.methods.index:adddefinition(terra(h: &map, key: deref_key_t, default: size_t): size_t
			if h.n_buckets == 0 then return default end
			var mask: size_t = h.n_buckets - 1
			var k: size_t = hash(size_t, &key)
			var i: size_t = k and mask
			var last: size_t = i
			var step: size_t = 0
			while not isempty(h.flags, i) and (isdel(h.flags, i) or not equal(deref(h, h.keys+i), &key)) do
				step = step + 1
				i = (i + step) and mask
				if i == last then return default end
			end
			return iif(iseither(h.flags, i), default, i)
		end)
		map.methods.index:adddefinition(terra(h: &map, key: deref_key_t): size_t
			var i = h:index(key, -1)
			assert(i ~= -1)
			return i
		end)

		terra map.methods.resize(h: &map, new_n_buckets: size_t): bool
			-- This function uses 0.25*n_buckets bytes of working space
			-- instead of (sizeof(key_t+val_t)+.25)*n_buckets.
			var new_flags: &int32 = nil
			var j: size_t = 1
			new_n_buckets = max(4, nextpow2(new_n_buckets))
			if h.count >= [size_t](new_n_buckets * UPPER + 0.5) then
				j = 0 -- requested size is too small
			else -- hash table size to be changed (shrink or expand); rehash
				new_flags = alloc(int32, fsize(new_n_buckets))
				if new_flags == nil then
					return false
				end
				fill(new_flags, 0xaa, fsize(new_n_buckets))
				if h.n_buckets < new_n_buckets then -- expand
					var new_keys = realloc(h.keys, new_n_buckets)
					if new_keys == nil then
						realloc(new_flags, 0)
						return false
					end
					if is_map then
						var new_vals = realloc(h.vals, new_n_buckets)
						if new_vals == nil then
							realloc(new_keys, 0)
							realloc(new_flags, 0)
							return false
						end
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
							var k: size_t = hash(size_t, deref(h, &key))
							var i: size_t = k and new_mask
							var step: size_t = 0
							while not isempty(new_flags, i) do
								step = step + 1
								i = (i + step) and new_mask
							end
							set_isempty_false(new_flags, i)
							if i < h.n_buckets and not iseither(h.flags, i) then -- kick out the existing element
								swap(h.keys[i], key)
								if is_map then swap(h.vals[i], val) end
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
					var new_keys = realloc(h.keys, new_n_buckets)
					if new_keys == nil then
						realloc(new_flags, 0)
						return false
					end
					if is_map then
						var new_vals = realloc(h.vals, new_n_buckets)
						if new_vals == nil then
							realloc(new_keys, 0)
							realloc(new_flags, 0)
							return false
						end
						h.vals = new_vals
					end
					h.keys = new_keys
				end
				realloc(h.flags, 0) -- free the working space
				h.flags = new_flags
				h.n_buckets = new_n_buckets
				h.n_occupied = h.count
				h.upper_bound = h.n_buckets * UPPER + 0.5
			end
			return true
		end

		map.methods.get_capacity = macro(function(self) return `self.n_buckets end)

		terra map.methods.set_capacity(h: &map, n_buckets: size_t)
			return h:resize(max(h.count, n_buckets))
		end

		terra map.methods.set_min_capacity(h: &map, n_buckets: size_t)
			assert(h:resize(max(h.n_buckets, n_buckets)))
		end

		terra map.methods.setkey(h: &map, key: key_t): {int8, size_t}
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
				var k: size_t = hash(size_t, deref(h, &key))
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

		map.methods.has_at_index = macro(function(h, i)
			return `i >= 0 and i < h.n_buckets and not iseither(h.flags, i)
		end)
		map.methods.key_at_index = macro(function(h, i) return `@deref(h, h.keys+i) end)
		map.methods.val_at_index = macro(function(h, i) return `h.vals[i] end)
		map.methods.noderef_key_at_index = macro(function(h, i) return `h.keys[i] end)

		--returns -1 on eof, which is also the start index which can be omitted.
		map.methods.next_index = macro(function(h, i)
			i = i or -1
			return quote
				var r: size_t = -1
				var i: size_t = i
				while i < h.n_buckets do
					i = i + 1
					if h:has_at_index(i) then
						r = i
						break
					end
				end
				in r
			end
		end)

		--hi-level (key/value pair-based) API

		terra map.methods.has(h: &map, key: deref_key_t)
			return h:index(key, -1) ~= -1
		end

		if is_map then
			map.methods.at = overload'at'
			map.methods.at:adddefinition(terra(h: &map, key: deref_key_t, default: &val_t): &val_t
				var i = h:index(key, -1)
				return iif(i ~= -1, &h.vals[i], default)
			end)
			map.methods.at:adddefinition(terra(h: &map, key: deref_key_t): &val_t
				return &h.vals[h:index(key)]
			end)

			map.methods.get = overload'get'
			map.methods.get:adddefinition(terra(h: &map, key: deref_key_t, default: val_t): val_t
				var i = h:index(key, -1)
				return iif(i ~= -1, h.vals[i], default)
			end)
			map.methods.get:adddefinition(terra(h: &map, key: deref_key_t): val_t
				return h.vals[h:index(key)]
			end)

			map.methods.set = overload'set'
			map.methods.set:adddefinition(terra(h: &map, key: key_t, val: val_t)
				var ret, i = h:setkey(key); assert(i ~= -1)
				h.vals[i] = val
				return i
			end)
			map.methods.set:adddefinition(terra(h: &map, key: key_t)
				var ret, i = h:setkey(key); assert(i ~= -1)
				return &h.vals[i]
			end)

			map.methods.add = overload'add'
			map.methods.add:adddefinition(terra(h: &map, key: key_t, val: val_t)
				var ret, i = h:setkey(key); assert(i ~= -1)
				if ret ~= h.PRESENT then
					h.vals[i] = val
					return -1
				end
				return i
			end)
			map.methods.add:adddefinition(terra(h: &map, key: key_t)
				var ret, i = h:setkey(key); assert(i ~= -1)
				return iif(ret ~= h.PRESENT, &h.vals[i], nil)
			end)
		else
			terra map.methods.set(h: &map, key: key_t)
				var _, i = h:setkey(key); assert(i ~= -1)
				return i
			end

			terra map.methods.add(h: &map, key: key_t)
				var ret, i = h:setkey(key); assert(i ~= -1)
				return iif(ret ~= h.PRESENT, i, -1)
			end
		end

		terra map.methods.del(h: &map, key: deref_key_t): bool
			var i = h:index(key, -1)
			if i == -1 then return false end
			h:del_at_index(i)
			return true
		end

		function map.metamethods.__for(h, body)
			if is_map then
				return quote
					for i = 0, h.n_buckets do
						if h:has_at_index(i) then
							[ body(`&h.keys[i], `&h.vals[i]) ]
						end
					end
				end
			else
				return quote
					for i = 0, h.n_buckets do
						if h:has_at_index(i) then
							[ body(`&h.keys[i]) ]
						end
					end
				end
			end
		end

		if is_map then
			terra map:merge(m: &map) for k,v in m do self:add(@k,@v) end end
			terra map:update(m: &map) for k,v in m do self:set(@k,@v) end end
		else
			terra map:merge(m: &map) for k in m do self:add(@k) end end
			terra map:update(m: &map) for k in m do self:set(@k) end end
		end

	end) --addmethods()

	return map
end
map_type = memoize(map_type)

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

local deref_pointer = macro(function(self, k) return `@k end)
local pass_through = macro(function(self, k) return k end)

local map_type = function(key_t, val_t, hash, equal, deref, deref_key_t, size_t)
	if terralib.type(key_t) == 'table' then
		local t = key_t
		key_t, val_t, hash, equal, deref, deref_key_t, size_t =
			t.key_t, t.val_t, t.hash, t.equal, t.deref, t.deref_key_t, t.size_t
	end
	assert(key_t, 'key type missing')
	deref_key_t = deref_key_t or (key_t:ispointer() and key_t.type) or key_t
	deref = deref or (key_t:ispointer() and deref_pointer) or pass_through
	size_t = size_t or int --it's faster to use 64bit hashes for 64bit keys
	return map_type(key_t, val_t, hash, equal, deref, deref_key_t, size_t)
end

return macro(
	--calling it from Terra returns a new map.
	function(key_t, val_t, hash, equal, deref, deref_key_t, size_t)
		key_t = key_t and key_t:astype()
		val_t = val_t and val_t:astype()
		size_t = size_t and size_t:astype()
		local map = map_type(key_t, val_t, hash, equal, deref, deref_key_t, size_t)
		return quote var m: map; m:init() in m end
	end,
	--calling it from Lua or from an escape or in a type declaration returns
	--just the type, and you can also pass a custom C namespace.
	map_type
)
