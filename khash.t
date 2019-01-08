
--Port of khash.h v0.2.8 to Terra.
--Written by Cosmin Apreutesei. Public Domain.
--C code from github.com/attractivechaos/klib (MIT License).
--Copyright (c) 2008, 2009, 2011 by Attractive Chaos <attractor@live.co.uk>.

--[[

KHASH_MAP_INIT_INT(32, char)
int main() {
	int ret, is_missing;
	khiter_t k;
	khash_t(32) *h = kh_init(32);
	k = kh_put(32, h, 5, &ret);
	kh_value(h, k) = 10;
	k = kh_get(32, h, 10);
	is_missing = (k == kh_end(h));
	k = kh_get(32, h, 5);
	kh_del(32, h, k);
	for (k = kh_begin(h); k != kh_end(h); ++k)
		if (kh_exist(h, k)) kh_value(h, k) = 1;
	kh_destroy(32, h);
	return 0;
}
]]

setfenv(1, require'low'.C)
include'stdlib.h'
include'string.h'
include'limits.h'

-- compiler specific configuration

local khint_t = int32
local khiter_t = khint_t

local isempty           = macro(function(flag, i) return `(flag[i>>4] >> ((i and 0xfU) << 1)) and 2 ~= 0 end)
local isdel             = macro(function(flag, i) return `(flag[i>>4] >> ((i and 0xfU) << 1)) and 1 ~= 0 end)
local iseither          = macro(function(flag, i) return `(flag[i>>4] >> ((i and 0xfU) << 1)) and 3 ~= 0 end)
local set_isdel_false   = macro(function(flag, i) return quote flag[i>>4] = flag[i>>4] and not (1UL << ((i and 0xfU) << 1)) end end)
local set_isempty_false = macro(function(flag, i) return quote flag[i>>4] = flag[i>>4] and not (2UL << ((i and 0xfU) << 1)) end end)
local set_isboth_false  = macro(function(flag, i) return quote flag[i>>4] = flag[i>>4] and not (3UL << ((i and 0xfU) << 1)) end end)
local set_isdel_true    = macro(function(flag, i) return quote flag[i>>4] = flag[i>>4] or 1UL << ((i and 0xfU) << 1) end end)

terra X31_hash_string(s: &int8): khint_t
	var h: khint_t = @s
	if h ~= 0 then
		s = s + 1
		while @s ~= 0 do
			h = (h << 5) - h + [khint_t](s[0])
			s = s + 1
		end
	end
	return h
end
X31_hash_string:setinlined(true)

terra Wang_hash(key: khint_t): khint_t
	key = key + not (key << 15)
	key = key ^     (key >> 10)
	key = key +     (key << 3)
	key = key ^     (key >> 6)
	key = key + not (key << 11)
	key = key ^     (key >> 16)
	return key
end
Wang_hash:setinlined(true)
--kh_int_hash_func2(key) Wang_hash((khint_t)key)

local fsize = macro(function(m) return `iif(m < 16, 1, m >> 4) end)

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

local HASH_UPPER = 0.77

local function __KHASH_PROTOTYPES(name, khkey_t, khval_t)
	--extern kh_t *kh_init(void);
	--extern void kh_destroy(kh_t *h);
	--extern void kh_clear(kh_t *h);
	--extern khint_t kh_get(const kh_t *h, khkey_t key);
	--extern int kh_resize(kh_t *h, khint_t new_n_buckets);
	--extern khint_t kh_put(kh_t *h, khkey_t key, int *ret);
	--extern void kh_del(kh_t *h, khint_t x);
end

local equal = macro(function(a, b) return `a == b end)
local int64_hash_func = macro(function(key) return `[int32](key>>33^key^key<<11) end)
local str_hash_equal = macro(function(a, b) return `strcmp(a, b) == 0 end)

local khash = macro(function(
	khkey_t, khval_t, kh_is_map, hash, hash_equal,
	kcalloc, kmalloc, krealloc, kfree
)

	hash_equal = hash_equal or equal

	if not kcalloc then
		kcalloc = calloc
		kmalloc = malloc
		krealloc = realloc
		kfree = free
	end

	local kh_t = struct {
		n_buckets: khint_t;
		size: khint_t;
		n_occupied: khint_t;
		upper_bound: khint_t;
		flags: &int32;
		keys: &khkey_t;
		vals: &khval_t;
	}

	local terra kh_init(): &kh_t
		return [&kh_t](kcalloc(1, sizeof(kh_t)))
	end
	kh_init:setinlined(true)

	terra kh_t.methods.destroy(h: &kh_t)
		if h ~= nil then
			kfree(h.keys); kfree(h.flags)
			kfree(h.vals)
			kfree(h)
		end
	end
	kh_t.methods.destroy:setinlined(true)

	terra kh_t.methods.clear(h: &kh_t)
		if h ~= nil and h.flags ~= 0 then
			memset(h.flags, 0xaa, fsize(h.n_buckets) * sizeof(int32))
			h.size = 0
			h.n_occupied = 0
		end
	end
	kh_t.methods.clear:setinlined(true)

	terra kh_t.methods.get(h: &kh_t, key: khkey_t): khint_t
		if h.n_buckets ~= 0 then
			var k: khint_t, i: khint_t, last: khint_t, mask: khint_t, step: khint_t = 0
			mask = h.n_buckets - 1
			k = hash(key); i = k and mask
			last = i
			while not isempty(h.flags, i) and (isdel(h.flags, i) or not hash_equal(h.keys[i], key)) do
				step = step + 1
				i = (i + step) and mask
				if i == last then return h.n_buckets end
			end
			return iif(iseither(h.flags, i), h.n_buckets, i)
		else
			return 0
		end
	end

	terra kh_t.methods.resize(h: &kh_t, new_n_buckets: khint_t): int
		-- This function uses 0.25*n_buckets bytes of working space instead of [sizeof(key_t+val_t)+.25]*n_buckets.
		var new_flags: &int32 = nil
		var j: khint_t = 1
		do
			kroundup32(new_n_buckets)
			if new_n_buckets < 4 then new_n_buckets = 4 end
			if h.size >= [khint_t](new_n_buckets * HASH_UPPER + 0.5) then j = 0 -- requested size is too small
			else -- hash table size to be changed (shrink or expand); rehash
				new_flags = [&int32](kmalloc(fsize(new_n_buckets) * sizeof(int32)))
				if new_flags == nil then return -1 end
				memset(new_flags, 0xaa, fsize(new_n_buckets) * sizeof(int32))
				if h.n_buckets < new_n_buckets then -- expand
					var new_keys: &khkey_t = [&khkey_t](krealloc(h.keys, new_n_buckets * sizeof(khkey_t)))
					if new_keys == nil then kfree(new_flags); return -1 end
					h.keys = new_keys
					if kh_is_map then
						var new_vals: &khval_t = [&khval_t](krealloc(h.vals, new_n_buckets * sizeof(khval_t)))
						if new_vals == nil then kfree(new_flags); return -1 end
						h.vals = new_vals
					end
				end -- otherwise shrink
			end
		end
		if j ~= 0 then -- rehashing is needed
			j = 0
			while j ~= h.n_buckets do
				if not iseither(h.flags, j) then
					var key: khkey_t = h.keys[j]
					var val: khval_t
					var new_mask: khint_t = new_n_buckets - 1
					if kh_is_map then val = h.vals[j] end
					set_isdel_true(h.flags, j)
					while true do -- kick-out process; sort of like in Cuckoo hashing
						var k: khint_t, i: khint_t, step: khint_t = 0
						var k = hash(key)
						var i = k and new_mask
						while not isempty(new_flags, i) do
							step = step + 1
							i = (i + step) and new_mask
						end
						set_isempty_false(new_flags, i)
						if i < h.n_buckets and not iseither(h.flags, i) then -- kick out the existing element
							do var tmp: khkey_t = h.keys[i]; h.keys[i] = key; key = tmp; end
							if kh_is_map then var tmp: khval_t = h.vals[i]; h.vals[i] = val; val = tmp; end
							set_isdel_true(h.flags, i) -- mark it as deleted in the old hash table
						else  -- write the element and jump out of the loop
							h.keys[i] = key
							if kh_is_map then h.vals[i] = val end
							break
						end
					end
				end
				j = j + 1
			end
			if h.n_buckets > new_n_buckets then -- shrink the hash table
				h.keys = [&khkey_t](krealloc(h.keys, new_n_buckets * sizeof(khkey_t)))
				if kh_is_map then h.vals = [&khval_t](krealloc(h.vals, new_n_buckets * sizeof(khval_t))) end
			end
			kfree(h.flags) -- free the working space
			h.flags = new_flags
			h.n_buckets = new_n_buckets
			h.n_occupied = h.size
			h.upper_bound = [khint_t](h.n_buckets * HASH_UPPER + 0.5)
		end
		return 0
	end

	terra kh_t.methods.put(h: &kh_t, key: khkey_t, ret: &int): khint_t
		var khint_t x
		if h.n_occupied >= h.upper_bound then -- update the hash table
			if h.n_buckets > (h.size<<1) then
				if kh_resize(h, h.n_buckets - 1) < 0 then -- clear "deleted" elements
					@ret = -1; return h.n_buckets
				end
			elseif kh_resize(h, h.n_buckets + 1) < 0 then -- expand the hash table
				@ret = -1; return h.n_buckets
			end
		end -- TODO: to implement automatic shrinking; resize() already supports shrinking
		do
			var k: khint_t, i: khint_t, site: khint_t, last: khint_t
			var mask: khint_t = h.n_buckets - 1
			var step: khint_t = 0
			x = h.n_buckets; site = x
			k = hash(key); i = k and mask
			if isempty(h.flags, i) then x = i -- for speed up
			else
				last = i
				while not isempty(h.flags, i) and (isdel(h.flags, i) or not hash_equal(h.keys[i], key)) do
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
			h.size = h.size + 1; h.n_occupied = h.n_occupied + 1
			@ret = 1
		elseif isdel(h.flags, x) then -- deleted
			h.keys[x] = key
			set_isboth_false(h.flags, x)
			h.size = h.size + 1
			@ret = 2
		else -- Don't touch h.keys[x] if present and not deleted
			@ret = 0
		end
		return x
	end

	terra kh_t.methods.kh_del(h: &kh_t, x: khint_t)
		if x ~= h.n_buckets and not iseither(h.flags, x) then
			set_isdel_true(h.flags, x)
			h.size = h.size - 1
		end
	end

	hk_t.first = macro(function(h) return 0 end)
	hk_t.last = macro(function(h) return `h.n_buckets end)
	kh_t.exist = macro(function(h, x) return `not iseither(h.flags, x) end)
	kh_t.key = macro(function(h, x) return `h.keys[x] end)
	kh_t.val = macro(function(h, x) return `h.vals[x] end)

	function hk_t.metamethods.__for(h, body)
		return quote
			var i: khint_t = h:first()
			while i ~= h:last() do
				if h:exist(i) then
					[ body(`h:key(i), `h:val(i)) ]
				end
				i = i + 1
			end
		end
	end

end)


--[[
-- More convenient interfaces

--! @function
  @abstract     Instantiate a hash set containing integer keys
  @param  name  Name of the hash table [symbol]

#define KHASH_SET_INIT_INT(name)
	KHASH_INIT(name, int32, char, 0, kh_int_hash_func, kh_int_hash_equal)

--! @function
  @abstract     Instantiate a hash map containing integer keys
  @param  name  Name of the hash table [symbol]
  @param  khval_t  Type of values [type]

#define KHASH_MAP_INIT_INT(name, khval_t)
	KHASH_INIT(name, int32, khval_t, 1, kh_int_hash_func, kh_int_hash_equal)

--! @function
  @abstract     Instantiate a hash set containing 64-bit integer keys
  @param  name  Name of the hash table [symbol]

#define KHASH_SET_INIT_INT64(name)
	KHASH_INIT(name, int64, char, 0, kh_int64_hash_func, kh_int64_hash_equal)

--! @function
  @abstract     Instantiate a hash map containing 64-bit integer keys
  @param  name  Name of the hash table [symbol]
  @param  khval_t  Type of values [type]

#define KHASH_MAP_INIT_INT64(name, khval_t)
	KHASH_INIT(name, int64, khval_t, 1, kh_int64_hash_func, kh_int64_hash_equal)

typedef const char *kh_cstr_t;
--! @function
  @abstract     Instantiate a hash map containing const char* keys
  @param  name  Name of the hash table [symbol]

#define KHASH_SET_INIT_STR(name)
	KHASH_INIT(name, kh_cstr_t, char, 0, kh_str_hash_func, kh_str_hash_equal)

--! @function
  @abstract     Instantiate a hash map containing const char* keys
  @param  name  Name of the hash table [symbol]
  @param  khval_t  Type of values [type]

#define KHASH_MAP_INIT_STR(name, khval_t)
	KHASH_INIT(name, kh_cstr_t, khval_t, 1, kh_str_hash_func, kh_str_hash_equal)

]]
