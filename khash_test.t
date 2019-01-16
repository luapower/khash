setfenv(1, require'low')

local random_keys = function(ktype, gen_key, n)
	return quote
		var keys = new(ktype, n)
		for i = 0, n do
			keys[i] = [gen_key(i)]
		end
	in
		keys
	end
end

local test_speed = function(ktype, vtype, gen_key, n, hash, equal, size_t)
	local C = allocs()
	C.memset = memset
	return quote
		var keys = [random_keys(ktype, gen_key, n)]
		var h: map(ktype, vtype, hash, equal, size_t, nil, C) = {}
		var t0 = clock()
		for i = 0, n do
			var k = keys[i]
			var i = h:put(k, 0)
			assert(h:equal(h:key_at(i), k))
			if i >= 0 then
				h:val_at(i) = h:val_at(i) + 1
			end
		end
		prf('key size: %2d, inserts: %8d, unique keys: %3.0f%%, mil/sec: %8.3f\n',
			sizeof(ktype), n, (1.0 * h.count / n) * 100, (n / 1000000.0) / (clock() - t0) )

		t0 = clock()
		for i = 0, n do
			assert(h:has(keys[i]))
		end
		prf('key size: %2d, lookups: %8d, unique keys: %3.0f%%, mil/sec: %8.3f\n',
			sizeof(ktype), n, (1.0 * h.count / n) * 100, (n / 1000000.0) / (clock() - t0) )

		h:free()
		free(keys)
	end
end

local test_speed_int32 = function(n, u)
	local gen_key = function(i) return `random(u) end
	return test_speed(int32, int32, gen_key, n)
end

C[[
#include <stdint.h>
uint64_t hash_64(uint64_t key)
{
	key = ~key + (key << 21);
	key = key ^ key >> 24;
	key = (key + (key << 3)) + (key << 8);
	key = key ^ key >> 14;
	key = (key + (key << 2)) + (key << 4);
	key = key ^ key >> 28;
	key = key + (key << 31);
	return key;
}
]]

local test_speed_int64 = function(n, u)
	local gen_key = function(i) return `random(u) end
	return test_speed(int64, int32, gen_key, n, hash_64)
end

local test_speed_int64_large = function(n, u)
	local gen_key = function(i) return `random(u) end
	local hash = macro(function(n) return `n end)
	return test_speed(int64, int32, gen_key, n, hash, nil, int64)
end

local hash_fnv_1a = terra(s: &int8, n: int): int32 --FNV-1A
	var d: uint32 = 0x811C9DC5
	for i=0,n do
		d = ((d ^ s[i]) * 16777619) and 0x7fffffff
	end
	return d
end

local test_speed_tuple16 = function(n, u)
	local T = tuple(int64, int32, float)
	local gen_key = function(i)
		return quote
			var key: T
			key._0 = random(u)
			key._1 = random(u)
			key._2 = random(u)
		in
			key
		end
	end
	local hash = macro(function(t)
		return `hash_fnv_1a([&int8](&t), sizeof(T))
	end)
	return test_speed(T, int32, gen_key, n)
end

local terra test()
	var h = map(int32, int32)
	h:put(6, 7); assert(h:get(6, 0) == 7)
	h:put(7, 8)
	h:put(12, 13)
	assert(h:has(12))
	h:del(7)
	assert(not h:has(7))
	assert(h:get(7, -1) == -1)
	prf('count: %d\n', h.count)
	for k,v in h do
		prf(' %4d -> %4d\n', k, v)
	end
	h:free()

	[test_speed_int32(1000000, 1000000 * .25)]
	[test_speed_int64(1000000, 1000000 * .25)]
	[test_speed_int64_large(1000000, 1000000 * .25)]
	[test_speed_tuple16(100000, 30)]
end
test()
