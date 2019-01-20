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
			assert(h:equal(&h:key_at_index(i), &k))
			if i >= 0 then
				inc(h:val_at_index(i))
			end
		end
		prf('key size: %4d, inserts: %8d, unique keys: %3.0f%%, mil/sec: %8.3f',
			sizeof(ktype), n, (1.0 * h.count / n) * 100, (n / 1000000.0) / (clock() - t0) )

		t0 = clock()
		for i = 0, n do
			assert(h:has(keys[i]))
		end
		prf('key size: %4d, lookups: %8d, unique keys: %3.0f%%, mil/sec: %8.3f',
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
]] --not used (slower than default).

local test_speed_int64 = function(n, u)
	local gen_key = function(i) return `random(u) end
	return test_speed(int64, int32, gen_key, n)
end

local test_speed_int64_large = function(n, u)
	local gen_key = function(i) return `random(u) end
	return test_speed(int64, int32, gen_key, n, nil, nil, int64)
end

includepath'$L/csrc/xxhash'
include'xxhash.h'
linklibrary'xxhash'

local test_speed_large = function(n, u, size, size_t)
	local T = struct { a: uint8[size]; }
	local gen_key = function(i)
		return quote
			var key: T
			for i=0,size,8 do
				key.a[i] = random(u)
			end
			in key
		end
	end
	T.methods.__hash32 = macro(function(t) return `XXH32(t, sizeof(T), 0) end)
	T.methods.__hash64 = macro(function(t) return `XXH64(t, sizeof(T), 0) end)
	return test_speed(T, int32, gen_key, n, nil, nil, size_t)
end

local terra test_speed()
	var h = map(int32, int32)
	h:put(6, 7); assert(h:get(6, 0) == 7)
	h:put(7, 8)
	h:put(12, 13)
	assert(h:has(12))
	h:del(7)
	assert(not h:has(7))
	assert(h:get(7, -1) == -1)
	prf('count: %d', h.count)
	for k,v in h do
		prf(' %4d -> %4d', k, v)
	end
	h:free()

	[test_speed_int32(1000000, 1000000 * .25)]
	[test_speed_int64(1000000, 1000000 * .25)]
	[test_speed_int64_large(1000000, 1000000 * .25)]
	[test_speed_large(100000, 2, 120, uint32)]
	[test_speed_large(100000, 2, 120, uint64)]
end

test_speed()
