
setfenv(1, require'low'.C)
local khash = require'khash'
local time = require'time'

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

local clock = terralib.cast({} -> {double}, time.clock)
local random = terralib.cast({double} -> {double}, math.random)

local map = khash.map(int32, int32)

terra test_int(n: int32)

	var keys = new(int32, n)
	for i = 0, n do
		keys[i] = random(n / 4)
	end

	var h: map = {}
	var t0 = clock()
	for i = 0, n do
		--printf('%p %d %d %d\n', &h, n, x, key)
		var k = keys[i]
		var i = h:put_key(k)
		check(h:key_at(i) == k)
		--printf('i: %d, lasterror: %d, %p key: %d\n', i, h.lasterror, h.keys, h:key_at(i))
		if h.lasterror == khash.ABSENT then
			h:val_at(i) = 0
		end
		--var v = &h:val_at(i)
		--@v = @v + 1
	end
	printf('# unique keys: %d, seconds/mil inputs: %.3f\n',
		h.count, (clock() - t0) / (n / 1000000.0) )
	h:free()
end

local terra test()
	--[=[
	var map: khash.map(int32, int64) = nil
	var t0 = clock()
	for i = 1, 1e6 do
		map:put(random(1e6), random(1e6))
		--check(map:get(6) == {true, 5})
	end
	printf('time for %d
	map:put(6, 7); check(map:get(6) == 7)
	map:put(7, 8)
	map:put(12, 13)
	check(map:has(12))
	map:del(7)
	check(not map:has(7))
	check(map:get(7) == [khash.invalid_value[int64]])
	printf('count: %d\n', map.count)
	for k,v in map do
		printf(' %4d -> %4d\n', k, v)
	end
	map:free()
	]=]
	test_int(10000)
end
test()
