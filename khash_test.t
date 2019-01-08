
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

terra test_int(n: int, u: double)

	var keys = new(int32, n)
	for i = 0, n do
		keys[i] = random(n * u)
	end

	var h: map = {}
	var t0 = clock()
	for i = 0, n do
		--printf('%p %d %d %d\n', &h, n, x, key)
		var k = keys[i]
		var i = h:put(k, 0)
		check(h:key_at(i) == k)
		--printf('i: %d, lasterror: %d, %p key: %d\n', i, h.lasterror, h.keys, h:key_at(i))
		if i >= 0 then
			h:val_at(i) = h:val_at(i) + 1
		end
	end
	printf('inserts: %d, unique keys: %2.0f%%, seconds/mil inputs: %.3f\n',
		n, (1.0 * h.count / n) * 100, (clock() - t0) / (n / 1000000.0) )

	t0 = clock()
	for i = 0, n do
		check(h:has(keys[i]))
	end
	printf('lookups: %d, unique keys: %2.0f%%, seconds/mil inputs: %.3f\n',
		n, (1.0 * h.count / n) * 100, (clock() - t0) / (n / 1000000.0) )

	h:free()
end

local terra test()
	printf'starting...\n'
	var h: khash.map(int32, int32) = {}
	h:put(6, 7); do var ok, val = h:get(6); check(ok and val == 7) end
	h:put(7, 8)
	h:put(12, 13)
	check(h:has(12))
	h:del(7)
	check(not h:has(7))
	do var ok, val = h:get(7) check(not ok) end
	printf('count: %d\n', h.count)
	for k,v in h do
		printf(' %4d -> %4d\n', k, v)
	end
	h:free()

	test_int(1, 1)
	test_int(10, 2)
	test_int(1000000, 1.0/4)
end
test()
