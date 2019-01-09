
setfenv(1, require'low'.C)
local khash = require'khash'
local time = require'time'

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
	prf('inserts: %d, unique keys: %2.0f%%, seconds/mil inputs: %.3f\n',
		n, (1.0 * h.count / n) * 100, (clock() - t0) / (n / 1000000.0) )

	t0 = clock()
	for i = 0, n do
		check(h:has(keys[i]))
	end
	prf('lookups: %d, unique keys: %2.0f%%, seconds/mil inputs: %.3f\n',
		n, (1.0 * h.count / n) * 100, (clock() - t0) / (n / 1000000.0) )

	h:free()
	free(keys)
end

local terra test()
	var h: khash.map(int32, int32) = {}
	h:put(6, 7); do var ok, val = h:get(6); check(ok and val == 7) end
	h:put(7, 8)
	h:put(12, 13)
	check(h:has(12))
	h:del(7)
	check(not h:has(7))
	do var ok, val = h:get(7) check(not ok) end
	prf('count: %d\n', h.count)
	for k,v in h do
		prf(' %4d -> %4d\n', k, v)
	end
	h:free()

	for i = 1, 100 do
		pr(i)
		test_int(1, 1)
		test_int(10, 2)
		test_int(1000000, 1.0/4)
	end
end
test()
