crabs = {}
for line in io.lines("./inputs/day7-1.txt") do
  for n in line:gmatch('([^,]+)') do crabs[#crabs+1]=n end
end

function triangle(n)
  if n == 0 then return 0 end
  return n + triangle(n - 1)
end

function bin_search(f, n, nprev)
  local value_prev = f(n - 1)
  local value = f(n)
  local value_next = f(n + 1)

  -- print(string.format("pos: %s, %s, %s value: %s, %s, %s", n-1, n, n+1, value_prev, value, value_next))

  if value_prev < value then
    return bin_search(f, n // 2, n)
  elseif value_next < value then
    return bin_search(f, ((nprev - n) // 2) + n + 1, n)
  else
    return value
  end
end

function fuel1(crabs, position)
  local sum = 0
  for i, crab in ipairs(crabs) do
    sum = sum + math.abs(position - crab)
  end
  return sum
end

part1 = bin_search(function (n) return fuel1(crabs, n) end, 500, 1000)

print(part1)

function fuel2(crabs, position)
  local sum = 0
  for i, crab in ipairs(crabs) do
    sum = sum + triangle(math.abs(position - crab))
  end
  return sum
end

part2 = bin_search(function (n) return fuel2(crabs, n) end, 500, 1000)

print(part2)

