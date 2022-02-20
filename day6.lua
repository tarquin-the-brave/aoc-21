fish = {0,0,0,0,0,0,0,0,0}

for line in io.lines("./inputs/day6-1.txt") do
  for n in line:gmatch('([^,]+)') do fish[n+1] = fish[n+1] + 1 end
end

function fish:newday()
  local breeders = self[1]
  for i = 1, 8 do
    self[i] = self[i+1]
  end
  self[9] = breeders
  self[7] = self[7] + breeders
end

function fish:count()
  local sum = 0
  for i, n in ipairs(self) do
    sum = sum + n
  end
  return sum
end

days = 0
repeat
  days = days + 1
  fish:newday()
until days == 256

print(fish:count())
