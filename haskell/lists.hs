[x^2 | x <- [(-1),(-2)..(-10)]]  -- This would work
-- OR
[x^2 | x <- reverse [(-10)..(-1)]]  -- Another approach
[-1,-2..(-10)]  -- Counts down from -1 to -10[10,9..1]      -- Counts down from 10 to 1
let fst' (x,_) = x
fst' (1, 2)[x^2 | x <- [(-1),(-2)..(-10)]]  -- This would work
-- OR
[x^2 | x <- reverse [(-10)..(-1)]]  -- Another approach2**(1/2)  -- Square root of 2
8**(1/3)  -- Cube root of 8[x**(-1) + 1 | x <- reverse [(-10)..(-1)]]  -- This would work
-- OR
map (+1) [x**(-1) | x <- reverse [(-10)..(-1)]]-- Filter elements
[x^2 | x <- [1..20], x `mod` 3 == 0]  -- Squares of multiples of 3

-- Multiple generators
[(x,y) | x <- [1..3], y <- [4..6]]  -- All combinations

-- Pattern matching in list comprehensions
[(a,b) | (a:b:_) <- [[1,2,3], [4,5,6]]]  -- Extract first two elements-- Nested list comprehensions
[[ x*y | y <- [1..5]] | x <- [1..5]]  -- Multiplication table

-- Guards in list comprehensions
[(x,y) | x <- [1..5], y <- [1..5], x + y > 8]  -- Sum greater than 8

-- Function composition
(reverse . take 5) [1..10]  -- Take 5 then reverse