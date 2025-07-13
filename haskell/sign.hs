sign :: Int -> Int
sign x
    | x > 0     = 1
    | x < 0     = -1
    | otherwise = 0

main :: IO ()
main = do
    print (sign 5)    -- prints 1
    print (sign (-3)) -- prints -1
    print (sign 0)    -- prints 0
