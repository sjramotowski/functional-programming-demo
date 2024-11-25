-- Write printAMessage here
printAMessage :: Show a => a -> IO ()
printAMessage a = print a


-- Write division here
division :: Double -> Double -> Maybe Double
division x y
    | y == 0 = Nothing
    | otherwise = Just (x/y)


-- Write factorial here
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)


-- Write factList here
factList :: Int -> [Int]
factList n = map factorial [1..n]



-- Write merge here
merge :: [Int] -> [Int] -> [Int]
merge [] ls2 = ls2
merge ls1 [] = ls1
--is and js = tails
merge (i:is) (j:js)
    | i <= j    = i : merge is (j:js)
    | otherwise = j : merge (i:is) js


-- main = print "Test" -- Replace this with your testing code

main = do
    -- printAMessage test

    printAMessage "Hello, World!"

    -- division tests

    -- valid inputs: Output: Just 5.0
    let div1 = division 10 2
    print div1
  
    --divide by zero: Output: Nothing
    let div2 = division 10 0
    print div2
  
    --decimals:  Output: Just 4.0
    let div3 = division 10.0 2.5
    print div3

    --factorial tests

    --0: Output: 1
    let fact0 = factorial 0
    print fact0

    --1: Output: 1
    let fact1 = factorial 1
    print fact1

    --5: Output: 120
    let fact5 = factorial 5
    print fact5

    --factList tests

    --1: Output: [1]
    let factL1 = factList 1
    print factL1

    --3: Output: [1,2,6]
    let factL3 = factList 3
    print factL3

    --5: Output: [1,2,6,24,120]
    let factL5 = factList 5
    print factL5

    --merge tests

    --lists same size: Output: [1,2,3,4,5,6]
    let merge1 = merge [1, 2, 3] [4, 5, 6]
    print merge1

    --Second list empty: Output: [1,2,3]
    let merge2 = merge [1, 2, 3] []
    print merge2

    --First list empty: Output: [4,5,6]
    let merge3 = merge [] [4, 5, 6]
    print merge3

    --Both list empty: Output: [}
    let merge4 = merge [] []
    print merge4

