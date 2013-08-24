toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x = toDigits( x `div` 10 ) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
  | n == 0 = []
  | n< 0 = []
toDigitsRev x = [x `mod` 10 ] ++ toDigitsRev(x `div` 10)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[])= [x]
doubleEveryOther (x:y:[]) = [2*x,y]
doubleEveryOther (x:y:z:[])   = [x, 2*y, z]
doubleEveryOther (x:y:zs)     = 2*x : y : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits xs  = sum xs 

validate :: Integer -> Bool
validate xs = (sum(doubleEveryOther(toDigits(xs)))) `mod` 10  == 0
