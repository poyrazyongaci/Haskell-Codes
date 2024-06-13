--seperates the card number into its digits and places it into a list
seperator :: Int -> [Int]
seperator 0 = []
seperator n = seperator ( n `div` 10) ++ [n `mod` 10]
--even indexed digits are doubled and placed into a list
doubler :: Int -> [Int]
doubler a = 
    let zippedlist = zip ([0..] :: [Int]) $ seperator a
        doubled = [2*y | (x,y) <-zippedlist ,even x]
    in doubled
--odd indexed digits are extracted and placed into a list
unchangedlister :: Int -> [Int]
unchangedlister b =
    let zippedlist = zip ([0..] :: [Int]) $ seperator b
        singled = [y | (x,y) <-zippedlist ,odd x]
    in singled
-- sums up both the doubled even indexed values and odd indexed values.
-- needs as doubled digits can result in 2 digit numbers, int to string and to int conversion
summer :: Int -> Int
summer n = 
    let lastdigitseperateddoubled = seperator (read (concat ( map show $ doubler n)) :: Int)
    in (sum(lastdigitseperateddoubled)+sum(unchangedlister n))
-- checks validity through divisibility by 10
validityChecker :: Int -> Bool
validityChecker n = (summer  n) `mod` 10 == 0

-- simple IO
main :: IO()
main = do
    putStrLn "Enter Credit Card Number"
    cardnumberStr <- getLine
    let cardnumber = read cardnumberStr :: Int
    print (validityChecker cardnumber )

    
    
    


    
    