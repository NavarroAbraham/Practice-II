-- Function to check if a number is even
compares :: (Integer -> Bool) -> (Integer -> String)
compares x y = if (x y) then "even" else "odd"

isEven :: Integer -> String
isEven = compares (even)

-- Function to calculate aliquot sum (sum of proper divisors)
aliquotSum :: Int -> Int
aliquotSum n = sum [d | d <- [1..n-1], n `mod` d == 0]

-- Function to classify number based on Nicomachus scheme (for program category)
programCategory :: Int -> String
programCategory n
  | aliquotSum n > n = "Administrative"  -- Abundant number
  | aliquotSum n == n = "Engineering"     -- Perfect number
  | otherwise = "Humanities"              -- Deficient number

-- Function to convert period code to human-readable format
periodCodeToString :: Int -> String
periodCodeToString periodCode = 
  let (year, sem) = divMod periodCode 10
  in "20" ++ show (year) ++ "-" ++ show sem

-- Function to parse the student code and generate the characteristics
parseStudentCode :: String -> String
parseStudentCode code = 
  if (length code /= 8) then "Error: The last three digits must contain exactly three numbers." else
  let periodCode = read (take 3 code) :: Int
      categoryCode = read (take 2 (drop 3 code)) :: Int
      consecutive = read (drop 5 code) :: Int
      periodStr = periodCodeToString periodCode
      categoryStr = programCategory categoryCode
      consecutiveStr = "num" ++ show consecutive
      parityStr = isEven (read code)
  in periodStr ++ " " ++ categoryStr ++ " " ++ consecutiveStr ++ " " ++ parityStr

-- Main function to get input from user and display the characteristics
main :: IO ()
main = do
  code <- getLine
  let result = parseStudentCode code
  putStrLn result