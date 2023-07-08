{-# LANGUAGE MonadComprehensions #-}

module MyLib
  ( hello
  , greet
  , sumn
  ) where

import           Control.Monad                  ( when )
import           Data.Char                      ( intToDigit )
import           Text.Printf                    ( printf )
import           Text.Read                      ( Lexeme(String) )

-- Elementary

-- 1. Write a program that prints ‘Hello World’ to the screen.
hello :: IO ()
hello = putStrLn "Hello, world!"

-- 2. Write a program that asks the user for their name and greets them with their name.
-- 3. Modify the previous program such that only the users Alice and Bob are greeted with their names.
greet :: IO ()
greet = do
  putStrLn "What is your name?"
  name <- getLine
  when (name == "Ron") $ putStrLn "You're a wizard, Ron!"

-- 4. Write a program that asks the user for a number n and prints the sum of the numbers 1 to n
sumn :: IO ()
sumn = do
  putStrLn "Enter a number:"
  n <- readLn :: IO Int
  printf "1 + ... + %d = %d\n" n (sum [1 .. n])

-- 7. Write a program that prints a multiplication table for numbers up to 12.
mul12 :: IO ()
mul12 = mul12' 1 where
  mul12' :: Int -> IO ()
  mul12' i = when
    (i <= 12)
    (do
      row
      mul12' (i + 1)
    )   where
    row :: IO ()
    row = do
      row' 1
      printf "\n"     where
      row' :: Int -> IO ()
      row' j = when
        (j <= 12)
        (do
          printf "%4d" (i * j)
          row' (j + 1)
        )

mul12' :: IO ()
mul12' = sequence_ [ row i | i <- [1 .. 12] ] where
  row :: Int -> IO ()
  row i = do
    sequence_ [ printf "%4d" $ i * j | j <- [1 .. 12] ]
    printf "\n"

{-- 11. 
Write a program that computes the sum of an alternating series where each element 
of the series is an expression of the form 
for each value of (-1) ^ (k + 1) / (2 * k - 1)
from 1 to a million, multiplied by 4. 
--}
euler :: Fractional a => [a] -> [a]
euler ss =
  let s0 = head ss
      s1 = ss !! 1
      s2 = ss !! 2
  in  s2 - (s2 - s1) ^ 2 / (s0 - 2 * s1 + s2) : euler (tail ss)

pisum :: Int -> Double
pisum n = 4 * sum [ (-1) ^ (k + 1) / realToFrac (2 * k - 1) | k <- [1 .. n] ]

pis :: [Double]
pis = (4 *) <$> scanl1 (+) (summands 1)
  where summands n = 1 / n : (negate <$> summands (n + 2))

pis' :: [Double]
pis' = euler pis

pis'' :: [Double]
pis'' = head <$> iterate euler pis

-- Lists, Strings

-- Write a function that merges two sorted lists into a new sorted list. [1,4,6],[2,3,5] → [1,2,3,4,5,6]. You can do this quicker than concatenating them followed by a sort.
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xs@(x : xs') ys@(y : ys') =
  if x <= y then x : merge xs' ys else y : merge xs ys'

-- Write a function that takes a number and returns a list of its digits. So for 2342 it should return [2,3,4,2].
digitsFrom :: Int -> String
digitsFrom n | n < 0 = "-" ++ digitsFrom (-n)
digitsFrom 0         = "0"
digitsFrom n         = digitsFrom' n "" where
  digitsFrom' :: Int -> String -> String
  digitsFrom' 0 ds = ds
  digitsFrom' n ds = digitsFrom' (n `div` 10) $ intToDigit (n `mod` 10) : ds


