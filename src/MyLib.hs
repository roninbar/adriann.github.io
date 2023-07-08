{-# LANGUAGE MonadComprehensions #-}

module MyLib
  ( hello
  , greet
  , sumn
  ) where

import           Control.Monad                  ( when )
import           Text.Printf                    ( printf )

hello :: IO ()
hello = putStrLn "Hello, world!"

greet :: IO ()
greet = do
  putStrLn "What is your name?"
  name <- getLine
  when (name == "Ron") $ putStrLn "You're a wizard, Ron!"

sumn :: IO ()
sumn = do
  putStrLn "Enter a number:"
  n <- readLn :: IO Int
  printf "1 + ... + %d = %d\n" n (sum [1 .. n])

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



