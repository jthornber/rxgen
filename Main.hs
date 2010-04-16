module Main where

import Control.Applicative
import Data.Char
import Data.List hiding (or)
import Prelude hiding (or)
import System.Environment
import System.Exit
import System.Random
import Test.QuickCheck hiding (sample)
import Test.QuickCheck.Gen hiding (sample)

data Regex = Charset String
           | Or Regex Regex
           | Cat Regex Regex
           | Star Regex
           | Plus Regex
           | Quest Regex
             deriving (Eq)

simplify' r@(Charset _)     = r
simplify' (Or r1 r2)        = Or (simplify' r1) (simplify' r2)
simplify' (Cat r1 r2)       = Cat (simplify' r1) (simplify' r2)
simplify' (Star (Star r))   = Star (simplify' r)
simplify' (Star (Plus r))   = Star (simplify' r)
simplify' (Star (Quest r))  = Star (simplify' r)
simplify' (Star r)          = Star (simplify' r)
simplify' (Plus (Star r))   = Star (simplify' r)
simplify' (Plus (Plus r))   = Plus (simplify' r)
simplify' (Plus (Quest r))  = Star (simplify' r)
simplify' (Plus r)          = Plus (simplify' r)
simplify' (Quest (Star r))  = Star (simplify' r)
simplify' (Quest (Plus r))  = Star (simplify' r)
simplify' (Quest (Quest r)) = Quest (simplify' r)
simplify' (Quest r)         = Quest (simplify' r)

simplify r = fst .
             head .
             dropWhile (\p -> fst p /= snd p) .
             zip rs $ tail rs
    where
      rs = iterate simplify' r

ppRegex :: Regex -> String
ppRegex (Charset [c]) = [c]
ppRegex (Charset str) = "[" ++ str ++ "]"
ppRegex (Or r1 r2)    = bracketPair r1 r2 "|"
ppRegex (Cat r1 r2)   = bracketPair r1 r2 ""
ppRegex (Star r)      = bracket r ++ "*"
ppRegex (Plus r)      = bracket r ++ "+"
ppRegex (Quest r)     = bracket r ++ "?"

bracket r@(Charset _)         = ppRegex r
bracket r                     = "(" ++ ppRegex r ++ ")"

bracketPair r1 r2 sep = bracket r1 ++ sep ++ bracket r2

instance Show Regex where
    show = ppRegex

instance Arbitrary Regex where
    arbitrary = sized regex

regex 0 = resize 1 (sized charset)
regex n = oneof [ resize (min n 10) (sized charset)
                , binOp Or (n `div` 2)
                , binOp Cat (n `div` 2)
                , unOp Star (n - 1)
                , unOp Plus (n - 1)
                , unOp Quest (n - 1)
                ]

unOp op n = op <$> regex n
binOp op n = op <$> regex n <*> regex n

charset n = do
  txt <- resize (n * 2) $ listOf1 chars
  return . Charset . nub . sort $ txt

chars = oneof [ choose ('a', 'z')
              , choose ('A','Z')
              ]

sample n s (MkGen m) = do
  rnd <- newStdGen
  let rnds rnd = rnd1 : rnds rnd2 where (rnd1, rnd2) = split rnd
  return [(m r s) | r <- take n $ rnds rnd]

rxgen :: Int -> Int -> IO [Regex]
rxgen n s = sample n s $ (arbitrary :: Gen Regex)

usage = do
  putStrLn "usage: rxgen <number of regexps to generate> <complexity (number)>"
  exitWith $ ExitFailure 1

main :: IO ()
main = do
  args <- getArgs
  case args of
    [count, size] -> rxgen (read count) (read size) >>=
                     putStr . unlines . map (show . simplify)
    _ -> usage
    