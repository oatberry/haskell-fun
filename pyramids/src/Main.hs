module Main where

import qualified Data.Text                     as T
import           System.Environment             ( getArgs )
import           System.Exit                    ( die )

-- Create one layer of a pyramid
layer :: T.Text -> Int -> Int -> T.Text
layer char base level =
  spaces `T.append` T.replicate level char `T.append` spaces
  where spaces = T.replicate ((base - level) `div` 2) $ T.singleton ' '

-- Create one pyramid
pyramid :: T.Text -> Int -> [T.Text]
pyramid char base = go base 1 -- 1 is the top level
 where
  go base level | base == level = [T.replicate base char]
                | otherwise     = layer char base level : go base (level + 2)

-- Create a sequence of pyramids
pyramids :: T.Text -> Int -> Int -> T.Text
pyramids char base count =
  T.unlines . map (T.unwords . replicate count) $ pyramid char base

main :: IO ()
main = do
  args                <- getArgs
  (char, base, count) <- parse args
  validate char base count
  putStr . T.unpack $ pyramids (T.singleton char) base count

usage = "Usage: pyramids [character] <basesize> <count>"

parse :: [String] -> IO (Char, Int, Int)
parse []                  = die usage
parse ("" : _)            = die usage
parse [char, base, count] = return (head char, read base, read count)
parse [base, count]       = return ('#', read base, read count)
parse _                   = die usage

validate :: Char -> Int -> Int -> IO ()
validate char base count
  | even base               = die "error: must use an even base size"
  | base <= 0 || count <= 0 = die "error: quantities cannot be non-positive"
  | otherwise               = return ()
