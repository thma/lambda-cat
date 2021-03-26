{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module Lib where
  
import Data.List (elemIndex)
import Data.Maybe (fromJust)

csv :: [Char]
csv =  "Name,Age,Home\n"
    ++ "Luke,19,Tatooine\n"
    ++ "Leia,19,Alderaan\n"
    ++ "Han,32,Corellia"

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn splitter = foldr go [[]]
  where
    go char xs
      -- If the current character is our "split" character create a new partition
      | splitter == char = [] : xs
      -- Otherwise we can add the next char to the current cell
      | otherwise = case xs of
        (cell : rest) -> (char : cell) : rest
        [] -> [[char]]

data CSV index where
  NamedCsv    :: [String] -> [[String]] -> CSV String
  NumberedCsv ::             [[String]] -> CSV Int

-- A side-effect of using GADTs is that we need to use standalone deriving 
-- for our instances.
deriving instance Show (CSV i)
deriving instance Eq   (CSV i)

data CSVType i where
  Named :: CSVType String
  Numbered :: CSVType Int

deriving instance Show (CSVType i)
deriving instance Eq (CSVType i)

decode :: CSVType i -> String -> Maybe (CSV i)
decode Named s = case splitOn ',' <$> lines s of
  (h:xs) -> Just $ NamedCsv h xs
  _      -> Nothing
decode Numbered s = Just . NumberedCsv . fmap (splitOn ',') . lines $ s

numberedCsv :: CSV Int
numberedCsv = fromJust $ decode  Numbered csv

namedCsv :: CSV String
namedCsv = fromJust $ decode Named csv

-- A safe indexing function to get elements by index.
-- This is strangely missing from the Prelude... 🤔
safeIndex :: Int -> [a] -> Maybe a
safeIndex i = lookup i . zip [0 ..]

getColumnByIndex :: i -> CSV i -> Maybe [String]
getColumnByIndex  columnName (NamedCsv headers rows) = do
  columnIndex <- elemIndex columnName headers
  traverse (safeIndex columnIndex) rows
getColumnByIndex n (NumberedCsv rows) = traverse (safeIndex n) rows

getColumnByNumber :: Int -> CSV i -> Maybe [String]
getColumnByNumber n (NamedCsv _ rows)  = traverse (safeIndex n) rows
getColumnByNumber n (NumberedCsv rows) = traverse (safeIndex n) rows