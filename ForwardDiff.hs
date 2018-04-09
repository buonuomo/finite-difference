module ForwardDiff where

import Text.PrettyPrint.Boxes
import Data.List (transpose)

-- Take the forward difference of a list
diff :: Num a => [a] -> [a]
diff = zipWith (-) <$> tail <*> id

-- Take the forward sum of a list (like integrating)
integ :: Num a => [a] -> [a]
integ = scanl1 (+)

-- Given a starter list, construct the difference table
diffTable :: Num a => [a] -> [[a]]
diffTable = iterate diff 

-- given a starter list, construct integration table
intTable :: Num a => [a] -> [[a]]
intTable = iterate integ

-- Undifferentiates the first column of a table into the next column of the 'original' table
undiff :: Num a => [a] -> [a]
undiff = zipWith (+) <$> tail <*> id

-- Constructs a diff table from the first column
undiffTable :: Num a => [a] -> [[a]]
undiffTable = transpose . iterate undiff

-- Take an h by w rectangle from the table
takeTable :: Num a => Int -> Int -> [[a]] -> [[a]] 
takeTable h w = take h . map (take w) 

-- newtype wrapper for pretty printing of tables
newtype Table a = Table {runTable :: [[a]]} 

-- Show instance for Table. Prints in a nice readable grid
instance Show a => Show (Table a) where
    show = render . boxTable . transpose . runTable
        where 
            boxTable :: (Show a) => [[a]] -> Box
            boxTable = foldr ((<+>) . foldr ((//) . text . show) nullBox) nullBox
