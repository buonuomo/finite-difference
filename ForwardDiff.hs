
module ForwardDiff where


import Text.PrettyPrint.Boxes
import Data.List (transpose)


-- Take the forward difference of a list
diff :: Num a => [a] -> [a]
diff xs = zipWith (-) (tail xs) xs

-- Given a starter list, construct the table
analyze :: Num a => [a] -> [[a]]
analyze = iterate diff 

-- Take a h x w rectangle from the table
takeTable :: Int -> Int -> [[a]] -> [[a]] 
takeTable h w = take h . map (take w) 

-- newtype wrapper for pretty printing of tables
newtype Table a = Table {runTable :: [[a]]} 

-- Show instance for Table. Prints in a nice readable grid
instance Show a => Show (Table a) where
    show = render . boxTable . transpose . runTable
        where 
            boxTable :: (Show a) => [[a]] -> Box
            boxTable = foldr ((<+>) . foldr ((//) . text . show) nullBox) nullBox
