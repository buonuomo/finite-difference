import Text.PrettyPrint.Boxes
import Data.List (transpose)

--newtype Sequence a = Sequence {runSeq :: [a]}
newtype Table a = Table {runTable :: [[a]]} 

diff :: Num a => [a] -> [a]
diff xs = zipWith (-) (tail xs) xs

analyze :: Num a => [a] -> Table a
analyze = Table . iterate diff 

takeTable :: Int -> Int -> Table a -> Table a 
takeTable h w = Table . take h . map (take w) . runTable

{-
instance Show a => Show (Sequence a) where
    show s = render $ boxed s
        where boxed :: (Show a) => Sequence a -> Box
              boxed (Sequence []) = nullBox
              boxed (Sequence (x:xs)) = text (show x) <+> boxed (Sequence xs)
-}

instance Show a => Show (Table a) where
    show t = render . boxed . Table . transpose . runTable$ t
        where boxed :: (Show a) => Table a -> Box
              boxed (Table []) = nullBox
              boxed (Table (x:xs)) = boxbox x <+> boxed (Table xs)

              boxbox :: (Show a) => [a] -> Box
              boxbox [] = nullBox
              boxbox (x:xs) = text (show x) // boxbox xs
