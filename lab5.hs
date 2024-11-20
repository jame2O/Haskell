import Data.Char
import Data.List hiding ((\\))
import System.FilePath (takeFileName)
type Presses = Int
type Text = String
type Button = Char

processInput :: (Button, Presses) -> Text
processInput (b, _)
            | b == '1' = "1"
            | b == '2' = "abc2"
            | b == '3' = "def3"
            | b == '4' = "ghi4"
            | b == '5' = "jkl5"
            | b == '6' = "mno6"
            | b == '7' = "pqrs7"
            | b == '8' = "tuv8"
            | b == '9' = "wxyz9"
            | b == '0' = " "
            | b == '*' = "*"
            | b == '#' = ".,"


phoneToString :: [(Button, Presses)] -> Text
phoneToString = phoneToString' False

phoneToString' :: Bool -> [(Button, Presses)] -> Text
phoneToString' _ [] = ""
phoneToString' cap ((b, p): xs)
    | b == '*' = phoneToString' (not cap) xs
    | otherwise =
        let chars = processInput (b, p)
            index = (p-1) `mod` length chars
            char = chars !! index
            finalChar = if cap then toUpper char else char
        in finalChar : phoneToString' False xs

getIndex :: Int -> Char -> [Char] -> Int
getIndex i x (c:cs)
        | x == c = i
        | otherwise = getIndex (i+1) x cs


processInput' :: Char -> (Button, Presses)
processInput' b
            | b `elem` "1" = ('1', 1)
            | b `elem` "abc2" = ('2', getIndex 0 b "abc2" + 1)
            | b `elem` "def3" = ('3', getIndex 0 b "def3" + 1)
            | b `elem` "ghi4" = ('4', getIndex 0 b "ghi4" + 1)
            | b `elem` "jkl5" = ('5', getIndex 0 b "jkl5" + 1)
            | b `elem` "mno6" = ('6', getIndex 0 b "mno6" + 1)
            | b `elem` "pqrs7" = ('7', getIndex 0 b "pqrs7" + 1)
            | b `elem` "tuv8" = ('8', getIndex 0 b "tuv8" + 1)
            | b `elem` "wxyz9" = ('9', getIndex 0 b "wxyz9" + 1)
            | b `elem` " " = ('0', 1)
            | b `elem` ".," = ('#', getIndex 0 b ".," + 1)



stringToPhone :: Text -> [(Button, Presses)]
stringToPhone = stringToPhone'

stringToPhone' :: Text -> [(Button, Presses)]
stringToPhone' [] = []

stringToPhone' (x:xs)
    | isUpper x = ('*', 1) : stringToPhone' [toLower x] ++ stringToPhone' xs
    | otherwise = processInput' x : stringToPhone' xs


-- Maybe Types

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:xs) = Just x

takeMaybe :: Int -> [a] -> Maybe [a]
takeMaybe n xs
    | n > length xs = Nothing
    | n < 0 = Nothing
    | n == 0 = Just []
    | n > 0 = Just (take n xs)

zipEither :: [a] -> [b] -> Either String [(a,b)]
zipEither xs ys
    | length xs /= length ys = Left "Lists are not the same length"
    | otherwise = Right (zip xs ys)

-- Trees

data BinLN a b = Leaf a | Node b (BinLN a b) (BinLN a b) deriving (Show)
data BinL a = LeafL a | NodeL (BinL a) (BinL a) deriving (Show)

leaves :: BinLN a b -> [a]
leaves (Leaf a) = [a]
leaves (Node _ l r) = leaves l ++ leaves r

exampleTree :: BinLN Int String
exampleTree = Node "root"
                (Node "left"
                    (Leaf 1)
                    (Leaf 2))
                (Node "right"
                    (Leaf 3)
                    (Node "right-right"
                        (Leaf 4)
                        (Leaf 5)))

showBin :: Show a => BinL a -> String
showBin (LeafL a) = concat ["(", show a, ")"]
showBin (NodeL l r) = concat ["(", showBin l, showBin r, ")"]

data BT a = Empty
          | Fork a (BT a) (BT a) deriving (Show, Read, Eq, Ord)


(//) :: BT a -> BT a -> BT a
(//) Empty x = x
(//) t Empty = t
(//) (Fork x l r) s = Fork x l (r // s)

(\\) :: BT a -> BT a -> BT a
(\\) Empty x = x
(\\) t Empty = t
(\\) (Fork x l r) s = Fork x (l \\ s) r

leafIndices :: BT a -> BT (Int,Int)
leafIndices t = fst $ labelLeaves t 0

labelLeaves :: BT a -> Int -> (BT (Int, Int), Int)
labelLeaves Empty i = (Empty, i)
labelLeaves (Fork _ Empty Empty) i = (Fork (i, i) Empty Empty, i+1)
labelLeaves (Fork _ l r) i =
    
