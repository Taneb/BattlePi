module BattlePi.Types where

import Control.Applicative
import Control.Monad
import qualified Data.Foldable as F
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S

data CoordBit = A | B | C | D | E | F
              deriving (Eq, Ord, Show, Enum, Bounded)
                       
data Coord =
  TopBit CoordBit CoordBit |
  CentralBit CoordBit CoordBit |
  RightBit CoordBit CoordBit
  deriving (Eq, Ord)

readCoord :: String -> Maybe (Coord, String)
readCoord (c:ns) = case reads ns of
  [(n,r)] 
    | c `elem` "ABCDEF" && n `elem` [1.. 6] -> 
      flip (,) r <$> liftA2 TopBit     (fromC c) (fromN n)
    | c `elem` "GHIJKL" && n `elem` [1.. 6] -> 
      flip (,) r <$> liftA2 CentralBit (fromC c) (fromN n)
    | c `elem` "GHIJKL" && n `elem` [7..12] -> 
      flip (,) r <$> liftA2 RightBit   (fromC c) (fromN n)
    | otherwise -> Nothing
  _ -> Nothing
  where 
    fromC :: Char -> Maybe CoordBit
    fromC k | k `elem` "AG" = Just A
            | k `elem` "BH" = Just B
            | k `elem` "CI" = Just C
            | k `elem` "DJ" = Just D
            | k `elem` "EK" = Just E
            | k `elem` "FL" = Just F
            | otherwise = Nothing
    fromN :: Int -> Maybe CoordBit
    fromN n | n `elem` [1, 7] = Just A
            | n `elem` [2, 8] = Just B
            | n `elem` [3, 9] = Just C
            | n `elem` [4,10] = Just D
            | n `elem` [5,11] = Just E
            | n `elem` [6,12] = Just F
readCoord _ = Nothing

showLowR :: CoordBit -> Char
showLowR c = case c of
  A -> 'A'
  B -> 'B'
  C -> 'C'
  D -> 'D'
  E -> 'E'
  F -> 'F'

showHighR :: CoordBit -> Char
showHighR c = case c of
  A -> 'G'
  B -> 'H'
  C -> 'I'
  D -> 'J'
  E -> 'K'
  F -> 'L'

showLowC :: CoordBit -> String
showLowC c = show $ case c of
  A -> 1 :: Int
  B -> 2
  C -> 3
  D -> 4
  E -> 5
  F -> 6

showHighC :: CoordBit -> String
showHighC c = show $ case c of
  A ->  7 :: Int
  B ->  8
  C ->  9
  D -> 10
  E -> 11
  F -> 12

instance Show Coord where
  show (TopBit     a b) = showLowR  a : showLowC  b
  show (CentralBit a b) = showHighR a : showLowC  b
  show (RightBit   a b) = showHighR a : showHighC b

instance Read Coord where
  readsPrec _  = maybe [] pure . readCoord

below :: Coord -> Maybe Coord
below (TopBit F x) = Just $ CentralBit A x
below (TopBit x y) = Just $ TopBit (succ x) y
below (CentralBit F _) = Nothing
below (CentralBit x y) = Just $ CentralBit (succ x) y
below (RightBit F _) = Nothing
below (RightBit x y) = Just $ RightBit (succ x) y

above :: Coord -> Maybe Coord
above (TopBit A _) = Nothing
above (TopBit x y) = Just $ TopBit (pred x) y
above (CentralBit A y) = Just $ TopBit F y
above (CentralBit x y) = Just $ CentralBit (pred x) y
above (RightBit A _) = Nothing
above (RightBit x y) = Just $ RightBit (pred x) y

rightOf :: Coord -> Maybe Coord
rightOf (TopBit _ F) = Nothing
rightOf (TopBit x y) = Just $ TopBit x (succ y)
rightOf (CentralBit x F) = Just $ RightBit x A
rightOf (CentralBit x y) = Just $ CentralBit x (succ y)
rightOf (RightBit _ F) = Nothing
rightOf (RightBit x y) = Just $ RightBit x (succ y)

leftOf :: Coord -> Maybe Coord
leftOf (TopBit _ A) = Nothing
leftOf (TopBit x y) = Just $ TopBit x (pred y)
leftOf (CentralBit _ A) = Nothing
leftOf (CentralBit x y) = Just $ CentralBit x (pred y)
leftOf (RightBit x A) = Just $ CentralBit x F
leftOf (RightBit x y) = Just $ RightBit x (pred y)

data Shape =
  Destroyer |
  Cruiser |
  Battleship |
  Hovercraft |
  AircraftCarrier
  deriving (Eq, Ord, Enum, Bounded, Show)

data Orientation =
  North | East | South | West
  deriving (Eq, Ord, Enum, Bounded, Show)


compass :: Orientation -> Coord -> Maybe Coord
compass North = above
compass East = rightOf
compass West = leftOf
compass South = below

rotateCw :: Orientation -> Orientation
rotateCw dir = case dir of
  North -> East
  East -> South
  South -> West
  West -> North

instance Monoid Orientation where
  mempty = North
  mappend North = id
  mappend East = rotateCw
  mappend South = rotateCw . rotateCw
  mappend West = rotateCw . rotateCw . rotateCw

path :: [Orientation] -> Coord -> Maybe Coord
path = foldr ((>=>) . compass) Just

figure :: Shape -> Coord -> Orientation -> Maybe (Set Coord)
figure s c0 d = fmap S.fromList go
  where
    go :: Maybe [Coord]
    go = case s of
      Destroyer -> do
        c1 <- compass d c0
        return [c0,c1]
      Cruiser -> do
        c1 <- compass d c0
        c2 <- compass d c1
        return [c0,c1,c2]
      Battleship -> do
        c1 <- compass d c0
        c2 <- compass d c1
        c3 <- compass d c2
        return [c0,c1,c2,c3]
      Hovercraft -> do
        c1 <- compass d c0
        c2 <- compass (d <> East) c1
        c3 <- compass d c2
        c4 <- compass (d <> West) c1
        c5 <- compass d c4
        return [c0,c1,c2,c3,c4,c5]
      AircraftCarrier -> do
        c1 <- compass d c0
        c2 <- compass d c1
        c3 <- compass d c2
        c4 <- compass (d <> East) c3
        c5 <- compass (d <> West) c3
        return [c0,c1,c2,c3,c4,c5]
        

countCs :: Set Shape -> Int
countCs = getSum . F.foldMap (Sum . f)
  where
    f Destroyer = 2
    f Cruiser = 3
    f Battleship = 4
    f Hovercraft = 6
    f AircraftCarrier = 6

data Result = Hit | Miss deriving (Eq, Show, Read)

coordLines :: [[Coord]]
coordLines =
  [map (TopBit y) [A ..F] | y <- [A ..F]] ++
  [map (CentralBit y) [A ..F] ++ map (RightBit y) [A .. F] | y <- [A ..F]]

type Board = Map Coord Result
type Board' = Coord -> Result

covering :: Set Coord -> Board -> Maybe Board
covering cs b =
  if (All True, Any True) ==
     flip F.foldMap cs
     (\c -> let x = c `M.lookup` b in
       (All $ x /= Just Miss, Any $ x == Just Hit))
  then Just $ flip appEndo b $ flip F.foldMap cs $ \c -> Endo $ M.insert c Miss
  else Nothing
