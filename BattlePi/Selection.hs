module BattlePi.Selection (selectTarget) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import System.Random

import BattlePi.Types

findPossibilities :: Board -> [Map Shape (Coord, Orientation)]
findPossibilities b0 = M.fromList <$> do
  d <- optional allPos
  let bd = f Destroyer d $ Just b0
  guard $ isJust bd
  c <- optional allPos
  let bc = f Cruiser c bd
  guard $ isJust bc
  b <- optional allPos
  let bb = f Battleship b bc
  guard $ isJust bb
  h <- optional allPos
  let bh = f Hovercraft h bb
  guard $ isJust bh
  a <- optional allPos
  let ba = f AircraftCarrier a bh
  guard $ isJust ba
  return
    [(s, p) | (s, Just p) <- zip [Destroyer .. AircraftCarrier] [d,c,b,h,a]]
  where
    f _ Nothing b = b
    f s (Just (co, o)) b = do
      cs <- figure s co o
      covering cs $ fromJust b
    allPos = liftA2 (,) allCs [North, East, South, West]
    allCs = concat $ map ((<$> cs) . uncurry) [TopBit, CentralBit, RightBit]
      where cs = liftA2 (,) [A .. F] [A .. F]

getPoints :: Set Coord -> [Map Shape (Coord, Orientation)] -> Map Coord Int
getPoints as =
  flip foldr M.empty $
  \m0 r -> flip (flip M.foldrWithKey r) m0 $
           \s (p, o) -> case figure s p o of
             Nothing -> id
             Just cs -> \m -> flip (flip S.foldr m) (S.intersection as cs) $
                              \c -> M.insertWith (+) c 1

maxKey :: Map Coord Int -> Maybe (Coord, Int)
maxKey m = flip (flip M.foldrWithKey Nothing) m $
           \c i v -> case compare (Just i) (fmap snd v) of
             GT -> Just (c, i)
             _ -> v

hits :: Board -> Set Coord
hits = M.keysSet . M.filter (== Hit)

nextToAHit :: Board -> Set Coord
nextToAHit b =
  let h = hits b in
  S.filter (`M.notMember` b) $ 
  S.mapMonotonic fromJust $
  S.delete Nothing $
  S.unions $ map (`S.map` h) [above, rightOf, below, leftOf]

targetting :: Map Shape (Coord, Orientation) -> Set Coord
targetting =
  M.foldrWithKey (\s (c, o) cs -> S.union cs $ fromJust $ figure s c o) S.empty

selectTarget11 :: Board -> Maybe Coord
selectTarget11 b =
  fmap fst . maxKey $
  M.filterWithKey (\k _ -> k `M.notMember` b) .
  getPoints (nextToAHit b) . take 1000 .
  filter (\ss -> hits b `S.isSubsetOf` targetting ss)
  $ findPossibilities b

selectTarget12 :: Board -> Maybe Coord
selectTarget12 = fmap fst . S.maxView . nextToAHit

selectTarget1 :: Board -> IO (Maybe Coord)
selectTarget1 b = do
  r0 <- race
        (return $! selectTarget11 b)
        (selectTarget12 b <$ threadDelay (8000000 :: Int))
  return $ either id id r0

{------------------------------------------------------------------------------}

data CombingStrategy =
  Diags

targets :: CombingStrategy -> [Coord]
targets Diags =
  let pat =
        [(A, A), (C, A), (E, A), (B, B), (D, B), (F, B), 
         (A, C), (C, C), (E, C), (B, D), (D, D), (F, D),
         (A, E), (C, E), (E, E), (B, F), (D, F), (F, F)]
  in uncurry <$> [TopBit, CentralBit, RightBit] <*> pat

acceptableTargets :: Board -> CombingStrategy -> [Coord]
acceptableTargets b c = filter (`M.notMember` b) $ targets c

chooseStrategy :: Board -> CombingStrategy
chooseStrategy = const Diags

selectTarget2 :: 
  RandomGen g => Board -> g -> Maybe (Coord, g)
selectTarget2 b g = case acceptableTargets b (chooseStrategy b) of
  [] -> Nothing
  ts -> let n = length ts
            (i, g') = randomR (0, pred n) g
        in Just (ts !! i, g')

{------------------------------------------------------------------------------}

selectTarget :: RandomGen g => Board -> g -> IO (Maybe (Coord, g))
selectTarget b g = do
  a0 <- selectTarget1 b
  case a0 of
    Just a -> return $ Just (a, g)
    Nothing -> return $ selectTarget2 b g

