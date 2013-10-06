module BattlePi.Placement where

import Control.Applicative
import Control.Monad.Trans.State
import qualified Data.Foldable as F
import qualified Data.Set as S
import System.Random

import BattlePi.Types

place :: StdGen -> (Board', StdGen)
place = runState $ do
  d <- liftA2 (figure Destroyer) randomCoord randomOrientation
  c <- liftA2 (figure Cruiser) randomCoord randomOrientation
  b <- liftA2 (figure Battleship) randomCoord randomOrientation
  h <- liftA2 (figure Hovercraft) randomCoord randomOrientation
  a <- liftA2 (figure AircraftCarrier) randomCoord randomOrientation
  let u = F.fold [d,c,b,h,a]
  case u of
    Just s | S.size s == 21 -> 
      return $ \x -> if x `S.member` s then Hit else Miss
    _ -> state place

randomCoord :: State StdGen Coord
randomCoord = do
  r <- state $ randomR (0,5)
  c <- state $ randomR (0,5)
  w <- state $ randomR (0,2::Int)
  return $ case w of
    0 -> TopBit     (toEnum r) (toEnum c)
    1 -> CentralBit (toEnum r) (toEnum c)
    2 -> RightBit   (toEnum r) (toEnum c)
    
randomOrientation :: State StdGen Orientation
randomOrientation = toEnum <$> state (randomR (0,3))