module BattlePi.Loop (battle) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Monad.State.Strict
import Control.Monad.Reader
import qualified Data.Map as M
import System.Random

import BattlePi.AnsiPP
import BattlePi.Selection
import BattlePi.Types

type BattlePlan = StateT (Board, StdGen) (ReaderT Board' IO)

display :: BattlePlan ()
display = do
  b' <- ask
  (b, _) <- get
  liftIO $ rFinal b b'

attack ::
  Async (Maybe (Coord, StdGen)) -> BattlePlan (Async (Maybe (Coord, StdGen)))
attack ac = do
  target <- liftIO $ wait ac
  case target of
    Nothing -> liftIO $ putStrLn "I'm finished!"
    Just (t, g') -> do
      (b, _) <- get
      liftIO $ print t
      r <- getResult
      put (M.insert t r b, g')
  (b, g) <- get
  liftIO $ async $ selectTarget b g 
  where
    getResult :: BattlePlan Result
    getResult = do
      input <- reads <$> liftIO getLine
      case input of
        [(r,_)] -> return r
        _ -> liftIO (putStrLn "Could not parse") >> getResult

defend :: BattlePlan ()
defend = do
  c <- getInput
  reader ($ c) >>= liftIO . print
  where
    getInput :: BattlePlan Coord
    getInput = do
      liftIO $ putStrLn "Input coord"
      input <- reads <$> liftIO getLine
      case input of
        [(c,_)] -> return c
        _ -> liftIO (putStrLn "CouldNotParse") >> getInput

startAttack :: BattlePlan (Async (Maybe (Coord, StdGen)))
startAttack = do
  (b, g) <- get
  liftIO $ async $ selectTarget b g

battlePlan' :: Bool -> BattlePlan ()
battlePlan' b = do
  ac <- startAttack
  display
  loop b ac
  where
    loop True ac = attack ac >>= loop False
    loop False ac = display >> defend >> loop True ac

battle :: Bool -> Board' -> StdGen -> IO ()
battle p b g = 
  flip runReaderT b . 
  flip evalStateT (M.empty, g) $ 
  battlePlan' p
