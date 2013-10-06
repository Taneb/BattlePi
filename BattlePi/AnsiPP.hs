module BattlePi.AnsiPP where

import qualified Data.Map as M
import Data.Monoid
import Text.PrettyPrint.ANSI.Leijen

import BattlePi.Types

renderBoard :: Board -> [Doc]
renderBoard b =
  flip map coordLines $ \cs ->
  mconcat $ flip map cs $ \c  ->
  case c `M.lookup` b of
    Nothing -> ondullwhite $ dullblack $ char '?'
    Just Hit -> ondullred $ red $ char 'X'
    Just Miss -> ondullblue space
    
renderBoard' :: Board' -> [Doc]
renderBoard' b =
  flip map coordLines $ \cs ->
  mconcat $ flip map cs         $ \c  ->
  case b c of
    Hit -> ondullred $ red $ char 'X'
    Miss -> ondullblue space

renderBoards :: Board -> Board' -> Doc
renderBoards b b' =
  let (x, y) = splitAt 6 $ renderBoard b in
  vcat $ zipWith (<+>) (renderBoard' b') $
  map (indent 6) x ++ y

rFinal :: Board -> Board' -> IO ()
rFinal b b' = do
  putDoc $ renderBoards b b'
  putStrLn ""
