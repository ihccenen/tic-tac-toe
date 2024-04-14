{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.State (StateT (runStateT), get, put)
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Core.Text
import Raylib.Core.Textures
import Raylib.Types
import Raylib.Util
import Raylib.Util.Colors

data Player where
  X :: Player
  O :: Player
  deriving (Eq)

instance Show Player where
  show X = "Player X"
  show O = "Player O"

data TileState where
  Empty :: TileState
  Has :: Player -> TileState
  deriving (Eq)

data End where
  Draw :: End
  Winner :: Player -> End

instance Show End where
  show Draw = "Draw"
  show (Winner p) = show p <> " wins"

type Board = Vector (Vector TileState)

data GameState where
  GameState
    :: { board :: Board
       , playerTurn :: IORef Player
       , xTexture :: Texture
       , oTexture :: Texture
       , window :: WindowResources
       }
    -> GameState


screenWidth :: (Num a) => a
screenWidth = 800

screenHeight :: (Num a) => a
screenHeight = 600

startup :: IO GameState
startup = do
  w <- initWindow screenWidth screenHeight "tic-tac-toe"
  setTargetFPS 60
  x <- loadRenderTexture 80 80 w
  o <- loadRenderTexture 100 100 w

  beginTextureMode x
  clearBackground black
  drawTriangle (Vector2 8 0) (Vector2 (80 / 2) (80 / 2 - 8)) (Vector2 72 0) gray
  drawTriangle (Vector2 0 8) (Vector2 0 72) (Vector2 (80 / 2 - 8) (80 / 2)) gray
  drawTriangle (Vector2 (80 / 2) (80 / 2 + 8)) (Vector2 8 80) (Vector2 72 80) gray
  drawTriangle (Vector2 80 8) (Vector2 (80 / 2 + 8) (80 / 2)) (Vector2 80 72) gray
  endTextureMode

  beginTextureMode o
  clearBackground gray
  drawRectangleLinesEx (Rectangle 10 10 80 80) 10 black
  endTextureMode

  return $
    GameState Game (Just TwoPlayers) emptyBoard turn (renderTexture'texture x) (renderTexture'texture o) w

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

updateTileState :: IORef Player -> Player -> TileState -> Bool -> IO TileState
updateTileState turn currentPlayer tileState clicked =
  case tileState of
    Empty ->
      if clicked
        then do
          atomicWriteIORef turn (nextPlayer currentPlayer)
          return $ Has currentPlayer
        else return tileState
    _any -> return tileState

clickedRec :: Rectangle -> IO Bool
clickedRec rec_ = do
  pos <- liftIO getMousePosition
  down <- liftIO $ isMouseButtonDown MouseButtonLeft
  return $ down && checkCollisionPointRec pos rec_

inlineCenter :: Float -> Float
inlineCenter z = screenWidth / 2 - z / 2

checkRestart :: StateT GameState IO GameState
checkRestart = do
  s <- get
  z <- liftIO (fromIntegral <$> measureText "Restart" 30 :: IO Float)
  let rec_ = Rectangle (inlineCenter $ z + 20) 520 (z + 20) 40
  liftIO $ drawRectangleRec rec_ gray
  liftIO $ drawText "Restart" (round $ inlineCenter z) 525 30 black
  clicked <- liftIO $ clickedRec rec_
  when clicked $ do
    liftIO $ atomicWriteIORef (playerTurn s) X
    put s {board = emptyBoard}
  return s

checkWin :: Board -> Bool
checkWin board'
  -- horizontals
  | row0 ! 0 /= Empty && row0 ! 0 == row0 ! 1 && row0 ! 0 == row0 ! 2 = True
  | row1 ! 0 /= Empty && row1 ! 0 == row1 ! 1 && row1 ! 0 == row1 ! 2 = True
  | row2 ! 0 /= Empty && row2 ! 0 == row2 ! 1 && row2 ! 0 == row2 ! 2 = True
  -- verticals
  | row0 ! 0 /= Empty && row0 ! 0 == row1 ! 0 && row0 ! 0 == row2 ! 0 = True
  | row0 ! 1 /= Empty && row0 ! 1 == row1 ! 1 && row0 ! 1 == row2 ! 1 = True
  | row0 ! 2 /= Empty && row0 ! 2 == row1 ! 2 && row0 ! 2 == row2 ! 2 = True
  -- diagonals
  | row0 ! 0 /= Empty && row0 ! 0 == row1 ! 1 && row0 ! 0 == row2 ! 2 = True
  | row0 ! 2 /= Empty && row0 ! 2 == row1 ! 1 && row0 ! 2 == row2 ! 0 = True
  | otherwise = False
  where
    row0 = board' ! 0
    row1 = board' ! 1
    row2 = board' ! 2

checkDraw :: Board -> Bool
checkDraw = V.null . V.foldMap (V.filter (== Empty))

getWinner :: Player -> End
getWinner X = Winner O
getWinner O = Winner X

checkGameEnd :: StateT GameState IO (Maybe End)
checkGameEnd = do
  s <- get
  p <- liftIO $ readIORef $ playerTurn s
  let board' = board s
      result
        | checkWin board' = Just $ getWinner p
        | checkDraw board' = Just Draw
        | otherwise = Nothing
  return result

gameText :: StateT GameState IO ()
gameText = do
  s <- get
  player <- liftIO $ readIORef (playerTurn s)
  ended <- checkGameEnd
  let (text, color) = case ended of
        Just result -> (show result, blue)
        Nothing -> (show player <> " turn", black)
  z <- liftIO (fromIntegral <$> measureText text 30 :: IO Float)
  liftIO $ drawText text (round $ inlineCenter z) 50 30 color

game :: StateT GameState IO GameState
game = do
  s <- get
  currentPlayer <- liftIO $ readIORef $ playerTurn s
  let win = checkWin $ board s
      cols :: Int -> Vector TileState -> IO (Vector TileState)
      cols = V.imapM . rows
      rows :: Int -> Int -> TileState -> IO TileState
      rows i j tileState = do
        let x' = (screenWidth / 2 - 50) + (120 * fromIntegral j) - 120
            y = (screenHeight / 2 - 50) + (120 * fromIntegral i) - 120
            rec_ = Rectangle x' y 100 100
        case tileState of
          Empty -> drawRectangleRec rec_ gray
          Has X -> do
            drawRectangleRec rec_ gray
            drawTextureRec (xTexture s) (Rectangle 0 0 80 (-80)) (Vector2 (x' + 10) (y + 10)) white
          Has O -> drawTextureRec (oTexture s) (Rectangle 0 0 100 (-100)) (Vector2 x' y) white
        if win
          then return tileState
          else clickedRec rec_ >>= updateTileState (playerTurn s) currentPlayer tileState
  board' <- liftIO $ V.imapM cols (board s)
  put $ s {board = board'}
  gameText
  checkRestart

mainLoop :: GameState -> IO GameState
mainLoop s =
  drawing
    ( do
        clearBackground rayWhite
        snd <$> runStateT game s
    )

shouldClose :: GameState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: GameState -> IO ()
teardown s = do
  let unloadTexture' texture = unloadTexture (texture s) (window s)
  unloadTexture' xTexture
  unloadTexture' oTexture
  closeWindow $ window s

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)
