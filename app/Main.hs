{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.State ( get, put, StateT(runStateT) )
import Data.IORef ( atomicWriteIORef, newIORef, readIORef, IORef )
import Data.Vector (Vector)
import Data.Vector qualified as V
import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Core.Text
import Raylib.Types
import Raylib.Util
import Raylib.Util.Colors
import Raylib.Core.Textures

data Player where
  X :: Player
  O :: Player
  deriving Eq

instance Show Player where
  show X = "Player X"
  show O = "Player O"

data TileState where
  Empty :: TileState
  Has :: Player -> TileState
  deriving Eq

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

initialState :: WindowResources -> IO GameState
initialState w = do
  turn <- newIORef X

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

  return $ GameState (V.replicate 3 $ V.replicate 3 Empty) turn (renderTexture'texture x) (renderTexture'texture o) w

screenWidth :: (Num a) => a
screenWidth = 800

screenHeight :: (Num a) => a
screenHeight = 600

startup :: IO GameState
startup = do
  w <- initWindow screenWidth screenHeight "tic-tac-toe"
  setTargetFPS 60  
  initialState w

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

checkTurnLoop :: IORef Player -> Player -> TileState -> Bool -> IO TileState
checkTurnLoop turn currentPlayer tileState clicked =
  case tileState of
    Empty -> if clicked
             then do
               atomicWriteIORef turn (nextPlayer currentPlayer)
               return $ Has currentPlayer
             else return tileState
    _any -> return tileState

drawBoard :: StateT GameState IO GameState
drawBoard = do
  s <- get
  let turn = playerTurn s
  pos <- liftIO getMousePosition
  down <- liftIO $ isMouseButtonDown MouseButtonLeft
  let f :: Int -> Vector TileState -> IO (Vector TileState)
      f i = V.imapM (g i)
      g :: Int -> Int -> TileState -> IO TileState
      g i j tileState = do
        currentPlayer <- readIORef turn
        let clicked = down && checkCollisionPointRec pos rec_
            x' = (screenWidth / 2 - 50) + (120 * fromIntegral i) - 120
            y = (screenHeight / 2 - 50) + (120 * fromIntegral j) - 120
            rec_ =
              Rectangle
              x'
              y
              100
              100
        case tileState of
          Empty -> drawRectangleRec rec_ gray
          Has X -> do
            drawRectangleRec rec_ gray
            drawTextureRec (xTexture s) (Rectangle 0 0 80 (-80)) (Vector2 (x' + 10) (y + 10)) white
          Has O -> drawTextureRec (oTexture s) (Rectangle 0 0 100 (-100)) (Vector2 x' y) white
        checkTurnLoop turn currentPlayer tileState clicked
  board' <- liftIO $ V.imapM f (board s)
  put $ s { board = board' }
  return s

drawTurn :: StateT GameState IO ()
drawTurn = do
  s <- get
  player <- liftIO $ readIORef (playerTurn s)
  let text = show player <> " turn"
  z <- liftIO $ measureText text 30
  liftIO $ drawText text (screenWidth `div` 2 - z `div` 2) 20 30 black

mainLoop :: GameState -> IO GameState
mainLoop s =
  drawing
    ( do
        clearBackground rayWhite
        (_, s') <-
          runStateT
            ( do
                drawTurn
                drawBoard
            )
            s
        return s'
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
