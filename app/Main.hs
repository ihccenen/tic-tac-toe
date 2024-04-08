{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.State
import Data.Vector (Vector)
import Data.Vector qualified as V
import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Core.Text
import Raylib.Types
import Raylib.Util
import Raylib.Util.Colors

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
       , playerTurn :: Player
       , window :: WindowResources
       }
    -> GameState

initialState :: WindowResources -> GameState
initialState = GameState (V.replicate 3 $ V.replicate 3 Empty) X

screenWidth :: (Num a) => a
screenWidth = 800

screenHeight :: (Num a) => a
screenHeight = 600

startup :: IO GameState
startup = do
  w <- initWindow screenWidth screenHeight "tic-tac-toe"
  setTargetFPS 60
  return $ initialState w

drawBoard :: StateT GameState IO GameState
drawBoard = do
  s <- get
  pos <- liftIO getMousePosition
  let f :: Int -> Vector TileState -> IO (Vector TileState)
      f i = V.imapM (g i)
      g :: Int -> Int -> TileState -> IO TileState
      g i j tileState = do
        let rec_ =
              Rectangle
              ((screenWidth / 2 - 50) + (120 * fromIntegral i) - 120)
              ((screenHeight / 2 - 50) + (120 * fromIntegral j) - 120)
              100
              100
        drawRectangleRec rec_ (if checkCollisionPointRec pos rec_ then gray else lightGray)
        return tileState
  board' <- liftIO $ V.imapM f (board s)
  put $ s { board = board' }
  return s

drawTurn :: StateT GameState IO ()
drawTurn = do
  s <- get
  let text = show (playerTurn s) <> " turn"
  x <- liftIO $ measureText text 30
  liftIO $ drawText text (screenWidth `div` 2 - x `div` 2) 20 30 black

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
teardown = closeWindow . window

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)
