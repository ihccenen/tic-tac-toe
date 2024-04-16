{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative (ZipList (ZipList))
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

data Phase where
  Menu :: Phase
  Game :: Phase

data GameMode where
  TwoPlayers :: GameMode
  VsAI :: GameMode

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

type Board = Vector TileState

data GameState where
  GameState
    :: { phase :: Phase
       , mode :: Maybe GameMode
       , singlePlayer :: Player
       , board :: Board
       , playerTurn :: IORef Player
       , xTexture :: Texture
       , oTexture :: Texture
       , window :: WindowResources
       }
    -> GameState

emptyBoard :: Board
emptyBoard = V.replicate 9 Empty

screenWidth :: (Num a) => a
screenWidth = 800

screenHeight :: (Num a) => a
screenHeight = 600

startup :: IO GameState
startup = do
  w <- initWindow screenWidth screenHeight "tic-tac-toe"
  setTargetFPS 60
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

  return $
    GameState
      Menu
      (Just TwoPlayers)
      X
      emptyBoard
      turn
      (renderTexture'texture x)
      (renderTexture'texture o)
      w

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

clickedRec :: Rectangle -> IO Bool
clickedRec rec_ = do
  pos <- getMousePosition
  down <- isMouseButtonDown MouseButtonLeft
  return $ down && checkCollisionPointRec pos rec_

turnUpdate :: Rectangle -> IORef Player -> Player -> TileState -> IO TileState
turnUpdate rec_ turnRef currentPlayer tileState = do
  clicked <- clickedRec rec_
  case tileState of
    Empty ->
      if clicked
        then do
          atomicWriteIORef turnRef (nextPlayer currentPlayer)
          return $ Has currentPlayer
        else return tileState
    _any -> return tileState

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
  | board' ! 0 /= Empty && board' ! 0 == board' ! 1 && board' ! 0 == board' ! 2 = True
  | board' ! 3 /= Empty && board' ! 3 == board' ! 4 && board' ! 3 == board' ! 5 = True
  | board' ! 6 /= Empty && board' ! 6 == board' ! 7 && board' ! 6 == board' ! 8 = True
  -- verticals
  | board' ! 0 /= Empty && board' ! 0 == board' ! 3 && board' ! 0 == board' ! 6 = True
  | board' ! 1 /= Empty && board' ! 1 == board' ! 4 && board' ! 1 == board' ! 7 = True
  | board' ! 2 /= Empty && board' ! 2 == board' ! 5 && board' ! 2 == board' ! 8 = True
  -- diagonals
  | board' ! 0 /= Empty && board' ! 0 == board' ! 4 && board' ! 0 == board' ! 8 = True
  | board' ! 2 /= Empty && board' ! 2 == board' ! 4 && board' ! 2 == board' ! 6 = True
  | otherwise = False

checkDraw :: Board -> Bool
checkDraw = V.null . V.filter (==Empty)

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
  let f :: Int -> TileState -> IO TileState
      f idx tileState = do
        let i = idx `div` 3
            j = idx `rem` 3
            win = checkWin $ board s
            x' = (screenWidth / 2 - 50) + (120 * fromIntegral j) - 120
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
          else turnUpdate rec_ (playerTurn s) currentPlayer tileState
  board' <- liftIO $ V.imapM f (board s)
  put $ s {board = board'}
  gameText
  checkRestart

updateGameStateWhenClicked :: Rectangle -> GameState -> StateT GameState IO ()
updateGameStateWhenClicked rec_ newState = do
  clicked <- liftIO $ clickedRec rec_
  when clicked $ put newState

menu :: StateT GameState IO GameState
menu = do
  s <- get
  tPSize <- liftIO $ measureText "Two Players" 30
  vsAISize <- liftIO $ measureText "Vs AI" 30
  startSize <- liftIO $ measureText "Start" 30
  let gameMode = mode s
      center = inlineCenter 0
      twoPlayersRec = Rectangle (center - fromIntegral (tPSize + 30)) 100 (fromIntegral tPSize + 20) 50
      vsAIRec = Rectangle (center + 10) 100 (fromIntegral vsAISize + 20) 50
      startRec =
        Rectangle (inlineCenter (fromIntegral startSize) - 10) (500 - 10) (fromIntegral startSize + 20) 50
      xRec = Rectangle (center + 20 - 6) (200 - 2) 30 30
      oRec = Rectangle (center + fromIntegral vsAISize - 6) (200 - 2) 30 30
      clickUpdates =
        updateGameStateWhenClicked
          <$> ZipList [startRec, twoPlayersRec, vsAIRec, xRec, oRec]
          <*> ZipList
            [ s {phase = Game}
            , s {mode = Just TwoPlayers}
            , s {mode = Just VsAI}
            , s {singlePlayer = X}
            , s {singlePlayer = O}
            ]
  liftIO $ do
    drawText "Two Players" (round center - (tPSize + 20)) 110 30 black
    drawText "Vs AI" (round center + 20) 110 30 black
    drawRectangleRec startRec green
    drawText "Start" (round $ inlineCenter $ fromIntegral startSize) 500 30 black
    case gameMode of
      Just TwoPlayers -> drawRectangleLinesEx twoPlayersRec 2 black
      Just VsAI -> do
        drawRectangleLinesEx vsAIRec 2 black
        drawText "Player:" (round center + 10) 160 30 black
        drawText "X" (round center + 20) 200 30 black
        drawText "O" (round center + vsAISize) 200 30 black
        case singlePlayer s of
          X -> drawRectangleLinesEx xRec 2 black
          O -> drawRectangleLinesEx oRec 2 black
      Nothing -> return ()

  sequence_ clickUpdates
  return s

mainLoop :: GameState -> IO GameState
mainLoop s =
  drawing
    ( do
        clearBackground rayWhite
        snd
          <$> runStateT
            ( do
                p <- phase <$> get
                case p of
                  Menu -> menu
                  Game -> game
            )
            s
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
