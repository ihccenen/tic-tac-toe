{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative (ZipList (ZipList))
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.State (StateT (runStateT), get, put)
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.Maybe (isNothing)
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Core.Text
import Raylib.Core.Textures
import Raylib.Types
import Raylib.Util
import Raylib.Util.Colors
import System.Random

data Phase where
  Menu :: Phase
  Game :: Phase

data GameMode where
  TwoPlayers :: GameMode
  VsAI :: GameMode
  deriving (Eq)

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
  deriving (Eq)

instance Show End where
  show Draw = "Draw"
  show (Winner p) = show p <> " wins"

type Board = Vector TileState

data GameState where
  GameState
    :: { phase :: Phase
       , mode :: GameMode
       , singlePlayer :: Player
       , board :: Board
       , playerTurn :: IORef Player
       , generator :: StdGen
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
  gen <- randomIO
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
      TwoPlayers
      X
      emptyBoard
      turn
      (mkStdGen gen)
      (renderTexture'texture x)
      (renderTexture'texture o)
      w

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

clickedRec :: Rectangle -> IO Bool
clickedRec rec_ = do
  pos <- getMousePosition
  down <- isMouseButtonPressed MouseButtonLeft
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

checkWin :: Board -> Player -> Maybe End
checkWin board' player
  -- horizontals
  | board' ! 0 == Has player && board' ! 0 == board' ! 1 && board' ! 0 == board' ! 2
      || board' ! 3 == Has player && board' ! 3 == board' ! 4 && board' ! 3 == board' ! 5
      || board' ! 6 == Has player && board' ! 6 == board' ! 7 && board' ! 6 == board' ! 8
      ||
      -- verticals
      board' ! 0 == Has player && board' ! 0 == board' ! 3 && board' ! 0 == board' ! 6
      || board' ! 1 == Has player && board' ! 1 == board' ! 4 && board' ! 1 == board' ! 7
      || board' ! 2 == Has player && board' ! 2 == board' ! 5 && board' ! 2 == board' ! 8
      ||
      -- diagonals
      board' ! 0 == Has player && board' ! 0 == board' ! 4 && board' ! 0 == board' ! 8
      || board' ! 2 == Has player && board' ! 2 == board' ! 4 && board' ! 2 == board' ! 6 =
      Just $ Winner player
  | otherwise = Nothing

checkDraw :: Board -> Bool
checkDraw = V.null . V.filter (== Empty)

getWinner :: Player -> End
getWinner X = Winner X
getWinner O = Winner X

checkGameEnd :: StateT GameState IO (Maybe End)
checkGameEnd = do
  s <- get
  p <- liftIO $ readIORef $ playerTurn s
  let board' = board s
      result
        | Just winner <- checkWin board' (nextPlayer p) = Just winner
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

randomMove :: StateT GameState IO ()
randomMove = do
  s <- get
  let ai = playerTurn s
      gen = generator s
      empty :: Vector (Int, TileState)
      empty = V.filter ((== Empty) . snd) $ V.indexed $ board s
      (i, nextGen) = randomR (0, V.length empty - 1) gen
  unless (V.null empty) $ do
    currentPlayer <- liftIO $ readIORef ai
    liftIO $ atomicWriteIORef ai (nextPlayer currentPlayer)
    put $
      s
        { board = V.update (board s) (V.singleton (fst $ empty ! i, Has currentPlayer))
        , generator = nextGen
        }

checkRestart :: StateT GameState IO ()
checkRestart = do
  s <- get
  z <- liftIO (fromIntegral <$> measureText "Restart" 30 :: IO Float)
  let center = inlineCenter 0
      rec_ = Rectangle (center - z - 30) 520 (z + 20) 40
  liftIO $ drawRectangleRec rec_ gray
  liftIO $ drawText "Restart" (round $ center - z - 20) 525 30 black
  clicked <- liftIO $ clickedRec rec_
  when clicked $ do
    liftIO $ atomicWriteIORef (playerTurn s) X
    put s {board = emptyBoard}

goToMenu :: StateT GameState IO GameState
goToMenu = do
  s <- get
  z <- liftIO (fromIntegral <$> measureText "Menu" 30 :: IO Float)
  let center = inlineCenter 0
      rec_ = Rectangle (center + 10) 520 (z + 20) 40
  liftIO $ drawRectangleRec rec_ gray
  liftIO $ drawText "Menu" (round center + 20) 525 30 black
  clicked <- liftIO $ clickedRec rec_
  when clicked $ do
    liftIO $ atomicWriteIORef (playerTurn s) X
    gen <- liftIO randomIO
    put s {phase = Menu, mode = TwoPlayers, board = emptyBoard, generator = mkStdGen gen}
  return s

game :: StateT GameState IO GameState
game = do
  s <- get
  currentPlayer <- liftIO $ readIORef $ playerTurn s
  let lose = checkWin (board s) (nextPlayer currentPlayer)
      f :: Int -> TileState -> IO TileState
      f idx tileState = do
        let i = idx `div` 3
            j = idx `rem` 3
            x' = (screenWidth / 2 - 50) + (120 * fromIntegral j) - 120
            y = (screenHeight / 2 - 50) + (120 * fromIntegral i) - 120
            rec_ = Rectangle x' y 100 100
        case tileState of
          Empty -> drawRectangleRec rec_ gray
          Has X -> do
            drawRectangleRec rec_ gray
            drawTextureRec (xTexture s) (Rectangle 0 0 80 (-80)) (Vector2 (x' + 10) (y + 10)) white
          Has O -> drawTextureRec (oTexture s) (Rectangle 0 0 100 (-100)) (Vector2 x' y) white
        case lose of
          Just _ -> return tileState
          _any -> turnUpdate rec_ (playerTurn s) currentPlayer tileState
  when (isNothing lose && mode s == VsAI && currentPlayer /= singlePlayer s) randomMove
  s' <- get
  board' <- liftIO $ V.imapM f (board s')
  put $ s' {board = board'}
  gameText
  checkRestart
  goToMenu

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
            , s {mode = TwoPlayers}
            , s {mode = VsAI}
            , s {singlePlayer = X}
            , s {singlePlayer = O}
            ]
  liftIO $ do
    drawText "Two Players" (round center - (tPSize + 20)) 110 30 black
    drawText "Vs AI" (round center + 20) 110 30 black
    drawRectangleRec startRec green
    drawText "Start" (round $ inlineCenter $ fromIntegral startSize) 500 30 black
    case gameMode of
      TwoPlayers -> drawRectangleLinesEx twoPlayersRec 2 black
      VsAI -> do
        drawRectangleLinesEx vsAIRec 2 black
        drawText "Player:" (round center + 10) 160 30 black
        drawText "X" (round center + 20) 200 30 black
        drawText "O" (round center + vsAISize) 200 30 black
        case singlePlayer s of
          X -> drawRectangleLinesEx xRec 2 black
          O -> drawRectangleLinesEx oRec 2 black

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
