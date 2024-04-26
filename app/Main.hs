{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative (ZipList (ZipList))
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.State (StateT (runStateT), get, put)
import Data.Maybe (isNothing)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.Vector (Vector, (!), (//))
import Data.Vector qualified as V
import Raylib.Core
  ( beginTextureMode
  , clearBackground
  , closeWindow
  , endTextureMode
  , getMousePosition
  , initWindow
  , isMouseButtonPressed
  , setTargetFPS
  , windowShouldClose
  )
import Raylib.Core.Shapes
  ( checkCollisionPointRec
  , drawLineEx
  , drawRectangleLinesEx
  , drawRectangleRec
  )
import Raylib.Core.Text (drawText, measureText)
import Raylib.Core.Textures
  ( drawTextureRec
  , loadRenderTexture
  , unloadTexture
  )
import Raylib.Types
  ( MouseButton (MouseButtonLeft)
  , Rectangle (Rectangle)
  , RenderTexture (renderTexture'texture)
  , Texture
  , Vector2 (Vector2)
  )
import Raylib.Util (WindowResources, drawing, raylibApplication)
import Raylib.Util.Colors
  ( black
  , blue
  , gray
  , green
  , rayWhite
  , red
  , violet
  , white
  )
import System.Random
  ( Random (randomR)
  , StdGen
  , mkStdGen
  , randomIO
  )

data Phase where
  Menu :: Phase
  Game :: Phase

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

data Line where
  Line :: {start :: Int, end :: Int, startPos :: Vector2, endPos :: Vector2} -> Line
  deriving (Eq)

data MatchStatus where
  Ongoing :: MatchStatus
  Draw :: MatchStatus
  Winner :: Player -> Line -> MatchStatus
  deriving (Eq)

type Board = Vector TileState

data GameState where
  GameState
    :: { phase :: Phase
       , singlePlayer :: Maybe Player
       , nextAIPlayTime :: Maybe UTCTime
       , board :: Board
       , playerTurn :: Player
       , matchStatus :: MatchStatus
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

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

clickedRec :: Rectangle -> IO Bool
clickedRec rec_ = do
  pos <- getMousePosition
  down <- isMouseButtonPressed MouseButtonLeft
  return $ down && checkCollisionPointRec pos rec_

inlineCenter :: Float -> Float
inlineCenter z = screenWidth / 2 - z / 2

checkWin :: Board -> Player -> Maybe (Player, (Int, Int))
checkWin board' player
  -- horizontals
  | board' ! 0 == Has player && board' ! 0 == board' ! 1 && board' ! 0 == board' ! 2 = Just (player, (0, 2))
  | board' ! 3 == Has player && board' ! 3 == board' ! 4 && board' ! 3 == board' ! 5 = Just (player, (3, 5))
  | board' ! 6 == Has player && board' ! 6 == board' ! 7 && board' ! 6 == board' ! 8 = Just (player, (6, 8))
  -- verticals
  | board' ! 0 == Has player && board' ! 0 == board' ! 3 && board' ! 0 == board' ! 6 = Just (player, (0, 6))
  | board' ! 1 == Has player && board' ! 1 == board' ! 4 && board' ! 1 == board' ! 7 = Just (player, (1, 7))
  | board' ! 2 == Has player && board' ! 2 == board' ! 5 && board' ! 2 == board' ! 8 = Just (player, (2, 8))
  -- diagonals
  | board' ! 0 == Has player && board' ! 0 == board' ! 4 && board' ! 0 == board' ! 8 = Just (player, (0, 8))
  | board' ! 2 == Has player && board' ! 2 == board' ! 4 && board' ! 2 == board' ! 6 = Just (player, (2, 6))
  | otherwise = Nothing

checkDraw :: Board -> Bool
checkDraw = V.null . V.filter (== Empty)

getGridPos :: Int -> (Float, Float)
getGridPos idx = (x, y)
  where
    i = idx `div` 3
    j = idx `rem` 3
    x = (screenWidth / 2 - 50) + (120 * fromIntegral j) - 120
    y = (screenHeight / 2 - 50) + (120 * fromIntegral i) - 120

startLine :: (Int, Int) -> Vector2
startLine (start', end') = Vector2 x' y'
  where
    (x, y) = getGridPos start'
    (x', y') = case end' - start' of
      2 -> (x, y + 50)
      6 -> (x + 50, y)
      4 -> (x + 100, y)
      _ -> (x, y)

gameEnd :: StateT GameState IO ()
gameEnd = do
  s <- get
  let board' = board s
      mkLine se@(x, y) = Line x y (startLine se) (startLine se)
  case checkWin board' (nextPlayer $ playerTurn s) of
    Nothing -> when (checkDraw board') $ put $ s {matchStatus = Draw}
    Just (p, x) -> put $ s {matchStatus = Winner p (mkLine x)}

drawEndLine :: StateT GameState IO ()
drawEndLine = do
  s <- get
  case matchStatus s of
    (Winner p line@(Line start' end' startPos' endPos'@(Vector2 endPosX endPosY))) -> do
      liftIO $ drawLineEx startPos' endPos' 5 violet
      let newEndPos = case end' - start' of
            2 -> if endPosX > 567 then endPos' else Vector2 (endPosX + 10) endPosY
            6 -> if endPosY > 467 then endPos' else Vector2 endPosX (endPosY + 10)
            4 -> if endPosY > 467 then endPos' else Vector2 (endPosX - 10) (endPosY + 10)
            _ -> if endPosX > 567 then endPos' else Vector2 (endPosX + 10) (endPosY + 10)
      put $ s {matchStatus = Winner p (line {endPos = newEndPos})}
    _any -> return ()

gameText :: StateT GameState IO ()
gameText = do
  s <- get
  let singlePlayerTurn = case singlePlayer s of
        Nothing -> show (playerTurn s) <> " turn"
        Just p -> (if p == playerTurn s then "Your Turn, " else "AI turn, ") <> show (playerTurn s)
      playerWinText w = case singlePlayer s of
        Just p -> (if w == p then "You win, " else "AI win, ") <> show w
        Nothing -> show w <> " wins"
      (text, color) = case matchStatus s of
        Ongoing -> (singlePlayerTurn, black)
        Draw -> ("Draw", blue)
        Winner w _ -> (playerWinText w, violet)
  z <- liftIO (fromIntegral <$> measureText text 30 :: IO Float)
  liftIO $ drawText text (round $ inlineCenter z) 50 30 color

randomMove :: StateT GameState IO ()
randomMove = do
  s <- get
  now <- Just <$> liftIO getCurrentTime
  let ai = playerTurn s
      gen = generator s
      empty = V.filter ((== Empty) . snd) $ V.indexed $ board s
      (i, nextGen) = randomR (0, V.length empty - 1) gen
  unless (now < nextAIPlayTime s || Ongoing /= matchStatus s || isNothing (singlePlayer s) || Just ai == singlePlayer s || V.null empty) $ do
    put $
      s
        { board = board s // [(fst $ empty ! i, Has ai)]
        , playerTurn = nextPlayer ai
        , generator = nextGen
        }
    gameEnd

restartGame :: StateT GameState IO ()
restartGame = do
  s <- get
  z <- liftIO (fromIntegral <$> measureText "Restart" 30 :: IO Float)
  let center = inlineCenter 0
      rec_ = Rectangle (center - z - 30) 520 (z + 20) 40
  liftIO $ drawRectangleRec rec_ gray
  liftIO $ drawText "Restart" (round $ center - z - 20) 525 30 black
  clicked <- liftIO $ clickedRec rec_
  when clicked $ do
    put s {board = emptyBoard, playerTurn = X, matchStatus = Ongoing}

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
    gen <- liftIO randomIO
    put
      s
        { phase = Menu
        , singlePlayer = Nothing
        , board = emptyBoard
        , playerTurn = X
        , matchStatus = Ongoing
        , generator = mkStdGen gen
        }
  return s

drawBoard :: StateT GameState IO (Vector Rectangle)
drawBoard = do
  s <- get
  let f idx tileState = do
        let (x, y) = getGridPos idx
            rec_ = Rectangle x y 100 100
        case tileState of
          Empty -> drawRectangleRec rec_ gray
          Has X -> do
            drawRectangleRec rec_ gray
            drawTextureRec (xTexture s) (Rectangle 0 0 80 (-80)) (Vector2 (x + 10) (y + 10)) white
          Has O -> drawTextureRec (oTexture s) (Rectangle 0 0 100 (-100)) (Vector2 x y) white
        return rec_
  liftIO $ V.imapM f (board s)

play :: Vector Rectangle -> Vector2 -> StateT GameState IO ()
play recs_ point = do
  s <- get
  down <- liftIO $ isMouseButtonPressed MouseButtonLeft
  let currentPlayer = playerTurn s
      board' = board s
      f i idx rec_
        | down && checkCollisionPointRec point rec_ =
            case board' ! idx of
              Empty -> Just idx
              _any -> i
        | otherwise = i
  when (Ongoing == matchStatus s && (isNothing (singlePlayer s) || (Just (playerTurn s) == singlePlayer s))) $ case V.ifoldl' f Nothing recs_ of
    Nothing -> return ()
    Just idx -> do
      now <- liftIO getCurrentTime
      let n = addUTCTime 1 now
      put $ s {board = board' // [(idx, Has currentPlayer)], playerTurn = nextPlayer currentPlayer, nextAIPlayTime = n <$ nextAIPlayTime s}
      gameEnd

game :: StateT GameState IO GameState
game = do
  recs <- drawBoard
  pos <- liftIO getMousePosition
  play recs pos
  randomMove
  gameText
  drawEndLine
  restartGame
  goToMenu

updateGameStateWhenClicked :: Rectangle -> GameState -> StateT GameState IO ()
updateGameStateWhenClicked rec_ newState = do
  clicked <- liftIO $ clickedRec rec_
  when clicked $ put newState

menu :: StateT GameState IO GameState
menu = do
  s <- get
  tPSize <- liftIO $ fromIntegral <$> measureText "Two Players" 30
  vsAISize <- liftIO $ fromIntegral <$> measureText "Vs AI" 30
  startSize <- liftIO $ fromIntegral <$> measureText "Start" 30
  now <- liftIO getCurrentTime
  let center = inlineCenter 0
      twoPlayersRec = Rectangle (center - (tPSize + 30)) 100 (tPSize + 20) 50
      vsAIRec = Rectangle (center + 10) 100 (vsAISize + 20) 50
      startRec =
        Rectangle (center - startSize - 30) 490 (startSize + 20) 40
      xRec = Rectangle (center + 20 - 6) (200 - 2) 30 30
      oRec = Rectangle (center + vsAISize - 6) 198 30 30
      clickUpdates = do
        updateGameStateWhenClicked
          <$> ZipList [startRec, twoPlayersRec, vsAIRec, xRec, oRec]
          <*> ZipList
            [ s {phase = Game}
            , s {singlePlayer = Nothing, nextAIPlayTime = Nothing}
            , s {singlePlayer = Just X, nextAIPlayTime = Just now}
            , s {singlePlayer = Just X, nextAIPlayTime = Just now}
            , s {singlePlayer = Just O, nextAIPlayTime = Just now}
            ]
  liftIO $ do
    drawText "Two Players" (round $ center - (tPSize + 20)) 110 30 black
    drawText "Vs AI" (round center + 20) 110 30 black
    drawRectangleRec startRec green
    drawText "Start" (round $ center - startSize - 20) 495 30 black
    case singlePlayer s of
      Nothing -> drawRectangleLinesEx twoPlayersRec 2 black
      Just p -> do
        drawRectangleLinesEx vsAIRec 2 black
        drawText "Player:" (round center + 10) 160 30 black
        drawText "X" (round center + 20) 200 30 black
        drawText "O" (round $ center + vsAISize) 200 30 black
        case p of
          X -> drawRectangleLinesEx xRec 2 black
          O -> drawRectangleLinesEx oRec 2 black
  sequence_ clickUpdates
  return s

startup :: IO GameState
startup = do
  w <- initWindow screenWidth screenHeight "tic-tac-toe"
  setTargetFPS 60
  gen <- randomIO
  x <- loadRenderTexture 80 80 w
  o <- loadRenderTexture 100 100 w

  beginTextureMode x
  clearBackground gray
  drawLineEx (Vector2 0 0) (Vector2 80 80) 10 black
  drawLineEx (Vector2 80 0) (Vector2 0 80) 10 black
  endTextureMode

  beginTextureMode o
  clearBackground gray
  drawRectangleLinesEx (Rectangle 10 10 80 80) 10 black
  endTextureMode

  return $
    GameState
      Menu
      Nothing
      Nothing
      emptyBoard
      X
      Ongoing
      (mkStdGen gen)
      (renderTexture'texture x)
      (renderTexture'texture o)
      w

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
shouldClose s = do
  z <- fromIntegral <$> measureText "Exit" 30
  let center = inlineCenter 0
      (x, y) = case phase s of
        Menu -> (inlineCenter z + z - 10, 490)
        Game -> (center + 175, 520)
      rec_ = Rectangle x y (z + 20) 40
  drawRectangleRec rec_ red
  drawText "Exit" (round x + 10) (round y + 5) 30 black
  clicked <- clickedRec rec_
  (|| clicked) <$> windowShouldClose

teardown :: GameState -> IO ()
teardown s = do
  let unloadTexture' texture = unloadTexture (texture s) (window s)
  unloadTexture' xTexture
  unloadTexture' oTexture
  closeWindow $ window s

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)
