{-# LANGUAGE OverloadedStrings #-}

module Tui where

import System.Directory

import Brick.AttrMap
import Brick.Main
import Brick.Types

import Brick.Widgets.Core
import Brick.Widgets.Border
import Graphics.Vty.Input.Events
import Data.Monoid ((<>))
import Data.Strings

tui :: IO ()
tui = do
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print endState

data TuiState =
    TuiState
    deriving (Show, Eq)

type ResourceName = String

tuiApp :: App TuiState e ResourceName
tuiApp =
    App
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = pure
        , appAttrMap = const $ attrMap mempty []
        }

buildInitialState :: IO TuiState
buildInitialState = pure TuiState

folder :: String -> Widget n
folder name = str $
    "  ╔═════════╗        \n" <>
    " ║           ╚════╗ \n" <>
    " ║                 ║ \n" <>
    " ║" ++ strPadBoth ' ' 16 name ++ " ║ \n" <>
    " ║                 ║ \n" <>
    " ║                 ║ \n" <>
    " ║                 ║ \n" <>
    "  ╚═══════════════╝ \n" 
ui :: Widget n 
ui = borderWithLabel (str "File Manager") 
    (folder "lol " <=> folder " kek " <=> folder "123") 
drawTui :: TuiState -> [Widget ResourceName]
drawTui _ts = [ ui]

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
    case e of
        VtyEvent vtye ->
            case vtye of
                EvKey (KChar 'q') [] -> halt s
                _ -> continue s
        _ -> continue s
