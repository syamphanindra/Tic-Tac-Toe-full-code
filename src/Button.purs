module Button where
import Prelude

import Control.Monad.Eff.Console (logShow, log)
import Data.Array ((!!), updateAt)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (length)
import FFI.Util (property, setProperty)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP



data Turn = X | O
type State =
  { cells :: Array String
  , nextTurn :: Turn
  , gameState :: String
  , furthurPlay :: Boolean
  }
count :: Array Int
count = [0]

data Query a = Toggle Int a|Reset a
type Input = Unit
type Output = Void

myButton :: forall m. H.Component HH.HTML Query Input Output m
myButton =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { cells: [ "", "", "", "", "", "", "", "", "" ]
    , nextTurn: X
    , gameState : "play"
    , furthurPlay : true
    }

  renderButton :: State -> Int -> H.ComponentHTML Query
  renderButton state index =
    HH.div
      [ HP.class_ $ ClassName "button"
      , HE.onClick (HE.input_ $ Toggle index)
      ]
      [ HH.span []
          [ HH.text $ fromMaybe "" $ state.cells !! index
          ]
      ]

  renderButtonsRow :: State -> Int -> H.ComponentHTML Query
  renderButtonsRow state rowIndex =
    HH.div
      [ HP.class_ $ ClassName "buttonsRow"
      ]
      [ renderButton state $ rowIndex * 3
      , renderButton state $ rowIndex * 3 + 1
      , renderButton state $ rowIndex * 3 + 2
      ]

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div [HP.id_ "maindiv"]
    [
      HH.h1 [] [HH.text "TIC TAC TOE"],
      HH.div
        [
          HP.id_ "di"
        ]
        [ renderButtonsRow state 0
        , renderButtonsRow state 1
        , renderButtonsRow state 2
        ],
      HH.br_,
      HH.br_,
      HH.button
        [
          HE.onClick (HE.input_ $ Reset)

        ]
        [
          HH.text "Reset"
        ],
        HH.br_,

      HH.h1 [] [HH.text $ state.gameState]

    ]
  calculateNextTurn :: Turn -> Turn
  calculateNextTurn X = O
  calculateNextTurn O = X

  toString :: Turn -> String
  toString X = "X"
  toString O = "O"

  findPlayer :: Turn -> String
  findPlayer X = "player1"
  findPlayer O = "player2"


  findBool :: Array String -> Int ->  Boolean
  findBool arr index = do
        let r = property arr (show(index))
        if (length r == 0) then
          pure true
          else
            pure false
    true
  findWinner :: Array String -> Int -> Boolean
  findWinner  (arr :: Array String) 0 = do
              let (l0 :: String) = property arr "0"
              let (l1 :: String)= property arr "1"
              let (l2 :: String) = property arr "2"
              let (l3 :: String) = property arr "3"
              let (l6 :: String) = property arr "6"
              let (l4 :: String) = property arr "4"
              let (l8 :: String) = property arr "8"
              if (((l0 == l1)&&(l1==l2))||((l0==l3)&&(l3==l6))||((l0==l4)&&(l4==l8))) then
                true
                else
                 false

  findWinner  arr 1 = do
              let (l1 :: String) = property arr "1"
              let (l4 :: String) = property arr "4"
              let (l7 :: String) = property arr "7"
              let (l0 :: String) = property arr "0"
              let (l1 :: String) = property arr "1"
              let (l2 :: String) = property arr "2"
              if (((l0==l1)&&(l1==l2))||((l1==l4)&&(l4==l7))) then
                true
                else
                 false

  findWinner  arr 2 = do
              let (l0 :: String) = property arr "0"
              let (l1 :: String) = property arr "1"
              let (l2 :: String) = property arr "2"
              let (l4 :: String) = property arr "4"
              let (l6 :: String) = property arr "6"
              let (l5 :: String) = property arr "5"
              let (l8 :: String) = property arr "8"
              if (((l0==l1)&&(l1==l2))||((l2==l5)&&(l5==l8))||((l2==l4)&&(l4==l6))) then
                true
                else
                 false

  findWinner  arr 3 = do
              let (l3 :: String) = property arr "3"
              let (l4 :: String) = property arr "4"
              let (l5 :: String) = property arr "5"
              let (l0 :: String) = property arr "0"
              let (l6 :: String) = property arr "6"
              if (((l3==l4)&&(l4 == l5))||((l0==l3)&&(l3==l6))) then
                true
                else
                 false
  findWinner  arr 4 = do
              let (l0 :: String) = property arr "0"
              let (l1 :: String) = property arr "1"
              let (l2 :: String) = property arr "2"
              let (l3 :: String) = property arr "3"
              let (l4 :: String) = property arr "4"
              let (l5 :: String) = property arr "5"
              let (l6 :: String) = property arr "6"
              let (l7 :: String) = property arr "7"
              let (l8 :: String) = property arr "8"
              if (((l0==l4)&&(l4==l8))||((l2==l4)&&(l4==l6))&&((l3==l4)&&(l4==l5))||((l1==l4)&&(l4==l7))) then
                true
                else
                 false
  findWinner  arr 5 = do
              let (l3 :: String) = property arr "3"
              let (l4 :: String) = property arr "4"
              let (l5 :: String) = property arr "5"
              let (l2 :: String) = property arr "2"
              let (l8 :: String) = property arr "8"
              if (((l3==l4)&&(l4==l5))||((l2==l5)&&(l5==l8))) then
                true
                else
                 false

  findWinner  arr 6 = do
              let (l0 :: String) = property arr "0"
              let (l3 :: String)= property arr "3"
              let (l6 :: String) = property arr "6"
              let (l7 :: String) = property arr "7"
              let (l8 :: String) = property arr "8"
              let (l4 :: String) = property arr "4"
              let (l2 :: String) = property arr "2"
              if (((l6==l7)&&(l7==l8))||((l0==l3)&&(l3==l6))||((l2==l4)&&(l4==l6))) then
                true
                else
                 false
  findWinner  arr 7 = do
              let (l6 :: String) = property arr "6"
              let (l7 :: String) = property arr "7"
              let (l8 :: String) = property arr "8"
              let (l1 :: String) = property arr "1"
              let (l4 :: String) = property arr "4"
              if (((l1==l4)&&(l4==l7))||((l6==l7)&&(l7==l8))) then
                true
                else
                 false
  findWinner  arr 8 = do
              let (l6 :: String) = property arr "6"
              let (l7 :: String) = property arr "7"
              let (l8 :: String) = property arr "8"
              let (l5 :: String) = property arr "5"
              let (l2 :: String) = property arr "2"
              let (l0 :: String) = property arr "0"
              let (l4 :: String) = property arr "4"
              if (((l6==l7)&&(l7==l8))||((l2==l5)&&(l5==l8))||((l0==l4)&&(l4==l8))) then
                true
                else
                 false
  findWinner  arr _ = true


  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Toggle bid next -> do
      state <- H.get
      let (r::String) = show(bid)


      if((findBool state.cells bid)&&state.furthurPlay) then do
        let lp = state.cells
        _ <- pure $ setProperty lp r (toString state.nextTurn)
        H.modify _ { cells = lp ,nextTurn = calculateNextTurn state.nextTurn  }
        let (y :: Int) = property count "0" + 1
        _ <- pure $ setProperty count "0" y
        (l:: Boolean) <- pure $ findWinner state.cells bid
        _ <- if((not l) && y==9) then do
           H.modify _ {gameState = "The game is draw"}
           pure next
             else
               pure next
        _ <- if(l) then do
          H.modify _ {furthurPlay=false,gameState=(findPlayer state.nextTurn)<>", you won"}
          pure next
                 else
                    pure next
        pure next
        else do
          pure next
    Reset next -> do
      state <- H.get
      H.modify _ {cells=["","","","","","","","",""],gameState="play",furthurPlay=true,nextTurn=X}
      _ <- pure $setProperty count "0" 0
      pure next
