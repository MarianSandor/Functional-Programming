-----------------------
-- Sandor Marian
-- 31.02.2020
-----------------------
module Main exposing (main, calculateScore)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Random
import Debug


import Card exposing (Card)


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model =
  { hand: List Card,
    deck: List Card,
    showDeck: Bool
  }


startingModel : Model
startingModel =
    Model [] Card.deck True


init : () -> (Model, Cmd Msg)
init _ =
  ( startingModel
  , Cmd.none
  )


type Msg
  = Draw
  | NewCard Card
  | ToogleDeck
  | PlayAgain


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Draw ->
      ( model
      , drawCard model
      )
    
    NewCard newCard ->
      ( {model | hand = newCard::model.hand, deck = (List.filter (\x -> x /= newCard) model.deck)}
      , Cmd.none
      )

    ToogleDeck ->
      (
        {model | showDeck = not model.showDeck}
      , Cmd.none
      )

    PlayAgain ->
      (
        {model | hand = [], deck = Card.deck, showDeck = model.showDeck}
      , Cmd.none
      )

drawCard : Model -> Cmd Msg
drawCard model =
  case model.deck of
    (first::rest) -> Random.generate NewCard (Random.uniform first rest)
    _ -> Cmd.none


calculateScore : List Card -> Int
calculateScore cards = let  
                            isBetter a b = case ((compare a 21), (compare b 21)) of
                                                (LT, GT) -> True
                                                (GT, LT) -> False
                                                (EQ, _) -> True
                                                (_, EQ) -> False
                                                (LT, LT) -> (abs (21-a)) < (abs (21-b))
                                                (GT, GT) -> (abs (21-a)) < (abs (21-b))

                            getScores values score scores = case values of
                                                                [] -> score::scores
                                                                x::xs -> case x of
                                                                            [a] -> getScores xs (score+a) scores
                                                                            [a,b] -> (getScores xs (score+a) scores) ++ (getScores xs (score+b) scores)
                                                                            _ ->  getScores xs score scores
                            scoresL = getScores (List.map Card.cardValue cards) 0 []
                       in     
                            List.foldr (\a -> \b -> if (isBetter a b) || b == 0 then a else b) 0 scoresL

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


view : Model -> Html Msg
view model =
  let
    isWin = (calculateScore model.hand) == 21
    isLost = (calculateScore model.hand) > 21

    appName = "Blackjack"

    drawBtn = div [] [ button [ onClick (Draw)] [ text "Draw" ]]
    toggleBtn = div [] [ button [ onClick (ToogleDeck)] [ text "Toggle" ]]
    playAgainBtn = div [] [ button [ onClick (PlayAgain)] [ text "Play again" ]]

    winText = div [] [ h2 [] [text "You won!!"] ]
    loseText = div [] [ h2 [] [text "You lost!!"] ]

    hand = div [] [ div [] [p [] [text "Player's hand:"]]
                  , div [] (List.map Card.viewCard model.hand)
                  , div [] [p [] [text ("Score: "++(String.fromInt (calculateScore model.hand)))]]
                  ]
    deck = div [] [ div [] [p [] [text "Deck:"]]
                  , div [] (List.map Card.viewCard model.deck)
                  ]

  in
    case (model.showDeck, isWin, isLost) of
      (True, False, False)  -> div []
                                [ div [] [ h1 [] [text appName] ]
                                , drawBtn
                                , toggleBtn
                                , hand
                                , deck
                                ]
      (False, False, False) -> div []
                                [ div [] [ h1 [] [text appName] ]
                                , drawBtn
                                , toggleBtn
                                , hand
                                ]
      (False, True, False) -> div []
                                [ div [] [ h1 [] [text appName] ]
                                , toggleBtn
                                , hand
                                , winText
                                , playAgainBtn
                                ]
      (True, True, False) -> div []
                                [ div [] [ h1 [] [text appName] ]
                                , toggleBtn
                                , hand
                                , winText
                                , playAgainBtn
                                , deck
                                ]
      (False, False, True) -> div []
                                [ div [] [ h1 [] [text appName] ]
                                , toggleBtn
                                , hand
                                , loseText
                                , playAgainBtn
                                ]
      (True, False, True) -> div []
                                [ div [] [ h1 [] [text appName] ]
                                , toggleBtn
                                , hand
                                , loseText
                                , playAgainBtn
                                , deck
                                ]
      _ -> div []
              [ div [] [ h1 [] [text appName] ]
              , div [] [ h2 [] [text "Something went wrong!"] ]
              ]