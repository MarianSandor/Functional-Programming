-----------------------
-- Sandor Marian
-- 31.02.2020
-----------------------
module Card exposing (Card, cardValue, viewCard, cardToString, deck)

import Html exposing (..)
import Html.Attributes exposing (style)

type Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
type Suit = Spades | Diamonds | Hearts | Clubs
type Card = Card {face: Face, suit: Suit}

faceToString : Face -> String
faceToString face = case face of 
                        Two -> "Two"
                        Three -> "Three"
                        Four -> "Four"
                        Five -> "Five"
                        Six -> "Six"
                        Seven -> "Seven"
                        Eight -> "Eight"
                        Nine -> "Nine"
                        Ten -> "Ten"
                        Jack -> "Jack"
                        Queen -> "Queen"
                        King -> "King"
                        Ace -> "Ace"

suitToString: Suit -> String
suitToString suit = case suit of 
                        Spades -> "Spades"
                        Diamonds -> "Diamonds"
                        Hearts -> "Hearts"
                        Clubs -> "Clubs"

cardToString: Card -> String
cardToString (Card {face, suit}) = (faceToString face) ++ " of " ++ (suitToString suit)

deck : List Card 
deck = let 
            faces = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]
            suits = [Spades, Diamonds, Hearts, Clubs]
            cards = List.map2 Tuple.pair (List.concatMap (\x -> (List.repeat 4 x)) faces) (List.concat (List.repeat 13 suits))
       in
            List.map (\(f, s) -> Card {face = f, suit = s}) cards

cardValue : Card -> List Int
cardValue (Card {face, suit}) = case face of
                                    Two -> [2]
                                    Three -> [3]
                                    Four -> [4]
                                    Five -> [5]
                                    Six -> [6]
                                    Seven -> [7]
                                    Eight -> [8]
                                    Nine -> [9]
                                    Ten -> [10]
                                    Jack -> [10]
                                    Queen -> [10]
                                    King -> [10]
                                    Ace -> [1, 11]

cardToUnicode : Card -> String
cardToUnicode (Card {face, suit}) = 
   case face of
     Ace -> case suit of 
       Spades ->"🂡"
       Hearts -> "🂱"
       Clubs ->  "🃑"
       Diamonds -> "🃁"
     Two -> case suit of 
       Spades ->"🂢"
       Hearts -> "🂲"
       Clubs ->  "🃒"
       Diamonds -> "🃂"
     Three -> case suit of 
       Spades ->"🂣"
       Hearts -> "🂳"
       Clubs ->  "🃓"
       Diamonds ->"🃃" 
     Four -> case suit of 
       Spades ->"🂤"
       Hearts -> "🂴"
       Clubs ->  "🃔"
       Diamonds -> "🃄"
     Five -> case suit of 
       Spades ->"🂥"
       Hearts -> "🂵"
       Clubs ->  "🃕"
       Diamonds -> "🃅"
     Six -> case suit of 
       Spades ->"🂦"
       Hearts -> "🂶"
       Clubs ->  "🃖"
       Diamonds -> "🃆"
     Seven -> case suit of 
       Spades ->"🂧"
       Hearts -> "🂷"
       Clubs ->  "🃗"
       Diamonds -> "🃇"
     Eight -> case suit of 
       Spades -> "🂨"
       Hearts ->  "🂸"
       Clubs ->   "🃘"
       Diamonds ->  "🃈"
     Nine -> case suit of 
       Spades -> "🂩"
       Hearts ->  "🂹"
       Clubs ->   "🃙"
       Diamonds ->  "🃉"
     Ten -> case suit of 
       Spades ->"🂪"
       Hearts -> "🂺"
       Clubs ->  "🃚"
       Diamonds -> "🃊"
     Jack -> case suit of 
       Spades ->"🂫"
       Hearts -> "🂻"
       Clubs ->  "🃛"
       Diamonds -> "🃋"
     Queen -> case suit of 
       Spades ->"🂭"
       Hearts -> "🂽"
       Clubs ->  "🃝"
       Diamonds -> "🃍"
     King -> case suit of 
       Spades -> "🂮"
       Hearts -> "🂾"
       Clubs ->  "🃞"
       Diamonds -> "🃎"

viewCard : Card -> Html msg
viewCard card = 
   let
     (Card {face, suit}) = card
     faceName = faceToString face
     suitName = suitToString suit
     suitColor s = 
       case s of
         Diamonds -> "red"
         Spades -> "black"
         Hearts -> "red"
         Clubs -> "black"
     unicode = cardToUnicode card
   in
     div [style "display" "inline-block"] [
       div [style "font-size" "12em", style "color" (suitColor suit)] [text unicode],
       div [style "font-size" "0.8em"]  [text (cardToString card)]
     ]