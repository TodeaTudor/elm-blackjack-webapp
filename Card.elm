-----------------------
-- John Doe
-- 31.02.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Card exposing (Suit(..), Face(..), Card(..), cardValue, viewCard, cardToString, deck, cardToUnicode)

import Html exposing (..)
import Html.Attributes exposing (style)

{-
  Replace with your definitions from assignment 1
-}
type Suit = Clubs | Diamonds | Hearts | Spades
type Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type Card = Card Face Suit

suitToString : Suit -> String
suitToString suit =
    case suit of
        Clubs -> "Clubs"
        Diamonds -> "Diamonds"
        Hearts -> "Hearts"
        Spades -> "Spades"

faceToString : Face -> String
faceToString face =
    case face of
        Ace -> "Ace"
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

cardToString : Card -> String
cardToString (Card f s) = faceToString f ++ " of " ++ suitToString s

cardValue : Card -> List Int
cardValue card =
    case card of
            Card Ace _ -> [1, 11]
            Card Two _ -> [2]
            Card Three _ -> [3]
            Card Four _ -> [4]
            Card Five _ -> [5]
            Card Six _ -> [6]
            Card Seven _ -> [7]
            Card Eight _ -> [8]
            Card Nine _ -> [9]
            Card Ten _ -> [10]
            Card Jack _ -> [10]
            Card Queen _ -> [10]
            Card King _ -> [10]

enumFace = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]
deck : List Card
deck =
           let
             generateClubs = List.map (\x -> Card x Clubs) enumFace
             generateDiamonds = List.map (\x -> Card x Diamonds) enumFace
             generateHearts = List.map (\x -> Card x Hearts) enumFace
             generateSpades = List.map (\x -> Card x Spades) enumFace
           in
             [] ++ generateClubs ++ generateSpades ++ generateHearts ++ generateDiamonds

{-
  Modify this function (if needed) to work with your `Card` definition
-}
cardToUnicode : Card -> String
cardToUnicode (Card face suit) =
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
       Spades ->"🂩"
       Hearts -> "🂹"
       Clubs ->  "🃙"
       Diamonds -> "🃉"
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
{-
  Modify this function (if needed) to work with your `Card` definition
-}
viewCard : Card -> Html msg
viewCard card = Debug.todo "Uncomment and complete after you added your implementations"
  -- let
  --   suit = Debug.todo
  --   face = Debug.todo
  --   faceName = faceToString face
  --   suitName = suitToString suit
  --   suitColor s = 
  --     case s of
  --       Diamond -> "red"
  --       Spades -> "black"
  --       Hearts -> "red"
  --       Clubs -> "black"
  --   unicode = cardToUnicode card
  -- in
  --   div [style "display" "inline-block"] [
  --     div [style "font-size" "12em", style "color" (suitColor suit)] [text unicode],
  --     div [style "font-size" "0.8em"]  [text (cardToString card)]
  --   ]