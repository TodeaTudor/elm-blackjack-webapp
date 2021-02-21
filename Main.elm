-----------------------
-- John Doe
-- 31.02.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Main exposing (main, calculateScore)

import Browser
import Html exposing (..)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (..)
import Random
import Debug


import Card exposing (..)

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
  | ResetDeck

updateDeck: List Card -> Card -> List Card
updateDeck deck cardToRemove = List.filter (\x -> x /= cardToRemove) deck

updateShowDeck: Bool -> Bool
updateShowDeck showDeck =
    case showDeck of
        True -> False
        False -> True

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Draw ->
      ( model
      , drawCard model
      )
    
    -- Add the new card to the player's hand (`hand`) and remove it from the `deck`
    NewCard newCard ->
      ( Model (newCard::model.hand) (updateDeck model.deck newCard) model.showDeck
      , Cmd.none
      )

    -- Toggle (if it's `True` set it `False`, if it's `False` set it to `True`) the `showDeck` member of the `Model`
    ToogleDeck ->
      (
       Model model.hand model.deck (updateShowDeck model.showDeck)
      , Cmd.none
      )

    -- I added the ResetDeck value just so I can add a button to reset the game
    ResetDeck ->
       ( Model [] Card.deck model.showDeck
       , Cmd.none
       )

drawCard : Model -> Cmd Msg
drawCard model =
  case model.deck of
    (first::rest) -> Random.generate NewCard (Random.uniform first rest)
    _ -> Cmd.none

{-
  1. Get the value of each card (use `cardValue`)
  2. Generate a list of all possible scores
  3. Return the score closest to 21 (less than 21!), if one exists, else the smallest score over 21
  ```elm
  calculateScore [Card King Hearts] == 10
  calculateScore [Card Two Hearts] == 2
  calculateScore [Card Two Hearts, Card King Spades] == 12
  calculateScore [Card Ace Hearts, Card King Spades] == 21
  calculateScore [Card Ace Hearts, Card Five Hearts, Card Seven Spades] == 13
  calculateScore [Card King Hearts, Card Five Hearts, Card Seven Spades] == 22
  calculateScore [Card King Hearts, Card Ten Clubs, Card Ace Spades] == 21
  calculateScore [Card Ace Spades, Card Ace Clubs, Card Ten Clubs, Card King Clubs] == 22
  ```
-}

{-
  Generates a list of all the possible scores determined by a certain number of Aces(since the Aces take the value 1 or 11)
-}
partitionAces: Int -> Int  -> List Int
partitionAces nbOf1Aces nbOf11Aces = if nbOf11Aces < 0
                                        then []
                                     else
                                        [nbOf1Aces * 1 + nbOf11Aces * 11] ++ partitionAces (nbOf1Aces + 1) (nbOf11Aces - 1)

{-

-}
calculateScore : List Card -> Int
calculateScore cards =
    let
        {-
          This function computes the number of aces in the list of cards and a list of scores for all the other cards that
          are not aces
        -}
        getScoreAndAces: Int -> List Card -> List Int -> (Int, List Int)
        getScoreAndAces numOfAces cardList score =
             case cardList of
                [] -> (numOfAces, score)
                x::xs -> case x of
                            Card Ace _ -> getScoreAndAces (numOfAces + 1) xs score
                            Card _ _ -> getScoreAndAces numOfAces xs (score ++ Card.cardValue x)

        {-
          We get the number of aces and the total score without aces, then we create a sorted list with all the possible
          scores by taking all the possible combinations of aces values in a list.
        -}
        (aces, scoreList) = getScoreAndAces 0 cards []
        scoreWithoutAces = List.sum scoreList
        possibleScores = List.sort (List.map (\x -> x + scoreWithoutAces) (partitionAces 0 aces))

    in
        case possibleScores of
            [] -> 0
            --If there are no aces, there is only one possible score and we return that.
            x::_ -> if aces == 0
                        then x
                   else
                        --If there are any aces, we get the scores that are lower than 21 and pick the one closest one to 21
                        --(since the list is sorted, we just reverse the list with scores <= 21 and pick the head). If there are
                        --no scores lower than 21, we pick the minimum from the scores larger than 21
                        case List.reverse (List.filter (\y -> y <= 21) possibleScores) of
                            [] -> case List.minimum (List.sort possibleScores) of
                                        Nothing -> 0
                                        Just number -> number
                            z::_ -> z


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

{-
  For the following two functions I did not pay enough attention and did not see the function viewCard below cardToUnicode
  in Card.elm, so I implemented that in my own way. I made the function printBlackCards that generates a string with the
  black cards and a function printRedCards that does the same with the red cards.
-}
printBlackCard:List Card -> String
printBlackCard cards =
    case cards of
        [] -> ""
        x::xs -> case x of
                    Card _ Spades -> Card.cardToUnicode x ++ " " ++ printBlackCard xs
                    Card _ Clubs -> Card.cardToUnicode x ++ " " ++ printBlackCard xs
                    Card _ _ -> printBlackCard xs

printRedCard:List Card -> String
printRedCard cards =
    case cards of
        [] -> ""
        x::xs -> case x of
                    Card _ Hearts -> Card.cardToUnicode x ++ " " ++ printRedCard xs
                    Card _ Diamonds -> Card.cardToUnicode x ++ " " ++ printRedCard xs
                    Card _ _ -> printRedCard xs

-- Finally, I think the view is self explanatory
view : Model -> Html Msg
view model =
  let
    appName = "Blackjack"
  in
    div []
      [ div [ style "font-size" "2em" ] [ h1 [] [text appName] ]
      , button [ onClick Draw, disabled ((calculateScore model.hand) >= 21) ] [ text "Draw" ]
      , pre [] [text "\n"]
      , span [ style "font-size" "4em" ] [ text ("Your hand: " ++ printBlackCard model.hand) ]
      , span [ style "font-size" "4em", style "color" "red" ] [ text (printRedCard model.hand) ]
      , pre [] [text "\n"]
      , button [ onClick ToogleDeck] [ text "Show Deck" ]
      , if model.showDeck == True
                then div [] [span [ style "font-size" "4em" ] [ text ("Your deck: " ++ printBlackCard model.deck) ],
                      span [ style "font-size" "4em", style "color" "red" ] [ text (printRedCard model.deck)]]
        else
                div [] []
      , div [ style "font-size" "4em" ] [ text ("Your score is: " ++ String.fromInt (calculateScore model.hand)) ]
      , if ((calculateScore model.hand) > 21)
                then div [ style "font-size" "4em", style "color" "brown" ] [ text ("You lose") ]
        else if ((calculateScore model.hand) == 21)
                then div [ style "font-size" "6em", style "color" "pink" ] [ text ("You win") ] else div [] []
      , button [ onClick ResetDeck] [ text "Reset" ]
      ]