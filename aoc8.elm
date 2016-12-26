module AOC8 exposing (createDisplay, turnOn, rect, flip, rotate, rotateRow, rotateColumn, solvePt2, parseString)

import Array exposing (repeat, length, append, slice, Array)
import Regex exposing (regex, contains, find, HowMany (..))
import Http
import Html exposing (..)
import Json.Decode as Decode


main : Program Never Model Msg
main =
    Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
    { result : Int
    , loading : Bool
    }


init : (Model, Cmd Msg)
init =
    (Model 0 True, fetchInput)



-- UPDATE


type Msg
    = FetchInput
    | InputLoaded (Result Http.Error (List String))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        FetchInput ->
            ({ model | loading = True}, fetchInput)
        
        InputLoaded (Ok strs) ->
            (Model (solvePt1 strs) False, Cmd.none)

        InputLoaded (Err _) ->
            (model, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  h2 [] [text (if model.loading then "Loading..." else (toString model.result))]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


fetchInput : Cmd Msg
fetchInput =
    let 
        url =
            "https://raw.githubusercontent.com/lieberkind/adventofcode2016/master/aoc3.input.json"
    in
        Http.send InputLoaded (Http.get url decodeStringList)


decodeStringList : Decode.Decoder (List String)
decodeStringList =
    (Decode.list Decode.string)



-- AOC8 PT. 2 SOLUTION


type Pixel = On | Off

type alias Display = Array (Array Pixel)    

type Instruction = 
    Rect Int Int
    | RotateRow Int
    | RotateColumn Int
    | Noop


followInstruction : Instruction -> Display -> Display
followInstruction instr disp =
    case instr of
        Rect x y       -> rect x y disp
        RotateRow y    -> rotateRow y disp
        RotateColumn x -> rotateColumn x disp
        Noop           -> disp


rectRegex : Regex.Regex
rectRegex =
    regex "^rect ([0-9]+)x([0-9]+)$"


rotateColumnRegex : Regex.Regex
rotateColumnRegex =
    regex "^rotate column x=([0-9]+) by ([0-9]+)$"


rotateRowRegex : Regex.Regex
rotateRowRegex =
    regex "^rotate row y=([0-9]+) by ([0-9]+)$"


parseString : String -> List Instruction
parseString str =
    if contains rectRegex str then
        find All rectRegex str
        |> List.concatMap .submatches
        |> List.map (Maybe.andThen (String.toInt >> Result.toMaybe))
        |> (\n -> 
            case n of
              [Just a, Just b] -> [Rect a b]
              _                -> [Noop]
        )
    else if contains rotateColumnRegex str then
        find All rotateColumnRegex str
        |> List.concatMap .submatches
        |> List.map (Maybe.andThen (String.toInt >> Result.toMaybe))
        |> (\n -> 
            case n of
              [Just a, Just b] -> List.repeat b (RotateColumn a)
              _                -> [Noop]
        )
    else if contains rotateRowRegex str then
        find All rotateRowRegex str
        |> List.concatMap .submatches
        |> List.map (Maybe.andThen (String.toInt >> Result.toMaybe))
        |> (\n -> 
            case n of
              [Just a, Just b] -> List.repeat b (RotateRow a)
              _                -> [Noop]
        )
    else
        [Noop]


createDisplay : Int -> Int -> Display
createDisplay x y =
    repeat y (repeat x Off)


turnOn : (Int, Int) -> Display -> Display
turnOn position display =
    let
        x = Tuple.first position
        y = Tuple.second position        
        row = Array.get y display    
    in
        case row of
            Just row ->
                let newRow =
                    Array.set x On row
                in
                    Array.set y newRow display

            _ ->
                display


rect : Int -> Int -> Display -> Display
rect x y display =
    let
        positions =
            Array.initialize x (\b -> Array.initialize y (\a -> (b, a)))
            |> Array.foldr Array.append Array.empty
            |> Array.toList
        
        reducer ps disp =
            case ps of
                p :: ps -> reducer ps (turnOn p disp)
                []      -> disp
    in 
        reducer positions display


-- This is really smart, but I probably won't understand it tommorrow
flip : Display -> Display
flip arr =
    let arrayLength =
        (unsafeGet 0 arr |> Array.length)
    in
        Array.initialize
            arrayLength
            (\a -> 
                Array.initialize (length arr) 
                (\b -> 
                    unsafeGet b arr |> (unsafeGet a)
                )
            )


rotateRow : Int -> Display -> Display
rotateRow y display =
    let row =
        Array.get y display
    in
        case row of
            Just row -> Array.set y (rotate row) display
            Nothing -> display


rotateColumn : Int -> Display -> Display
rotateColumn x display =
    flip display
    |> rotateRow x
    |> flip


rotate : Array a -> Array a
rotate arr =
    let
        init = (slice -1 (length arr) arr)
        last = (slice 0 -1 arr)
    in
        append last init


unsafeGet : Int -> Array a -> a
unsafeGet idx arr =
    let element =
        Array.get idx arr
    in
        case element of
            Just elem -> elem
            Nothing -> Debug.crash ("No element at index " ++ toString idx)
    

solvePt2 : Display
solvePt2 =
    input
    |> List.concatMap parseString
    |> List.foldl followInstruction (createDisplay 50 6)