module AOC8 exposing (createDisplay, turnOn, rect, flip, rotate, rotateRow, rotateColumn, parseString)

import Array exposing (repeat, length, append, slice, Array)
import Regex exposing (regex, contains, find, HowMany (..))
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Time exposing (Time, millisecond)


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
    { display : Display
    , instructions : List Instruction
    , loading : Bool
    , playing : Bool
    }


init : (Model, Cmd Msg)
init =
    (Model (createDisplay 50 6) [] True False, fetchInput)



-- UPDATE


type Msg
    = FetchInput
    | InputLoaded (Result Http.Error (List String))
    | NextInstruction (List Instruction)
    | AllInstructions (List Instruction)
    | TogglePlay
    | Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        FetchInput ->
            ({ model | loading = True}, fetchInput)
        
        InputLoaded (Ok strs) ->
            (Model model.display (parseInput strs) False False, Cmd.none)

        InputLoaded (Err _) ->
            (model, Cmd.none)
        
        NextInstruction instrs ->
            case instrs of
                instr :: instrs ->
                    let newModel =
                        { model | 
                            display = (followInstruction instr model.display),
                            instructions = instrs,
                            playing = False
                        }
                    in
                        (newModel, Cmd.none)

                _ ->
                    ({ model | playing = False}, Cmd.none)
        
        AllInstructions instrs ->
            let newModel =
                { model | 
                    display = (followInstructions instrs model.display),
                    instructions = [],
                    playing = False
                }
            in
                (newModel, Cmd.none)
        
        TogglePlay ->
            ({ model | playing = not model.playing }, Cmd.none)
        
        Tick _ ->
            case model.instructions of
                instr :: instrs ->
                    let newModel =
                        { model |
                            display = (followInstruction instr model.display),
                            instructions = instrs
                        }
                    in
                        (newModel, Cmd.none)

                _ ->
                    ({ model | playing = False }, Cmd.none)


-- VIEW


isOn : Pixel -> Bool
isOn pixel =
    case pixel of
        On -> True
        Off -> False


pixel : Pixel -> Html Msg
pixel pixel =
    div 
        [ style
            [ ("border-radius", "50%")
            , ("float", "left")
            , ("width", "10px")
            , ("height", "10px")
            , ("background", if isOn pixel then "limegreen" else "black")
            ]
        ]
        []


row : Array Pixel -> Html Msg
row pixels =
    div
        [ style [ ("overflow", "hidden") ] ]
        (Array.map pixel pixels |> Array.toList)


display : Display -> Html Msg
display display =
    div
        [ style
            [ ("background", "black")
            , ("padding", "5px")
            , ("max-width", "500px")
            ]
        ]
        (Array.map row display |> Array.toList)


view : Model -> Html Msg
view model =
    let
        loadingHtml = h2 [] [text "Loading..." ]
        displayHtml = display model.display
    in
        div []
            [ if model.loading then loadingHtml else displayHtml
            , button [onClick (NextInstruction model.instructions)] [ text "Next Instruction" ]
            , button [onClick (TogglePlay)] [ text (if model.playing then "Stop" else "Play") ]
            , button [onClick (AllInstructions model.instructions)] [ text "All Instructions" ]
            ]
        



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.playing
    then Time.every (20 * millisecond) Tick
    else Sub.none



-- HTTP


fetchInput : Cmd Msg
fetchInput =
    let 
        url =
            "https://raw.githubusercontent.com/lieberkind/adventofcode2016/master/aoc8.input.json"
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
        last = (slice -1 (length arr) arr)
        init = (slice 0 -1 arr)
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


parseInput : List String -> List Instruction
parseInput input =
    List.concatMap parseString input


followInstructions : List Instruction -> Display -> Display
followInstructions instrs display =
    List.foldl followInstruction display instrs