module AOC8 exposing (createDisplay, turnOn, rect, flip, rotate, rotateRow, rotateColumn, parseString)

import Array exposing (repeat, length, append, slice, Array)
import Regex exposing (regex, HowMany (..))
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



-- AOC8 PT. 2 SOLUTION


type Led
    = On
    | Off


type alias Display =
    Array (Array Led)    


type Instruction
    = Rect Int Int
    | RotateRow Int
    | RotateColumn Int
    | NoOp


parseString : String -> List Instruction
parseString str =
    let
        rectRegex =
            regex "^rect ([0-9]+)x([0-9]+)$"

        rotateColumnRegex =
            regex "^rotate column x=([0-9]+) by ([0-9]+)$"

        rotateRowRegex =
            regex "^rotate row y=([0-9]+) by ([0-9]+)$"

        getPotentialIntegers : List Regex.Match -> List (Maybe Int)
        getPotentialIntegers matches =
            matches
            |> List.concatMap .submatches
            |> List.map (Maybe.andThen (String.toInt >> Result.toMaybe))
        
        toInstructionList : (Int -> Int -> List Instruction) -> List (Maybe Int) -> List Instruction
        toInstructionList constructor potentialIntegers =
            case potentialIntegers of
                [Just a, Just b] ->
                    constructor a b
                
                _ ->
                    [NoOp]
    in
        if Regex.contains rectRegex str then
            Regex.find All rectRegex str
            |> getPotentialIntegers
            |> toInstructionList (\a b -> [Rect a b])

        else if Regex.contains rotateColumnRegex str then
            Regex.find All rotateColumnRegex str
            |> getPotentialIntegers
            |> toInstructionList (\a b -> List.repeat b (RotateColumn a))

        else if Regex.contains rotateRowRegex str then
            Regex.find All rotateRowRegex str
            |> getPotentialIntegers
            |> toInstructionList (\a b -> List.repeat b (RotateRow a))

        else
            [NoOp]


parseInput : List String -> List Instruction
parseInput input =
    List.concatMap parseString input


rect : Int -> Int -> Display -> Display
rect x y display =
    let
        positions =
            Array.initialize x (\b -> Array.initialize y (\a -> (b, a)))
            |> flattenArray
            |> Array.toList
    in 
        turnOnAll positions display


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


followInstruction : Instruction -> Display -> Display
followInstruction instr disp =
    case instr of
        Rect x y ->
            rect x y disp

        RotateRow y ->
            rotateRow y disp
        
        RotateColumn x ->
            rotateColumn x disp
        
        NoOp ->
            disp


followInstructions : List Instruction -> Display -> Display
followInstructions instrs display =
    List.foldl followInstruction display instrs



-- DISPLAY HELPERS


createDisplay : Int -> Int -> Display
createDisplay x y =
    repeat y (repeat x Off)


turnOn : (Int, Int) -> Display -> Display
turnOn position display =
    let (x, y, row) =
        ( Tuple.first position
        , Tuple.second position        
        , Array.get (Tuple.second position) display
        )
    in
        case row of
            Just row ->
                let newRow =
                    Array.set x On row
                in
                    Array.set y newRow display

            _ ->
                display


turnOnAll : List (Int, Int) -> Display -> Display
turnOnAll positions display =
    case positions of
        p :: ps ->
            turnOnAll ps (turnOn p display)
        
        _ ->
            display


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

-- ARRAY HELPERS


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


flattenArray : Array (Array a) -> Array a
flattenArray =
    Array.foldr Array.append Array.empty



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


isOn : Led -> Bool
isOn pixel =
    case pixel of
        On -> True
        Off -> False


pixel : Led -> Html Msg
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


row : Array Led -> Html Msg
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