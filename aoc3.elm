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



-- AOC3 SOLUTION


type Candidate
    = Triple Int Int Int
    | InvalidCandidate


toCandidate : String -> Candidate
toCandidate str =
    let resultList =
        String.words str
        |> List.map (String.toInt)
    in
        case resultList of
            [Ok x, Ok y, Ok z] -> Triple x y z    
            _                  -> InvalidCandidate

isPossibleTriangle : Int -> Int -> Int -> Bool
isPossibleTriangle a b c =
    a + b > c && a + c > b && b + c > a


isCandidateValid : Candidate -> Bool
isCandidateValid cand =
    case cand of
        Triple x y z     -> isPossibleTriangle x y z
        InvalidCandidate -> False


solvePt1 : List String -> Int
solvePt1 strs =
    strs
    |> List.map toCandidate
    |> List.filter isCandidateValid
    |> List.length



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