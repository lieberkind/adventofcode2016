import Http
import Html exposing (..)
import Json.Decode as Decode
import Array
import Regex exposing (Regex, regex, escape)
import Tuple exposing (first, second)


main : Program Never Model Msg
main =
    Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- COMMON


type alias IP =
    { sequences : List String
    , hypernetSequences : List String
    }


parseIP : String -> IP
parseIP str =
    let toIP t =
        IP
            (Tuple.first t |> List.map Tuple.second)
            (Tuple.second t |> List.map Tuple.second)
    in
        Regex.find Regex.All sequenceRegex str
        |> List.map .match
        |> Array.fromList
        |> Array.toIndexedList
        |> List.partition isIndexEven
        |> toIP



-- AOC7 PT 1


isIndexEven : (Int, a) -> Bool
isIndexEven t =
    (Tuple.first t) % 2 == 0


sequenceRegex : Regex
sequenceRegex =
    regex "[a-z]+"


isAbba : List Char -> Bool
isAbba chars =
    case chars of
        [a, b, c, d] -> a == d && b == c && a /= b
        _            -> False


hasAbba : List Char -> Bool
hasAbba chars =
    case chars of
        [a, b, c, d]           -> isAbba [a, b, c, d]
        a :: b :: c :: d :: xs -> isAbba [a, b, c, d] || hasAbba (b :: c :: d :: xs)
        _                      -> False


supportsTLS : IP -> Bool
supportsTLS ip =
    let strHasAbba =
        (String.toList >> hasAbba)
    in
        not (List.any strHasAbba ip.hypernetSequences) &&
            (List.any strHasAbba ip.sequences)


solvePt1 : List String -> Int
solvePt1 strs =
    strs
    |> List.map parseIP
    |> List.filter supportsTLS
    |> List.length



-- AOC7 PT 2


type alias ABA =
    { a : Char
    , b : Char
    , c : Char
    }


isAba : Char -> Char -> Char -> Bool
isAba a b c =
    a == c && b /= a


abaToBab : ABA -> ABA
abaToBab aba =
    ABA aba.b aba.a aba.b


findAbas : String -> List ABA
findAbas str =
    let reducer chars abas =
        case chars of
            a :: b :: c :: xs -> 
                if
                    isAba a b c
                then
                    reducer (b :: c :: xs) (ABA a b c :: abas)
                else
                    reducer (b :: c :: xs) abas

            _ -> abas
    in
        reducer (String.toList str) []


containsAba : ABA -> String -> Bool
containsAba aba =
    String.contains (String.fromList [aba.a, aba.b, aba.c])


containsAbas : List ABA -> String -> Bool
containsAbas abas str =
    List.any (flip containsAba str) abas


supportsSSL : IP -> Bool
supportsSSL ip =
    let babs =
        List.map findAbas ip.sequences |> List.concat |> List.map abaToBab
    in
        List.any (containsAbas babs) ip.hypernetSequences


solvePt2 : List String -> Int
solvePt2 strs =
    strs
    |> List.map parseIP
    |> List.filter supportsSSL
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
            (Model (solvePt2 strs) False, Cmd.none)

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
            "https://raw.githubusercontent.com/lieberkind/adventofcode2016/master/aoc7.input.json"
    in
        Http.send InputLoaded (Http.get url decodeStringList)


decodeStringList : Decode.Decoder (List String)
decodeStringList =
    (Decode.list Decode.string)