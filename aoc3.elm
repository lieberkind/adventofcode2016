module AOC3 exposing (solvePt1)

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

input : List String
input =
    ["  10  20  29"]

solvePt1 : Int
solvePt1 =
    input
    |> List.map toCandidate
    |> List.filter isCandidateValid
    |> List.length