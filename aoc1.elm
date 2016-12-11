module AOC1 exposing (solvePt1)

import String exposing (left, dropLeft, toInt)

type Instruction = MoveRight Int | MoveLeft Int | StandStill

type Direction = North | East | South | West

type alias DirectedPoint = (Direction, Point)

type alias Point = { x:Int, y:Int }

toInstruction : String -> Instruction
toInstruction str =
  let (turn, moves) =
    (left 1 str, toInt (dropLeft 1 str))
  in
    case (turn, moves) of
      ("R", Ok distance) -> MoveRight distance
      ("L", Ok distance) -> MoveLeft distance
      (_, _) -> StandStill

move : Direction -> Int -> Point -> Point
move dir dist p =
  case dir of
    North -> {p | y = p.y + dist }
    East  -> {p | x = p.x + dist }
    South -> {p | y = p.y - dist }
    West  -> {p | x = p.x - dist }

followInstruction : Instruction -> DirectedPoint -> DirectedPoint
followInstruction instruction directedPoint =
    case (directedPoint, instruction) of
      ((North, point), MoveRight distance) -> (East, move East distance point)
      ((North, point), MoveLeft distance) -> (West, move West distance point)

      ((East, point), MoveRight distance) -> (South, move South distance point)
      ((East, point), MoveLeft distance) -> (North, move North distance point)

      ((South, point), MoveRight distance) -> (West, move West distance point)
      ((South, point), MoveLeft distance) -> (East, move East distance point)

      ((West, point), MoveRight distance) -> (North, move North distance point)
      ((West, point), MoveLeft distance) -> (South, move South distance point)
      (_, StandStill) -> directedPoint

input : List String
input = ["R4", "R5", "L5", "L5", "L3", "R2", "R1", "R1", "L5", "R5", "R2", "L1", "L3", "L4", "R3", "L1", "L1", "R2", "R3", "R3", "R1", "L3", "L5", "R3", "R1", "L1", "R1", "R2", "L1", "L4", "L5", "R4", "R2", "L192", "R5", "L2", "R53", "R1", "L5", "R73", "R5", "L5", "R186", "L3", "L2", "R1", "R3", "L3", "L3", "R1", "L4", "L2", "R3", "L5", "R4", "R3", "R1", "L1", "R5", "R2", "R1", "R1", "R1", "R3", "R2", "L1", "R5", "R1", "L5", "R2", "L2", "L4", "R3", "L1", "R4", "L5", "R4", "R3", "L5", "L3", "R4", "R2", "L5", "L5", "R2", "R3", "R5", "R4", "R2", "R1", "L1", "L5", "L2", "L3", "L4", "L5", "L4", "L5", "L1", "R3", "R4", "R5", "R3", "L5", "L4", "L3", "L1", "L4", "R2", "R5", "R5", "R4", "L2", "L4", "R3", "R1", "L2", "R5", "L5", "R1", "R1", "L1", "L5", "L5", "L2", "L1", "R5", "R2", "L4", "L1", "R4", "R3", "L3", "R1", "R5", "L1", "L4", "R2", "L3", "R5", "R3", "R1", "L3"]

solvePt1 : Int
solvePt1 =
  List.map toInstruction input
  |> List.foldl followInstruction (North, Point 0 0) 
  |> Tuple.second
  |> (\p -> abs p.x + abs p.y)