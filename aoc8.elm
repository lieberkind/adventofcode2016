module AOC8 exposing (createDisplay, turnOn, rect, flip, rotate, rotateRow, rotateColumn)

import Array exposing (repeat, length, append, slice)


type Pixel = On | Off


type alias Display = Array.Array (Array.Array Pixel)    


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
        case (row) of
            (Just row) ->
                let (newRow) =
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


rotate : Array.Array a -> Array.Array a
rotate arr =
    append (slice -1 (length arr) arr) (slice 0 -1 arr)


unsafeGet : Int -> Array.Array a -> a
unsafeGet idx arr =
    let element =
        Array.get idx arr
    in
        case element of
            Just elem -> elem
            Nothing -> Debug.crash ("No element at index " ++ toString idx)
    
