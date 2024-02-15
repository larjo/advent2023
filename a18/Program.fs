open System
open System.IO

type Direction = 
    | Up
    | Down
    | Left
    | Right

type Instruction = {
    Direction: Direction
    Distance: int
}

let readInput (filePath : string) = 
    File.ReadAllLines(filePath)
    |> Seq.map (fun (line: string) -> line.Split(' '))
    |> Seq.map (fun line -> 
        let direction = 
            match line[0] with
            | "U" -> Up
            | "D" -> Down
            | "L" -> Left
            | "R" -> Right
            | _ -> failwith "Invalid direction"
        let distance = int line[1]
        { Direction = direction; Distance = distance }
    )

type Field = {
    mutable X: int
    mutable Y: int
    Arr: bool array2d
}
module Field =
    let create width height =
        { 
            X = width / 2
            Y = height / 2
            Arr = Array2D.zeroCreate height width
        }
    let goTo x y (field : Field) =
        field.X <- x
        field.Y <- y
    let move (direction : Direction) (field : Field) =
        match direction with
        | Up -> field.Y <- field.Y - 1
        | Down -> field.Y <- field.Y + 1
        | Left -> field.X <- field.X - 1
        | Right -> field.X <- field.X + 1
    
    let digHole (field : Field) =
        try
            field.Arr[field.Y, field.X] <- true
        with
        | :? IndexOutOfRangeException ->
            printfn $"Out of bounds {field.Y} {field.X}"
            reraise()

    let print (field : Field) =
        for i = 0 to Array2D.length1 field.Arr - 1 do
            for j = 0 to Array2D.length2 field.Arr - 1 do
                printf "%s" (if field.Arr[i, j] then "#" else ".")
            printfn ""

let digTrench (field : Field) (instruction : Instruction) =
    for _ in 1 .. instruction.Distance do
        do field |> Field.digHole
        do field |> Field.move instruction.Direction
let boolToInt (b : bool) = if b then 1 else 0

let array2DtoRows (arr : bool array2d) =
    seq {
        for i = 0 to Array2D.length1 arr - 1 do
            seq {
                for j = 0 to Array2D.length2 arr - 1 do
                    yield arr[i, j]
            }
    }

type State =
    | Inside
    | Outside
    | Left
    | Right
module State =
    let toBool (state : State) =
        match state with
        | Inside -> true
        | Outside -> false
        | Left -> true
        | Right -> true

let fillRowSeq (row : seq<bool>)  =
    row
    |> Seq.scan (fun state hole ->
        match (state, hole) with
        | (Outside, false) -> Outside
        | (Outside, true) -> Left
        | (Left, true) -> Left
        | (Left, false) -> Inside
        | (Inside, false) -> Inside
        | (Inside, true) -> Right
        | (Right, true) -> Right
        | (Right, false) -> Outside
    ) Outside
    |> Seq.tail

let fillRowsSeq (rows : bool seq seq) =
    rows
    |> Seq.map (fillRowSeq >> Seq.map State.toBool)

let printRowSeq (row : seq<bool>) =
    row
    |> Seq.map (fun b -> if b then "#" else ".")
    |> String.concat " "
    |> printfn "%s"
do
    fillRowSeq [false; true; false; false; true; false; false ]
    |> Seq.map (State.toBool >> boolToInt >> string) 
    |> String.concat " " 
    |> printfn "%s"

let fillRow (field : Field) =
    let mutable inside = false
    for x = 0 to Array2D.length2 field.Arr - 1 do
        if field.Arr[field.Y, x] then
            if inside then
                inside <- false
            else
                inside <- true
        else
            if inside then
                field.Arr[field.Y, x] <- true
let fillField (field : Field) =
    for y = 0 to Array2D.length1 field.Arr - 1 do
        do field.Y <- y
        do fillRow field

let countHoles (rows : bool seq seq) =
    rows
    |> Seq.sumBy (Seq.sumBy boolToInt)

let field = Field.create 600 600
do
    readInput "assets/input.txt"
    |> Seq.iter (digTrench field)
    Field.print field
    let filled = field.Arr |> array2DtoRows |> fillRowsSeq
    filled |> Seq.iter printRowSeq
    printfn "%i" (countHoles filled)
// 42556 is to high