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

let boolToSymbol (b : bool) = if b then "#" else "."

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
    let create (minX, minY, maxX, maxY) =
        { 
            X = -minX
            Y = -minY
            Arr = Array2D.zeroCreate (maxX - minX + 1) (maxY - minY + 1)
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
            field.Arr[field.X, field.Y] <- true
        with
        | :? IndexOutOfRangeException ->
            printfn $"Out of bounds {field.X} {field.Y}"
            reraise()

    let print (field : Field) =
        for i = 0 to Array2D.length1 field.Arr - 1 do
            for j = 0 to Array2D.length2 field.Arr - 1 do
                printf "%s" (boolToSymbol field.Arr[i, j])
            printfn ""
        printfn ""

let digTrench (field : Field) (instruction : Instruction) =
    for _ in 1 .. instruction.Distance do
        do field |> Field.digHole
        do field |> Field.move instruction.Direction

let getBounds (field : Field) (instructions : Instruction seq) =
    let bounds =
        seq {
            for instruction in instructions do
                for _ in 1 .. instruction.Distance do
                    do field |> Field.move instruction.Direction
                    yield (field.X, field.Y)
        } |> Seq.fold (fun (minX, minY, maxX, maxY) (x, y) -> 
                (min minX x, min minY y, max maxX x, max maxY y)) (0, 0, 0, 0)
    field |> Field.goTo 0 0
    bounds
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

let countHoles (rows : bool seq seq) =
    rows
    |> Seq.sumBy (Seq.sumBy boolToInt)

let printRowSeq (row : seq<bool>) =
    row
    |> Seq.map boolToSymbol
    |> String.concat ""
    |> printfn "%s"

do
    fillRowSeq [false; true; false; false; true; false; false ]
    |> Seq.map (State.toBool >> boolToSymbol) 
    |> String.concat " " 
    |> printfn "%s"

do
    let field1 = Field.create (0, 0, 1000, 1000)
    let instructions = readInput "assets/example.txt" |> Seq.toArray
    let bounds = getBounds field1 instructions
    let field = Field.create bounds
    instructions |> Seq.iter (digTrench field)
    Field.print field
    let filled = field.Arr |> array2DtoRows |> fillRowsSeq
    filled |> Seq.iter printRowSeq
    printfn "%i" (countHoles filled)
// 42556 is to high
