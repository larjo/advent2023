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

let arr = 
    readInput "assets/example.txt"
    |> Seq.fold (fun (acc: int array2d) (x : Instruction) ->
        acc) (Array2D.zeroCreate 10 10)


Seq.iter (printfn "%A") <| readInput "assets/example.txt"

