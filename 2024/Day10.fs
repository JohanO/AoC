module Year2024.Day10

open System.IO

type Point = { X: int; Y: int; Z: int }

let day10Input = 
    File.ReadAllLines "./input.txt"
    |> Array.map (fun row -> row.ToCharArray() |> Array.map (fun c -> int c - int '0'))

let toPoints (input : int array array) =
    [ for y = 0 to input.Length - 1 do
        for x = 0 to input.[y].Length - 1 do
            yield { X = x; Y = y; Z = input[x][y] }]

let checkPoints pos height =
    [ { pos with X = pos.X + 1; Z = height }
      { pos with X = pos.X - 1; Z = height }
      { pos with Y = pos.Y + 1; Z = height }
      { pos with Y = pos.Y - 1; Z = height }]

let scoreTrail head map scoreFunc =
    let rec findTrailTails curHeight pos =
        match curHeight with
        | 9 -> [pos]
        | _ -> 
            checkPoints pos (curHeight + 1)
            |> List.filter (fun p -> map |> List.contains p)
            |> List.collect (fun p -> findTrailTails (curHeight + 1) p)

    findTrailTails 0 head |> scoreFunc

let findTrails scoreFunc input =
    let map = input |> toPoints
    map 
    |> List.filter (fun p -> p.Z = 0)
    |> List.map (fun p -> scoreTrail p map scoreFunc)
    |> List.sum

let firstPart input =
    input |> findTrails (List.distinct >> List.length)

let secondPart input =
    input |> findTrails List.length
