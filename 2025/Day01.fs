module Year2025.Day01

open System
open System.IO

let dial = 100

type Direction = Left | Right

type rotation = {
    Direction : Direction
    Count : int
}

let day1Input =
    File.ReadAllText("input.txt")
    |> _.Split("\n", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun s -> {
        Direction = if s.[0] = 'L' then Left else Right
        Count = s.[1..] |> int
    })

let move position rotation =
    match rotation.Direction with
    | Left -> 
        if rotation.Count > position then
            let newPos = (position - rotation.Count) % dial
            if newPos < 0 then dial + newPos else newPos
        else position - rotation.Count
    | Right -> (position + rotation.Count) % dial

let countZeros (pos, zeroCount) rotation =
    let newPos = move pos rotation
    if newPos = 0 then (newPos, zeroCount + 1) else (newPos, zeroCount)

let part1 () =
    day1Input
    |> Array.fold countZeros (50, 0)
    |> snd
    |> printfn "Part 1: %d"

let countZeroPassages (pos, zeroPassages) rotation =
    let newPos = move pos rotation
    let passages =
        match rotation.Direction with
        | Left -> 
            if pos = 0 then rotation.Count / dial
            elif (pos - rotation.Count) <= 0 then ((pos - rotation.Count) / dial) |> abs |> (+) 1
            else 0
        | Right -> (pos + rotation.Count) / dial
    Console.WriteLine($"Moving {rotation.Direction} {rotation.Count} steps from {pos} to {newPos} passing {passages} zeros")
    (newPos, zeroPassages + passages)

let part2 () =
    day1Input
    |> Array.fold countZeroPassages (50, 0)
    |> snd
    |> printfn "Part 2: %d"
    