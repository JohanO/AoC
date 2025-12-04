module Year2025.Day04

open System
open System.IO

type Cell = Empty | Filled

let day4Input =
    File.ReadAllText("input.txt")
    |> _.Split("\r\n", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun line ->
        line.ToCharArray()
        |> Array.map (fun c -> if c = '@' then Filled else Empty)
    )

let directions = [| (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1) |]

let isFilled x y =
    if x < 0 || y < 0 || x >= day4Input.Length || y >= day4Input.[0].Length then false
    else day4Input.[x].[y] = Filled

let check x y =
    if day4Input.[x].[y] = Empty then false
    else
        directions
        |> Array.filter (fun (dx, dy) -> isFilled (x + dx) (y + dy))
        |> Array.length < 4

let part1 () =
    day4Input
    |> Array.mapi (fun x row ->
        row
        |> Array.mapi (fun y _ -> if check x y then 1 else 0)
        |> Array.sum)
    |> Array.sum
    |> printfn "Part 1: %d"

let remove x y =
    day4Input.[x].[y] <- Empty

let countRemove input =
    input
    |> Array.mapi (fun x row -> 
        row
        |> Array.mapi (fun y _ -> if check x y then (remove x y; 1) else 0)
        |> Array.sum)
    |> Array.sum

let part2 () =
    day4Input
    |> List.unfold (fun input ->
        match countRemove input with
        | 0 -> None
        | x -> Some (x, input))
    |> List.sum
    |> printfn "Part 2: %d"
