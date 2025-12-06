module Year2025.Day06

open System
open System.IO  

type Operation =
    | Add
    | Mul

type Cell =
    | Number of int64
    | Operation of Operation

let input = 
    File.ReadAllLines("input.txt")
    |> Array.map (fun line -> line.Split(" ", StringSplitOptions.RemoveEmptyEntries) )
    |> Array.map (Array.map (fun token ->
        match token with
        | "+" -> Operation Add
        | "*" -> Operation Mul
        | _ -> Number (int64 token)
    ))

let number cell =
    match cell with
    | Number n -> n
    | _ -> 0

let part1() =
    let rows = input.Length
    let cols = input.[0].Length
    [for col in 0 .. cols - 1 do
        [ for row in 0 .. rows - 2 do input.[row].[col] ]
        |> (match input.[rows-1].[col] with
            | Operation Add -> List.sumBy number
            | Operation Mul -> List.fold (fun acc cell -> acc * (number cell)) 1L
            | _ -> failwith "Invalid operation" )
    ]
    |> List.sum
    |> printfn "Part 1: %d"
    
type Column = {
    Start : int
    End   : int
}

let input2 =
    File.ReadAllLines("input.txt")

let part2() =
    let rows = input2.Length
    let columns = 
        input2.[rows-1]
        |> Seq.mapi (fun i c -> if c <> ' ' then i else -1)
        |> Seq.filter (fun i -> i > -1)
        |> Seq.toList
        |> (fun l -> l @ [ input2.[rows-1].Length + 1 ])
        |> List.pairwise
        |> List.map (fun (start, endi) -> { Start = start; End = endi - 2 })

    columns
    |> List.map (fun column ->
        [ for col in column.Start .. column.End do 
            [| for row in 0 .. rows - 2 do input2.[row].[col] |]
            |> String
            |> _.Trim()
            |> int64
        ]
        |> (match input2.[rows-1].[column.Start] with
            | '+' -> List.sum
            | '*' -> List.fold (fun acc n -> acc * n) 1L
            | _ -> failwith "Invalid operation" ))
    |> List.sum
    |> printfn "Part 2: %d"
