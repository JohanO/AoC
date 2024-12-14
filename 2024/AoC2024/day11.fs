module day11

open System
open System.IO

let day11Input = 
    File.ReadAllText "./input/day11.txt"
    |> _.Split(" ", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int64
    |> Array.toList

let (|IsEvenDigits|_|) v = 
    let str = v.ToString()
    if str.Length % 2 = 0 then
        Some (str[..str.Length / 2 - 1] |> int64, str[str.Length / 2..] |> int64)
    else
        None

let applyRules x = 
    match x with
    | 0L -> [1L]
    | IsEvenDigits (l, r) -> [l; r]
    | _ -> [x * 2024L]

let rec blink count input = 
    let rec loop i acc = 
        if i = count then
            acc
        else
            List.collect applyRules acc |> loop (i + 1)
    loop 0 input

let firstPart input = 
    input 
    |> blink 25
    |> List.length

let secondPart input = 
    input 
    |> Array.ofList
    |> Array.Parallel.map (fun x -> blink 75 [x] |> List.length)
    |> Array.sum