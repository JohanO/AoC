module Year2025.Day03

open System
open System.IO

let day3Input =
    File.ReadAllText("input.txt")
    |> _.Split("\r\n", StringSplitOptions.RemoveEmptyEntries)
    
let maxJolt (line : string) =
    let left = line.Substring(0, line.Length - 1) |> Seq.mapi (fun i c -> (i, c)) |> Seq.maxBy snd
    let right = line.[(fst left + 1)..] |> Seq.max
    sprintf "%c%c" (left |> snd) right |> int

let part1 () =
    day3Input
    |> Array.map maxJolt
    |> Array.sum
    |> printfn "Part 1: %d"

let maxJolt2 (line : string) =
    let mapped = line |> Seq.mapi (fun i c -> (i, c)) |> Seq.toArray
    let endIndex = mapped.Length - 1
    let first = mapped.[0..endIndex-11] |> Seq.maxBy snd
    let second = mapped.[fst first + 1..endIndex-10] |> Seq.maxBy snd
    let third = mapped.[fst second + 1..endIndex-9] |> Seq.maxBy snd
    let fourth = mapped.[fst third + 1..endIndex-8] |> Seq.maxBy snd
    let fifth = mapped.[fst fourth + 1..endIndex-7] |> Seq.maxBy snd
    let sixth = mapped.[fst fifth + 1..endIndex-6] |> Seq.maxBy snd
    let seventh = mapped.[fst sixth + 1..endIndex-5] |> Seq.maxBy snd
    let eighth = mapped.[fst seventh + 1..endIndex-4] |> Seq.maxBy snd
    let ninth = mapped.[fst eighth + 1..endIndex-3] |> Seq.maxBy snd
    let tenth = mapped.[fst ninth + 1..endIndex-2] |> Seq.maxBy snd
    let eleventh = mapped.[fst tenth + 1..endIndex-1] |> Seq.maxBy snd
    let twelfth = mapped.[fst eleventh + 1..endIndex] |> Seq.maxBy snd
    [ 
        snd first
        snd second
        snd third
        snd fourth
        snd fifth
        snd sixth
        snd seventh
        snd eighth
        snd ninth
        snd tenth
        snd eleventh
        snd twelfth
    ]
    |> Seq.fold (fun acc c -> acc + string c) ""
    |> int64

let part2 () =
    day3Input
    |> Array.map maxJolt2
    |> Array.sum
    |> printfn "Part 2: %d"