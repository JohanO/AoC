module Year2015.Day01

open System.IO

let nextFloor current c =
    if c = '(' then current + 1 else current - 1

let part1 () =
    File.ReadAllText("input.txt")
    |> Seq.fold nextFloor 0
    |> printfn "Part 1: %d"

let part2 () =
    File.ReadAllText("input.txt")
    |> Seq.fold (fun (floor, position) c ->
        let newFloor = nextFloor floor c
        if floor = 0 && newFloor = -1 then
            printfn "Part 2: %d" (position + 1)
        (newFloor, position + 1)
    ) (0, 0)
    |> ignore