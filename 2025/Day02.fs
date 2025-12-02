module Year2025.Day02

open System
open System.IO

type IdRange = {
    Min : int64
    Max : int64
}

let day2Input =
    File.ReadAllText("input.txt")
    |> _.Split(",", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun s ->
        let parts = s.Split("-")
        { Min = int64 parts[0]
          Max = int64 parts[1] })

let isInvalidId1 id =
    let idStr = id.ToString()
    let size = idStr.Length
    if size % 2 <> 0 then false
    else
        let halfSize = size / 2
        let leftHalf = idStr.[0..halfSize - 1]
        let rightHalf = idStr.[halfSize..size - 1]
        leftHalf = rightHalf

let part1 () =
    day2Input
    |> Array.collect (fun range -> [| range.Min .. range.Max |])
    |> Array.filter isInvalidId1
    |> Array.sum
    |> printfn "Part 1: %d"


let isInvalidId2 id =
    let rec hasDuplicates str len =
        if len = 0 then false
        elif str |> Seq.chunkBySize len |> Seq.map String |> Seq.distinct |> Seq.length = 1 then true
        else hasDuplicates str (len - 1)

    let idStr = id.ToString()
    let size = idStr.Length / 2
    hasDuplicates idStr size

let part2 () =
    day2Input
    |> Array.collect (fun range -> [| range.Min .. range.Max |])
    |> Array.filter isInvalidId2
    |> Array.sum
    |> printfn "Part 2: %d"