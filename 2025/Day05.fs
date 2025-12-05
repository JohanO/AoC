module Year2025.Day05

open System
open System.IO

type IdRange = { Min: int64; Max: int64 }

let freshIds =
    File.ReadAllLines("input.txt")
    |> Array.takeWhile (fun line -> line <> "")
    |> Array.map (fun line -> 
        let parts = line.Split('-', StringSplitOptions.RemoveEmptyEntries)
        { Min = int64 parts.[0]; Max = int64 parts.[1] })

let availableIds =
    File.ReadAllLines("input.txt")
    |> Array.skipWhile (fun line -> line <> "")
    |> Array.skip 1
    |> Array.map int64

let isFresh id =
    freshIds
    |> Array.exists (fun range -> id >= range.Min && id <= range.Max)

let part1 () =
    availableIds
    |> Array.filter isFresh
    |> Array.length
    |> printfn "Part 1: %d"

let overlap a b =
    (a.Max < b.Min || a.Min > b.Max) |> not

let merge a b =
    { Min = Math.Min(a.Min, b.Min); Max = Math.Max(a.Max, b.Max) }

let reduceOverlap (curRange, cleared) range =
    if overlap curRange range then
        (merge curRange range, cleared)
    else 
        (range, curRange :: cleared)

let part2 () =
    freshIds
    |> Array.sortBy (fun range -> range.Min)
    |> Array.fold reduceOverlap ({Min = 0; Max = -1;}, [])
    |> (fun p -> fst p :: snd p)
    |> List.sumBy (fun r -> r.Max - r.Min + 1L)
    |> printfn "Part 2: %i"