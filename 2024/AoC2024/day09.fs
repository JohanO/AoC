module day09

open System.IO

type Block =
    | Occupied of int
    | Free


let day09Input =
    File.ReadAllText "./input/day09.txt"
    |> _.ToCharArray()
    |> Array.map (fun c -> int c - int '0')

let toBlocks input =
    input
    |> Array.indexed
    |> Array.collect (fun (i, v) -> 
        let b = if i % 2 = 0 then Occupied(i / 2) else Free
        Array.replicate v b)

let rec compact l r (disk: Block[]) =
    if l >= r then
        disk
    else
        match (disk[l], disk[r]) with
        | (Free, Free) -> compact l (r - 1) disk
        | (Free, Occupied id) -> disk |> Array.updateAt l (Occupied id) |> Array.updateAt r Free |> compact (l + 1) (r - 1)
        | (Occupied _, Free) -> compact (l + 1) (r - 1) disk
        | (Occupied _, Occupied _) -> compact (l + 1) r disk

let checksum (disk: Block[]) =
    disk
    |> Array.indexed
    |> Array.sumBy (fun (i, b) ->
        match b with
        | Free -> 0L
        | Occupied id -> (int64 i) * (int64 id))

let firstPart input =
    let disk = input |> toBlocks
    disk |> compact 0 (Array.length disk - 1) |> checksum

let rec compact2 r (disk: Block[]) =
    if r <= 0 then
        disk
    else
        match disk[r] with
        | Free -> compact2 (r - 1) disk
        | Occupied id ->
            let length = disk[..r] |> Array.rev |> Array.takeWhile ((=) (Occupied id)) |> Array.length
            let newR = r - length
            let l = [ 0..r ] |> List.tryFind (fun l -> disk[l .. (l + length - 1)] |> Array.forall ((=) Free))

            match l with
            | None -> compact2 newR disk
            | Some l ->
                (disk, [ 0 .. (length - 1) ])
                ||> List.fold (fun acc d ->
                    acc
                    |> Array.updateAt (l + d) (Occupied id)
                    |> Array.updateAt (newR + 1 + d) Free)
                |> compact2 newR

let secondPart input =
    let disk = input |> toBlocks
    disk |> compact2 (Array.length disk - 1) |> checksum