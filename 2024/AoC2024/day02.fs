module day02

open System

type Level = Increasing | Decreasing

let day02Input =
    System.IO.File.ReadLines("Input/Day02.txt")
    |> Seq.map (fun s -> s.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int |> Seq.toList)
    |> Seq.toList

let rule level list =
    match level with
    | Increasing -> list |> Seq.pairwise |> Seq.forall (fun (a, b) -> a < b && b - a <= 3)
    | Decreasing -> list |> Seq.pairwise |> Seq.forall (fun (a, b) -> a > b && a - b <= 3)

let check report =
    match report with
    | [] -> true
    | a::[] -> true
    | a::b::[] when a < b -> rule Increasing report 
    | a::b::[] when a > b -> rule Decreasing report
    | a::b::[] when a = b -> false
    | a::b::tail when a < b -> rule Increasing report
    | a::b::tail when a > b -> rule Decreasing report
    | a::b::tail when a = b -> false

let firstPart input =
    input
    |> List.map check
    |> List.filter (fun x -> x)
    |> List.length


let damper list =
    if (check list) then
        true
    else
        [ for i = 0 to list.Length - 1 do
            list |> List.removeAt i |> check ]
        |> List.exists (fun x -> x)

let secondPart input =
    input
    |> List.map damper
    |> List.filter (fun x -> x)
    |> List.length