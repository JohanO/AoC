module Year2024.Day07

open System
open System.IO

type Equation = { Value: int64; Operands: int64 array }
type Operation = Add | Mul | Con
   
let day07Input = 
    File.ReadAllLines "./input.txt"
    |> Array.map (fun a -> a.Split(':', StringSplitOptions.RemoveEmptyEntries) |> Array.toList)
    |> Array.map (fun a -> { Value = int64 a.[0]; Operands = a.[1].Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int64 })

let rec comibinations count =
    seq {
        if count = 0 then yield []
        else
            let tail = comibinations (count - 1)
            yield! tail |> Seq.map (fun t -> Add :: t)
            yield! tail |> Seq.map (fun t -> Mul :: t)
            yield! tail |> Seq.map (fun t -> Con :: t)
    }

let apply x y op =
    match op with
    | Add -> x + y
    | Mul -> x * y
    | Con -> [string x; string y] |> String.concat "" |> int64

let evaluate equation operations =
    equation.Operands
    |> Array.skip 1
    |> Array.fold2 apply equation.Operands.[0] <| operations
    
let check equation =
    comibinations (equation.Operands.Length - 1)
    |> Seq.map (fun ops -> evaluate equation (List.toArray ops))
    |> Seq.exists (fun x -> x = equation.Value)

let firstPart =
    day07Input
    |> Array.filter check
    |> Array.sumBy _.Value
    