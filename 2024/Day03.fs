module Year2024.Day03

open System.Text.RegularExpressions

let day03Input =
    System.IO.File.ReadAllText("input.txt")

let mulRx = new Regex(@"mul[\(]\d+,\d+[\)]", RegexOptions.Compiled)

let digitsRx = new Regex(@"\d+", RegexOptions.Compiled)

let numbers s =
    digitsRx.Matches(s)
    |> Seq.map (fun m -> m.Value)
    |> Seq.map int
    |> Seq.pairwise
    |> Seq.head

let firstPart input =
    input
    |> mulRx.Matches
    |> Seq.map (fun v -> numbers v.Value)
    |> Seq.map (fun (a, b) -> a * b)
    |> Seq.sum




let fullRx = new Regex(@"mul[\(]\d+,\d+[\)]|don't[\(][\)]|do[\(][\)]", RegexOptions.Compiled)

type Instruction = 
    | Mul of int * int
    | Dont
    | Do

type State =
    | Enabled of int
    | Disabled of int

let toInstruction (s : string) =
    match s.Substring(0, 3) with
    | "mul" -> numbers s |> Mul
    | "don" -> Dont
    | "do(" -> Do
    | _ -> failwith "Invalid instruction"

let step state instruction =
    match state with
    | Enabled sum -> 
        match instruction with
        | Mul (a, b) -> Enabled (sum + a * b)
        | Dont -> Disabled sum
        | Do -> Enabled sum
    | Disabled sum -> 
        match instruction with
        | Mul _ -> Disabled sum
        | Dont -> Disabled sum
        | Do -> Enabled sum


let secondPart input =
    input
    |> fullRx.Matches
    |> Seq.map (fun v -> toInstruction v.Value)
    |> Seq.fold step (Enabled 0)