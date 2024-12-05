module day05

open System.IO

type Rule = { First : int; Second: int; }

let rules = 
    File.ReadAllLines "./input/day05.txt"
    |> Array.takeWhile (fun x -> x.Length > 0)
    |> Array.map (fun a -> a.Split("|")) |> Array.map (fun a -> { First = int a.[0]; Second = int a.[1] })

let pages = 
    File.ReadAllLines "./input/day05.txt"
    |> Array.skipWhile (fun x -> x.Length > 0)
    |> Array.skip 1
    |> Array.map (fun a -> a.Split(","))
    |> Array.map (fun a -> a |> Array.map int)

let fulfillsRule rule pages = 
    if Array.contains rule.First pages && Array.contains rule.Second pages then
        Array.findIndex (fun v -> v = rule.First) pages < Array.findIndex (fun v -> v = rule.Second) pages
    else
       true

let firstPart =
    pages
    |> Array.filter (fun p -> rules |> Array.forall (fun r -> fulfillsRule r p))
    |> Array.map (fun a -> a.[a.Length/2])
    |> Array.sum



let notFullfillsRule rule pages = 
    if Array.contains rule.First pages && Array.contains rule.Second pages then
        Array.findIndex (fun v -> v = rule.First) pages > Array.findIndex (fun v -> v = rule.Second) pages
    else
       false

let reorder pages =    
    rules
    |> Array.filter (fun r -> Array.contains r.First pages && Array.contains r.Second pages)
    |> Array.groupBy (fun r -> r.First)
    |> Array.sortByDescending (fun (_, a) -> a.Length)
    |> Array.map fst

let secondPart =
    pages
    |> Array.filter (fun p -> rules |> Array.exists (fun r -> notFullfillsRule r p))
    |> Array.map reorder
    |> Array.map (fun a -> a.[a.Length/2])
    |> Array.sum
    