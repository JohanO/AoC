﻿module day01

open System
open System.IO

let day01Input =
    File.ReadLines("Input/Day01a.txt")
    |> Seq.map (fun s -> s.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int64)
    |> Seq.map (fun a -> a.[0], a.[1])
    |> Seq.toList

let distance x1 x2 =
    abs (x1 - x2)

let firstPart input =
    let left = input |> Seq.map fst |> Seq.sort
    let right = input |> Seq.map snd |> Seq.sort
    Seq.map2 distance left right 
    |> Seq.sum



let count x list =
    list |> Seq.filter (fun y -> y = x) |> Seq.length |> int64

let secondPart input =
    let left = input |> Seq.map fst
    let right = input |> Seq.map snd
    left |> Seq.map (fun x -> (count x right) * x) |> Seq.sum

