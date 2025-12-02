module Year2024.Day06

open System.IO

type Direction = Up | Down | Left | Right
type Position = { X : int; Y : int; Direction : Direction }

let map = 
    File.ReadAllLines "./input.txt"
    |> Array.map (fun a -> a.ToCharArray())

let isGuard x =
    x = '^' || x = 'v' || x = '<' || x = '>'

let guardDirection x =
    match x with
    | '^' -> Up
    | 'v' -> Down
    | '<' -> Left
    | '>' -> Right
    | _ -> failwith "Invalid guard"

let findGuard =
    let x = map |> Array.findIndex (fun a -> Array.exists isGuard a)
    let y = map.[x] |> Array.findIndex isGuard
    { X = x; Y = y; Direction = guardDirection map.[x].[y] }

let nextPos pos =
    match pos.Direction with
    | Up -> { pos with X = pos.X - 1 }
    | Down -> { pos with X = pos.X + 1 }
    | Left -> { pos with Y = pos.Y - 1 }
    | Right -> { pos with Y = pos.Y + 1 }

let turn direction =
    match direction with
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let rec walk (pos, trail) =
    let newPos = nextPos pos
    if newPos.X < 0 || newPos.X >= map.Length || newPos.Y < 0 || newPos.Y >= map.[0].Length then
        (newPos, pos :: trail)
    elif map.[newPos.X].[newPos.Y] = '#' then
        walk ({pos with Direction = turn pos.Direction}, trail)
    else
        walk (newPos, pos :: trail)

let firstPart = 
    walk (findGuard, [])
    |> snd
    |> List.distinctBy (fun a -> (a.X, a.Y))
    |> List.length


let putObstacle x y =
    map
    |> Array.mapi (fun i a -> 
        a |> Array.mapi (fun j b -> 
            if i = x && j = y then '#' else b))

let rec walkLoop (map : char array array) (pos, trail) =
    let newPos = nextPos pos
    if newPos.X < 0 || newPos.X >= map.Length || newPos.Y < 0 || newPos.Y >= map.[0].Length then
        0
    elif (trail |> List.exists (fun p -> p = pos)) then
        1
    elif map.[newPos.X].[newPos.Y] = '#' then
        walkLoop map ({pos with Direction = turn pos.Direction}, trail)
    else
        walkLoop map (newPos, pos :: trail)


let secondPart =
    [ for i = 0 to map.Length - 1 do
        for j = 0 to map.[0].Length - 1 do
            if map.[i].[j] = '.' then
                let newMap = putObstacle i j
                yield walkLoop newMap (findGuard, []) ]
    |> List.sum
