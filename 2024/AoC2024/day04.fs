module day04

let day04Input = 
    System.IO.File.ReadAllLines "./input/day04.txt"
    |> Array.map Seq.toArray

let isChar x y c =
    if x < 0 || y < 0 || x >= day04Input.Length || y >= day04Input.[0].Length then false
    elif day04Input.[x].[y] = c then true
    else false

let directions = [| (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1) |]

let check x y = 
    if day04Input.[x].[y] <> 'X' then 0
    else
        directions
        |> Array.map (fun (dx, dy) ->
            if isChar (x + dx) (y + dy) 'M' && isChar (x + 2 * dx) (y + 2 * dy) 'A' && isChar (x + 3 * dx) (y + 3 * dy) 'S' then 1
            else 0)
        |> Array.sum
    
let firstPart input = 
    input
    |> Array.mapi (fun x a -> a |> Array.mapi (fun y _ -> check x y) |> Array.sum)
    |> Array.sum
    


let cross x y = 
    if day04Input.[x].[y] <> 'A' then 0
    elif ((isChar (x - 1) (y - 1) 'M' && isChar (x + 1) (y + 1) 'S') || (isChar (x - 1) (y - 1) 'S' && isChar (x + 1) (y + 1) 'M'))
         && ((isChar (x - 1) (y + 1) 'M' && isChar (x + 1) (y - 1) 'S') || (isChar (x - 1) (y + 1) 'S' && isChar (x + 1) (y - 1) 'M')) then 1
    else 0

let secondPart input =
    input
    |> Array.mapi (fun x a -> a |> Array.mapi (fun y _ -> cross x y) |> Array.sum)
    |> Array.sum