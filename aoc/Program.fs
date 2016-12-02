type North = North of int
type East = East of int
type South = South of int
type West = West of int
type CardinalDirection = 
    | North
    | East
    | South
    | West

type Direction(dir: CardinalDirection) =
    member this.Direction = dir
    member this.Right =
        match this.Direction with
        | North -> Direction(CardinalDirection.East)
        | East -> Direction(CardinalDirection.South)
        | South -> Direction(CardinalDirection.West)
        | West -> Direction(CardinalDirection.North)

    member this.Left =
        match this.Direction with
        | North -> Direction(CardinalDirection.West)
        | West -> Direction(CardinalDirection.South)
        | South -> Direction(CardinalDirection.East)
        | East -> Direction(CardinalDirection.North)

let step pos (dir:Direction) steps = 
    let x,y = pos
    match dir.Direction with
    | North -> 
        let dis = [y..y+steps]
        dis |> Seq.map (fun a -> (x, a)) |> Seq.toList
    | East -> 
        let dis = [x..x+steps]
        dis |> Seq.map (fun a -> (a, y)) |> Seq.toList
    | South -> 
        let dis = [y-steps..y]
        dis |> Seq.sortBy (~-) |> Seq.map (fun a -> (x, a)) |> Seq.toList
    | West -> 
        let dis = [x-steps..x]
        dis |> Seq.sortBy (~-) |> Seq.map (fun a -> (a, y)) |> Seq.toList

let rec firstduplicate oldPositions newPositions =
    let positionMatch pos list = List.exists (fun elem -> pos = elem) list
    match newPositions with
    | [] -> None
    | head::tail -> 
        match head with
        | _ when positionMatch head oldPositions -> Some(head)
        | _ -> firstduplicate oldPositions tail

let day1 (input:string) =
    let startDir = Direction(CardinalDirection.North)
    let commands = input.Split([|", "|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.toList
    let rec calc pos (dir:Direction) (cmds:string list) prevpos duplicate =
        match cmds with
        | [] -> 
            let total = abs (fst pos) + abs (snd pos)
            match duplicate with
            | Some p -> (total, (abs (fst p) + abs (snd p)))
            | None -> (total, 0)
        | head::tail -> 
            let cmddir = head.Chars(0)
            let move direction =
                let steps = int head.[1..] 
                let newpos = List.tail (step pos direction steps)
                let allpos = prevpos @ newpos
                match duplicate with
                | Some p -> calc (Seq.last allpos) direction tail allpos duplicate
                | None -> calc (Seq.last allpos) direction tail allpos (firstduplicate prevpos newpos)
            match cmddir with
            | 'L' -> 
                move dir.Left
            | 'R' -> 
                move dir.Right
            | _ -> failwith "Unknown command"
    calc (0,0) startDir commands [] None

[<EntryPoint>]
let main argv = 
    let result = day1 "L3, R1, L4, L1, L2, R4, L3, L3, R2, R3, L5, R1, R3, L4, L1, L2, R2, R1, L4, L4, R2, L5, R3, R2, R1, L1, L2, R2, R2, L1, L1, R2, R1, L3, L5, R4, L3, R3, R3, L5, L190, L4, R4, R51, L4, R5, R5, R2, L1, L3, R1, R4, L3, R1, R3, L5, L4, R2, R5, R2, L1, L5, L1, L1, R78, L3, R2, L3, R5, L2, R2, R4, L1, L4, R1, R185, R3, L4, L1, L1, L3, R4, L4, L1, R5, L5, L1, R5, L1, R2, L5, L2, R4, R3, L2, R3, R1, L3, L5, L4, R3, L2, L4, L5, L4, R1, L1, R5, L2, R4, R2, R3, L1, L1, L4, L3, R4, L3, L5, R2, L5, L1, L1, R2, R3, L5, L3, L2, L1, L4, R4, R4, L2, R3, R1, L2, R1, L2, L2, R3, R3, L1, R4, L5, L3, R4, R4, R1, L2, L5, L3, R1, R4, L2, R5, R4, R2, L5, L3, R4, R1, L1, R5, L3, R1, R5, L2, R1, L5, L2, R2, L2, L3, R3, R3, R1"
    System.Console.ReadLine() |> ignore
    0