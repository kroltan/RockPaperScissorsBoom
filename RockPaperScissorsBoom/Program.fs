open System

type MatchResult =
    | Won
    | Lost
    | Draw

type Element =
    | Rock
    | Paper
    | Scissors
    | Bomb

type Player = {
    Pick: Element[] -> Element;
    AnnounceResult: MatchResult -> unit;
}

let victoriesOf element =
    match element with
    | Rock -> [Scissors]
    | Paper -> [Rock]
    | Scissors -> [Paper; Bomb]
    | Bomb -> [Rock; Paper]

let evaluate player rival =
    let winsOver a b =
        victoriesOf a
        |> List.contains b
    match (winsOver player rival, winsOver rival player) with
    | (true, false) -> Won
    | (false, true) -> Lost
    | _ -> Draw

let parse options move =
    let cleanup (input: string) = input.ToLowerInvariant().Trim()
    let compare =
        string
        >> cleanup
        >> (fun x -> String.Equals(x, move))
    Seq.tryFind compare options

let runRound (a: Player) (b: Player) options =
    let aChoice = a.Pick options
    let bChoice = b.Pick options
    let result = evaluate aChoice bChoice
    a.AnnounceResult result
    b.AnnounceResult result

let player = {
    Pick = fun options ->
        options
        |> Seq.map string
        |> String.concat ", "
        |> printfn "Choose one of %s:"
        match parse options (Console.ReadLine()) with
        | Some(x) -> x
        | None -> failwith "Unknown choice!";
    AnnounceResult = fun result ->
        match result with
        | Won -> printfn "You won!"
        | Lost -> printfn "You lost!"
        | Draw -> printfn "It's a draw!"
}

let computer = 
    let rng = Random()
    {
        Pick = fun list -> 
            let choice = Array.item (rng.Next(list.Length)) list
            printfn "Computer chose %A" choice
            choice;
        AnnounceResult = fun _ -> ();
    }

[<EntryPoint>]
let main argv =
    let options = [|Rock; Paper; Scissors; Bomb|]

    while true do
        runRound player computer options
    0
