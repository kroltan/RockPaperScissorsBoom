open System
open System.IO
open FSharp.Json

type Element = {
    Name: string;
    Victories: string[]
}

type Configuration = {
    Elements: Element[];
}

type MatchResult =
    | Won
    | Lost
    | Draw

type Player = {
    Pick: Element[] -> Element;
    AnnounceResult: MatchResult -> unit;
}

let (!) (result: MatchResult) =
    match result with
    | Won -> Lost
    | Lost -> Won
    | x -> x

let winsOver (player: Element) (rival: Element) =
    player.Victories
    |> Array.contains rival.Name

let evaluate player rival =
    match (winsOver player rival, winsOver rival player) with
    | (true, false) -> Won
    | (false, true) -> Lost
    | _ -> Draw

let parse options move =
    let cleanup (input: string) = input.ToLowerInvariant().Trim()
    let compare e =
        e.Name
        |> cleanup
        |> (=) move
    Seq.tryFind compare options

let runRound (a: Player) (b: Player) options =
    let aChoice = a.Pick options
    let bChoice = b.Pick options
    let result = evaluate aChoice bChoice
    a.AnnounceResult result
    b.AnnounceResult !result
    result

let player = {
    Pick = fun options -> 
        let rec choose () =
            options
            |> Seq.map (fun x -> x.Name)
            |> fun xs -> String.Join(", ", xs)
            |> printf "Choose one of %s: "
            match parse options (Console.ReadLine()) with
            | Some(x) -> x
            | None ->
                printfn "Unknown choice!"
                choose ()
        choose ()
    AnnounceResult = function
        | Won -> printfn "You won!"
        | Lost -> printfn "You lost!"
        | Draw -> printfn "It's a draw!"
}

let computer = 
    let rng = Random()
    {
        Pick = fun options -> 
            let choice = Array.item (rng.Next(options.Length)) options
            printfn "Computer chose %s" choice.Name
            choice
        AnnounceResult = fun _ -> ();
    }

let setup =
    File.ReadAllText
    >> Json.deserialize<Configuration>

[<EntryPoint>]
let main argv =
    let config = setup (__SOURCE_DIRECTORY__ + "/config.json")

    while true do
        runRound player computer config.Elements
        |> ignore
    0
