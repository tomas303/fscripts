open System
open System.Text.RegularExpressions
#load "tutils.fsx"
open Tom


let main argv =
    Log.write "regex cheatsheet"

    let text = "yellow horse"
    let m = Regex.Match(text, "\w+")
    Log.write [ for g in m.Groups -> g.Value ]
    let m = m.NextMatch()
    Log.write [ for g in m.Groups -> g.Value ]

    let (|RegexContains|_|) pattern input =
        let matches = Regex.Matches(input, pattern)
        if matches.Count > 0 then Some [ for m in matches -> m.Value ]
        else None

    match "whale" with
    | RegexContains "\d+" value -> Log.write "cislo"
    | RegexContains "\w+" value -> Log.write "slovo"
    | _ -> Log.write "neco jineho"



    let (|RegexMatch|_|) pattern input =
        let matches = Regex.Matches(input, pattern)
        Log.writeVal "pattern" pattern
        Log.writeVal "mc" matches.Count
        if matches.Count > 0 then Some [ for m in matches -> m ]
        else None

    match "whale|shark" with
    | RegexMatch "\d+" rm -> Log.write rm
    | RegexMatch "\w+" rm -> Log.write rm
    | _ -> Log.write "neco jineho"




    0


#if INTERACTIVE
fsi.CommandLineArgs |> Array.toList |> List.tail |> List.toArray |> main
#else
[<EntryPoint>]
let entryPoint args = main args
#endif
