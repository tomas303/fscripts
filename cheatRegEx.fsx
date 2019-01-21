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

    0


#if INTERACTIVE
fsi.CommandLineArgs |> Array.toList |> List.tail |> List.toArray |> main
#else
[<EntryPoint>]
let entryPoint args = main args
#endif
