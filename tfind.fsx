open System
open System.IO
open System.Text.RegularExpressions
#load "tutils.fsx"
open Tom

type OptDir = OptDir of string
type OptRegex = OptRegex of string
type CmdLineOptions = {
    dir: OptDir;
    regex: OptRegex;

    }


type FindInfo =
    | Yes of string list list
    | No

type FileInfo = {
    fileName: string;
    hits: FindInfo;
}

let main args =

    let rec parseArgsRec args opts =
        match args with
        | [] ->
            opts
        | ("/d"|"-d"|"--directory")::x::t ->
            let newopts = { opts with dir = OptDir(x)}
            parseArgsRec t newopts
        | ("/r"|"-r"|"--regex")::x::t ->
            let newopts = { opts with regex = OptRegex(x)}
            parseArgsRec t newopts
        | x::t ->
            eprintfn "Option '%s' is unrecognized" x
            parseArgsRec t opts

    let defaultOptions = {
        dir = OptDir(Directory.GetCurrentDirectory());
        regex = OptRegex(".*");
        }

    let parseArgs args =
        parseArgsRec args defaultOptions

    let options = parseArgs (Array.toList args)

    let fileTest regex file =

        let rec itest result (m:Match) =
            match m.Success with
            | true ->
                let hit = [ for g in m.Groups -> g.Value ]
                itest (hit::result) (m.NextMatch())
            | false ->
                List.rev result

        let text = File.ReadAllText(file)
        let m = Regex.Match(text, regex)
        //Log.writeVal "test:" file
        let findhits = itest [] m
        //Log.writeVal "findhits:" findhits
        match findhits with
        | [] ->
            { fileName=file; hits = No }
        | _ ->
            { fileName=file; hits = Yes findhits }

    let files =
        let rec ifiles dir =
            seq {
                yield!
                    Directory.EnumerateFiles dir
                yield!
                    Directory.EnumerateDirectories dir
                    |> Seq.map ifiles
                    |> Seq.collect id
            }
        let (OptDir dir) = options.dir
        ifiles dir

    let search =
        let (OptRegex regex) = options.regex
        let result = Seq.fold (fun result file -> (fileTest regex file)::result) [] files
        List.rev result

    let printResult result =

        let rec printHits x =
            match x with
            | [] -> ()
            | h::t ->
                printfn "\t%A" h
                printHits t

        match result.hits with
        | Yes x ->
            Console.ForegroundColor<-ConsoleColor.Green
            printfn "%d in %s" x.Length result.fileName
            Console.ForegroundColor<-ConsoleColor.Cyan
            printHits x
        | No ->
            Console.ForegroundColor<-ConsoleColor.Red
            printfn "0 in %s" result.fileName
            printfn "\n"

    let hits = search
    //Log.write hits
    hits |> List.map printResult |> ignore
    0

#if INTERACTIVE
fsi.CommandLineArgs |> Array.toList |> List.tail |> List.toArray |> main
#else
[<EntryPoint>]
let entryPoint args = main args
#endif