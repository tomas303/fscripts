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


type Matches =
    | Yes of Match list
    | No

type FileMatches = {
    fileName: string;
    matches: Matches;
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
        let m = RX.find (File.ReadAllText(file)) regex
        match m with
        | [] ->
            { fileName=file; matches = No }
        | _ ->
            { fileName=file; matches = Yes m }

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

    let printResult fileMatch =

        match fileMatch.matches with
        | Yes matches ->
            Console.ForegroundColor<-ConsoleColor.Green
            printfn "%d in %s" matches.Length fileMatch.fileName
            Console.ForegroundColor<-ConsoleColor.Cyan
            RX.print matches (fun m -> printfn "\t%A" m)
        | No ->
            Console.ForegroundColor<-ConsoleColor.Red
            printfn "0 in %s" fileMatch.fileName
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