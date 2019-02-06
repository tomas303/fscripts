#!/usr/bin/fsharpi --exec

open System
open System.IO
open System.Text.RegularExpressions
#load "tutils.fsx"
open Tom

type OptCommand =
    | Search
    | Replace
    | Help
type OptFolder = OptFolder of string
type OptRegex = OptRegex of string
type OptReplacement = OptReplacement of string
type OptVerbose =
    | HitFiles
    | AllFiles
type CmdLineOptions = {
    command: OptCommand;
    folder: OptFolder;
    regex: OptRegex;
    replacement: OptReplacement;
    verbose: OptVerbose;
    }


type Matches =
    | Yes of Match list
    | No
    | Error of string

type FileMatches = {
    fileName: string;
    matches: Matches;
}

let main args =

    let regexEmpty(OptRegex regex) =
        regex.Length = 0

    let replacementEmpty(OptReplacement replacement) =
        replacement.Length = 0

    let rec parseArgsRec args opts =
        match args with
        | [] ->
            opts
        | "search"::t ->
            let newopts = { opts with command = Search}
            parseArgsRec t newopts
        | "replace"::t ->
            let newopts = { opts with command = Replace}
            parseArgsRec t newopts
        | ("/h"|"-h"|"/?"|"-?"|"--help"|"help")::t ->
            let newopts = { opts with command = Help}
            parseArgsRec t newopts
        | ("/f"|"-f"|"--folder")::x::t ->
            let newopts = { opts with folder = OptFolder(x)}
            parseArgsRec t newopts
        | ("/r"|"-r"|"--regex")::x::t ->
            let newopts = { opts with regex = OptRegex(x)}
            parseArgsRec t newopts
        | ("/p"|"-p"|"--replacement")::x::t ->
            let newopts = { opts with replacement = OptReplacement(x)}
            parseArgsRec t newopts
        | ("/v"|"-v"|"--verbose")::t ->
            let newopts = { opts with verbose = AllFiles}
            parseArgsRec t newopts
        | x::t when opts.command = Search && regexEmpty opts.regex ->
            let newopts = { opts with regex = OptRegex(x)}
            parseArgsRec t newopts
        | x::t when opts.command = Replace && regexEmpty opts.regex ->
            let newopts = { opts with regex = OptRegex(x)}
            parseArgsRec t newopts
        | x::t when opts.command = Replace && not(regexEmpty opts.regex) && replacementEmpty opts.replacement ->
            let newopts = { opts with replacement = OptReplacement(x)}
            parseArgsRec t newopts
        | x::t ->
            eprintfn "Option '%s' is unrecognized" x
            parseArgsRec t opts

    let defaultOptions = {
        command = Search;
        folder = OptFolder(Directory.GetCurrentDirectory());
        regex = OptRegex("");
        replacement = OptReplacement("");
        verbose = HitFiles;
        }

    let parseArgs args =
        parseArgsRec args defaultOptions

    let options = parseArgs (Array.toList args)

    let fileFind regex filetag =
        //printfn "file: %s" file
        match filetag with
        | FI.OK (FI.FileName file) ->
            try
                let m = RX.find (File.ReadAllText(file)) regex
                match m with
                | [] ->
                    { fileName=file; matches = No }
                | _ ->
                    { fileName=file; matches = Yes m }
            with
            | exn ->
                { fileName=file; matches = Error exn.Message }
        | FI.Error (FI.FileName file, FI.ErrMessage msg) ->
            { fileName=file; matches = Error msg }

    let fileReplace regex replacement filetag =
        match filetag with
        | FI.OK (FI.FileName file) ->
            let m = RX.replace (File.ReadAllText(file)) regex replacement
            File.WriteAllText (file, m)
        | FI.Error (FI.FileName file, FI.ErrMessage msg) -> ()

    let printResult verbose fileMatch =

        match fileMatch.matches with
        | Yes matches ->
            Console.ForegroundColor<-ConsoleColor.Green
            printfn "%s:\t%d" fileMatch.fileName matches.Length
            Console.ForegroundColor<-ConsoleColor.Cyan
            RX.print matches (fun m -> printfn "\t%A" m)
        | No ->
            match verbose with
            | AllFiles ->
                Console.ForegroundColor<-ConsoleColor.Magenta
                printfn "%s:\tnothing found" fileMatch.fileName
                printfn "\n"
            | _ -> ()
        | Error msg ->
            match verbose with
            | AllFiles ->
                Console.ForegroundColor<-ConsoleColor.Red
                printfn "%s:\texception %s" fileMatch.fileName msg
                printfn "\n"
            | _ -> ()

    let search options =
        let (OptRegex regex) = options.regex
        let (OptFolder folder) = options.folder
        let matches = Seq.fold (fun result filetag -> (fileFind regex filetag)::result) [] (FI.files folder)
        List.rev matches |> ignore
        matches |> List.map (printResult options.verbose) |> ignore

    let replace options =
        try
            let (OptRegex regex) = options.regex
            let (OptFolder folder) = options.folder
            let (OptReplacement replacement) = options.replacement
            Seq.iter (fun filetag -> fileReplace regex replacement filetag) (FI.files folder)
        with
        | exn ->
            printfn "Exception e: %s" exn.Message

    let printHelp options =
        printfn "Search and replace based on regular expressions"
        printfn ""
        printfn "Usage: search|replace|help [-d path] [-r regex] [-p replacement]"
        printfn ""
        printfn "\tsearch\t search directory for regex"
        printfn "\treplace\t search and replace with replacement"
        printfn "\thelp\t display help"
        printfn ""
        printfn "\t-h, --help\t same as help"
        printfn "\t-f, --folder\t folder to be searched(including subfolders), default value is current folder"
        printfn "\t-r, --regex\t regular expression to be searched"
        printfn "\t-p, --replacement\t replacement in case of replace command"
        printfn "\t-v, --verbose\t print files where nothing was found or error encountered aswell"
        printfn ""
        printfn "\tshort parameters can also be prefixed with / instead of -"
        printfn ""
        printfn "\tregex parameter value can follow directly search command"
        printfn "\t fsharpi tfind.fsx search \d+\.\d+"
        printfn ""
        printfn "\tregex and replacement parameter value can follow directly replace command"
        printfn "\t fsharpi tfind.fsx replace (\d+)\.(\d+) $2,$1"
        printfn ""

    match options.command with
    | Search -> search options
    | Replace -> replace options
    | _ -> printHelp options

    Log.write options

    0

#if INTERACTIVE
fsi.CommandLineArgs |> Array.toList |> List.tail |> List.toArray |> main
#else
[<EntryPoint>]
let entryPoint args = main args
#endif