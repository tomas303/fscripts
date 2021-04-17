#!/usr/bin/dotnet fsi

open System
open System.IO
open System.Text.RegularExpressions
#load "tutils.fsx"
open Tom
#load "argsparser.fsx"
open Argsparser

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

    let defaultOptions = {
        command = Search;
        folder = OptFolder(Directory.GetCurrentDirectory());
        regex = OptRegex("");
        replacement = OptReplacement("");
        verbose = HitFiles;
        }

    let pCmdLine =
        let pSearch = parg "search" (fun acum x -> { acum with command = Search })
        let pReplace = parg "replace" (fun acum x -> { acum with command = Replace })
        let pHelp = parg "help" (fun acum x -> { acum with command = Help })
        let pFolder = anyOf ["f";"folder"] (fun acum x -> { acum with folder = OptFolder(x) })
        let pRegex = anyOf ["re";"regex"] (fun acum x -> { acum with regex = OptRegex(x) })
        let pReplacement = anyOf ["p";"replacement"] (fun acum x -> { acum with replacement = OptReplacement(x) })
        let pVerbose = anyOf ["v";"verbose"] (fun acum x -> { acum with verbose = AllFiles })
        let pCmdSearch = pSearch .>>. many (pFolder <|> pRegex <|> pVerbose)
        let pCmdReplace = pReplace .>>. many (pFolder <|> pRegex <|> pReplacement <|> pVerbose)
        let pCmdHelp = pHelp
        let pFolderFluid = parg "*" (fun acum x -> { acum with folder = OptFolder(x) })
        let pRegexFluid = parg "*" (fun acum x -> { acum with regex = OptRegex(x) })
        let pReplacementFluid = parg "*" (fun acum x -> { acum with replacement = OptReplacement(x) })
        let pCmdFluidSearch = pSearch .>>. pRegexFluid .>>. optional(pFolderFluid)
        let pCmdFluidReplace = pReplace .>>. pRegexFluid .>>. pReplacementFluid .>>. optional(pFolderFluid)
        all(pCmdFluidSearch
            <|> pCmdFluidReplace
            <|> pCmdSearch
            <|> pCmdReplace
            <|> pCmdHelp)

    let printHelp () =
        printfn ""
        printfn "Usage:"
        printfn "\tsearch|replace|help [-f path] [-re regex] [-p replacement]"
        printfn "\tsearch regex [path]"
        printfn "\treplace regex replacement [path]"
        printfn ""
        printfn "\tsearch\t searches files in path based on regular expression"
        printfn "\treplace\t replaces searched occurencies with replacement"
        printfn "\thelp\t show help"
        printfn ""
        printfn "\t-f, --folder\t folder to be searched(including subfolders), default value is current folder"
        printfn "\t-re, --regex\t regular expression to be searched"
        printfn "\t-p, --replacement\t replacement in case of replace command"
        printfn "\t-v, --verbose\t print files where nothing was found or error encountered aswell"
        printfn ""
        printfn "\t dotnet fsi tfind.fsx search \d+\.\d+"
        printfn "\t dotnet fsi tfind.fsx replace (\d+)\.(\d+) $2,$1"
        printfn ""

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

    match run pCmdLine defaultOptions (Array.toList args) with
    | Failure err ->
        printfn "%s" err
        printHelp ()
    | Success (options, _) ->
        printfn "%A" options
        match options.command with
        | Search -> search options
        | Replace -> replace options
        | _ -> printHelp ()
    0

#if INTERACTIVE
fsi.CommandLineArgs |> Array.toList |> List.tail |> List.toArray |> main
#else
[<EntryPoint>]
let entryPoint args = main args
#endif