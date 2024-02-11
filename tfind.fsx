#!/usr/bin/dotnet fsi

open System
open System.IO
open System.Threading
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
type OptTechnique =
    | Single
    | ParallelByMailbox
    | ParallelByChannel
type OptParallels = OptParallels of int
type CmdLineOptions = {
    command: OptCommand;
    folder: OptFolder;
    regex: OptRegex;
    replacement: OptReplacement;
    verbose: OptVerbose;
    technique: OptTechnique;
    parallels: OptParallels;
    }


type Matches =
    | Yes of Match list
    | No
    | Error of string

type FileMatches = {
    fileName: string;
    matches: Matches;
}



type JobKind =
    | JKSearch of FI.FileTag * OptRegex * OptVerbose
    | JKReplace of FI.FileTag * OptRegex * OptReplacement * OptVerbose
    | JKHelp
    | JKFinito
    | JKNoWork

type CoordinatorMessage =
    | Job of JobKind
    | RequestJob of AsyncReplyChannel<JobKind>
    | JobFinished of FileMatches * OptVerbose
    | NoMoreJobs

let main args =

    let defaultOptions = {
        command = Search;
        folder = OptFolder (Directory.GetCurrentDirectory());
        regex = OptRegex "";
        replacement = OptReplacement "";
        verbose = HitFiles;
        technique = Single;
        parallels = OptParallels 0;
        }

    let pCmdLine =
        let pSearch = parg "search" (fun acum x -> { acum with command = Search })
        let pReplace = parg "replace" (fun acum x -> { acum with command = Replace })
        let pHelp = parg "help" (fun acum x -> { acum with command = Help })
        let pFolder = anyOf ["f";"folder"] (fun acum x -> { acum with folder = OptFolder(x) })
        let pRegex = anyOf ["re";"regex"] (fun acum x -> { acum with regex = OptRegex(x) })
        let pReplacement = anyOf ["p";"replacement"] (fun acum x -> { acum with replacement = OptReplacement(x) })
        let pVerbose = anyOf ["v";"verbose"] (fun acum x -> { acum with verbose = AllFiles })
        let pTechnique = anyOf ["t";"technique"] (fun acum x -> { acum with technique = if x = "mb" then ParallelByMailbox elif x = "ch" then ParallelByChannel else  Single})
        let pParallels = anyOf ["j";"parallels"] (fun acum x -> { acum with parallels = OptParallels(int(x)) })
        let pCmdSearch = pSearch .>>. many (pFolder <|> pRegex <|> pVerbose <|> pTechnique <|> pParallels)
        let pCmdReplace = pReplace .>>. many (pFolder <|> pRegex <|> pReplacement <|> pVerbose <|> pTechnique <|> pParallels)
        let pCmdHelp = pHelp
        let pFolderFluid = parg "*" (fun acum x -> { acum with folder = OptFolder(x) })
        let pRegexFluid = parg "*" (fun acum x -> { acum with regex = OptRegex(x) })
        let pReplacementFluid = parg "*" (fun acum x -> { acum with replacement = OptReplacement(x) })
        let pCmdFluidSearch = pSearch .>>. pRegexFluid .>>. optional(pFolderFluid) .>>. many (pVerbose <|> pTechnique <|> pParallels)
        let pCmdFluidReplace = pReplace .>>. pRegexFluid .>>. pReplacementFluid .>>. optional(pFolderFluid) .>>. many (pVerbose <|> pTechnique <|> pParallels)
        all(pCmdFluidSearch
            <|> pCmdFluidReplace
            <|> pCmdSearch
            <|> pCmdReplace
            <|> pCmdHelp)

    let printHelp () =
        printfn ""
        printfn "Usage:"
        printfn "\tsearch|replace|help [-f path] [-re regex] [-p replacement] [-j parallels]"
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
        printfn "\t-t, --technique\t internal technique for search(mb - parallel by use of mailboxes, ch - parallel by channel, si - non parallel)"
        printfn "\t-j, --parallels\t number of workers searching for match(zero is default, means no workers)"
        printfn "\t\t\tnow experimental to find out about MailboxProcessors what are used to implement workers"
        printfn ""
        printfn "\t dotnet fsi tfind.fsx search \d+\.\d+"
        printfn "\t dotnet fsi tfind.fsx replace (\d+)\.(\d+) $2,$1"
        printfn ""

    let fileFind regex filetag =
        // printfn "file: %A" filetag
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
        let (OptParallels parallels) = options.parallels
        match options.technique with
        | Single ->
            let matches = Seq.fold (fun result filetag -> (fileFind regex filetag)::result) [] (FI.files folder)
            List.rev matches |> ignore
            matches |> List.map (printResult options.verbose) |> ignore
        | ParallelByMailbox ->
            let searcher = new MBX.Coordinator<FI.FileTag, FileMatches>(fileFind regex, parallels)
            Observable.add (printResult options.verbose) searcher
            Seq.iter (fun filetag -> searcher.PostJob(filetag) |> ignore) (FI.files folder)
            searcher.PostFinished()
            searcher.WaitAllJobs()
        | ParallelByChannel ->
            let runner = new CH.Runner<FI.FileTag, FileMatches>(fileFind regex, parallels)
            Observable.add (printResult options.verbose) runner
            Seq.iter (fun filetag -> runner.PostJob(filetag) |> ignore) (FI.files folder)
            runner.PostFinished()
            runner.WaitAllJobs()


    let replace options =
        try
            let (OptRegex regex) = options.regex
            let (OptFolder folder) = options.folder
            let (OptReplacement replacement) = options.replacement
            Seq.iter (fun filetag -> fileReplace regex replacement filetag) (FI.files folder)
        with
        | exn ->
            printfn "Exception e: %s" exn.Message

    match run pCmdLine defaultOptions args with
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
fsi.CommandLineArgs |> Array.toList |> List.tail |> main
// ["search"; "let"] |> main
// ["search"; "let"; "-j"; "4"] |> main
#else
[<EntryPoint>]
let entryPoint args = main args
#endif