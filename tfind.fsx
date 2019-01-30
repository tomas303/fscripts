open System
open System.IO
open System.Text.RegularExpressions
#load "tutils.fsx"
open Tom

type OptCommand =
    | Search
    | Replace
    | Help
type OptDir = OptDir of string
type OptRegex = OptRegex of string
type OptReplacement = OptReplacement of string
type CmdLineOptions = {
    command: OptCommand;
    dir: OptDir;
    regex: OptRegex;
    replacement: OptReplacement;
    }


type Matches =
    | Yes of Match list
    | No

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
        | ("/d"|"-d"|"--directory")::x::t ->
            let newopts = { opts with dir = OptDir(x)}
            parseArgsRec t newopts
        | ("/r"|"-r"|"--regex")::x::t ->
            let newopts = { opts with regex = OptRegex(x)}
            parseArgsRec t newopts
        | ("/p"|"-p"|"--replacement")::x::t ->
            let newopts = { opts with replacement = OptReplacement(x)}
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
        dir = OptDir(Directory.GetCurrentDirectory());
        regex = OptRegex("");
        replacement = OptReplacement("");
        }

    let parseArgs args =
        parseArgsRec args defaultOptions

    let options = parseArgs (Array.toList args)

    let fileFind regex file =
        let m = RX.find (File.ReadAllText(file)) regex
        match m with
        | [] ->
            { fileName=file; matches = No }
        | _ ->
            { fileName=file; matches = Yes m }

    let fileReplace regex replacement file =
        let m = RX.replace (File.ReadAllText(file)) regex replacement
        File.WriteAllText (file, m)

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

    match options.command with
    | Search ->
        let (OptRegex regex) = options.regex
        let (OptDir dir) = options.dir
        let matches = Seq.fold (fun result file -> (fileFind regex file)::result) [] (FI.files dir)
        List.rev matches |> ignore
        matches |> List.map printResult |> ignore
    | Replace ->
        let (OptRegex regex) = options.regex
        let (OptDir dir) = options.dir
        let (OptReplacement replacement) = options.replacement
        Seq.iter (fun file -> fileReplace regex replacement file) (FI.files dir)
    | _ ->
        printfn "Search and replace based on regular expressions"
        printfn ""
        printfn "Usage: search|replace|help [-d path] [-r regex] [-p replacement]"
        printfn ""
        printfn "\tsearch\t search directory for regex"
        printfn "\treplace\t search and replace with replacement"
        printfn "\thelp\t display help"
        printfn ""
        printfn "\t-h, --help\t same as help"
        printfn "\t-d, --directory\t directory to be searched(including subdirecotory), default value is current directory"
        printfn "\t-r, --regex\t regular expression to be searched"
        printfn "\t-p, --replacement\t replacement in case of replace command"
        printfn ""
        printfn "\tshort parameters can also be prefixed with / instead of -"
        printfn ""
        printfn "\tregex parameter value can follow directly search command"
        printfn "\t fsharpi tfind.fsx search \d+\.\d+"
        printfn ""
        printfn "\tregex and replacement parameter value can follow directly replace command"
        printfn "\t fsharpi tfind.fsx replace (\d+)\.(\d+) $2,$1"
        printfn ""

    0

#if INTERACTIVE
fsi.CommandLineArgs |> Array.toList |> List.tail |> List.toArray |> main
#else
[<EntryPoint>]
let entryPoint args = main args
#endif