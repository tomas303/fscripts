open System
open System.IO
open System.Text.RegularExpressions

type OptDir = OptDir of string
type OptRegex = OptRegex of string
type CmdLineOptions = {
    dir: OptDir;
    regex: OptRegex;
}

type FindInfo =
    | Yes of string list
    | No

type FileInfo = {
    fileName: string;
    hits: FindInfo;
}
    
let main args = 
    
    let consoleLogger argName argValue = 
        printfn "%s=%A" argName argValue 
    
    let logger = consoleLogger    
    
    let rec parseArgsRec logger args opts =
        //logger "args" args
        match args with
        | [] ->
            opts
        | ("/d"|"-d"|"--directory")::x::t ->
            let newopts = { opts with dir = OptDir(x)}
            parseArgsRec logger t newopts
        | ("/r"|"-r"|"--regex")::x::t ->
            let newopts = { opts with regex = OptRegex(x)}
            parseArgsRec logger t newopts
        | x::t ->
            eprintfn "Option '%s' is unrecognized" x
            parseArgsRec logger t opts            
    
    let defaultOptions = {
        dir = OptDir(Directory.GetCurrentDirectory());
        regex = OptRegex(".*");
        }
        
    let parseArgs args =
        parseArgsRec logger args defaultOptions            
            
    let options = parseArgs (Array.toList args)
    //logger "options" options
    
    let fileTest regex file =
        let text = File.ReadAllText(file)
        let m = Regex.Match(text, regex)
        if m.Success then 
            //logger "groups" m.Groups
            { fileName=file; hits = Yes [ for g in m.Groups -> g.Value ] }
        else 
            { fileName=file; hits = No }
    
    let rec fileSearch job dir = 
        let hits = Directory.GetFiles dir |> Array.map job
        let subhits = Directory.GetDirectories dir |> Array.map (fileSearch job) |> Array.concat
        Array.concat [ hits;subhits ]
        
    let search =
        let (OptDir dir) = options.dir
        let (OptRegex regex) = options.regex
        fileSearch (fileTest regex) dir

    let printResult result =
        match result.hits with
        | Yes x ->
            printfn "%d in %s" x.Length result.fileName
            printfn "\t%A" x
        | No -> printfn "0 in %s" result.fileName
    
    let hits = search
    //logger "hits" hits
    hits |> Array.map printResult |> ignore 
    
    0
    
#if INTERACTIVE
fsi.CommandLineArgs |> Array.toList |> List.tail |> List.toArray |> main
#else
[<EntryPoint>]
let entryPoint args = main args
#endif