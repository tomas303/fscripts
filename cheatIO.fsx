open System.IO
#load "tutils.fsx"
open Tom

let enumFiles dir =
    Directory.EnumerateFiles dir
    
let rec enumFiles2 dir =
    seq {
        yield!
            Directory.EnumerateFiles dir
        yield!
            Directory.EnumerateDirectories dir |> Seq.map enumFiles2
            // flatten colections - each call to EnumerateDirectories
            // return just other sequence
            |> Seq.collect id
    }
    

let main argv = 

    let searchHeadAndTail files =
        // seq itself doesnt match against head::tail pattern because of lazy
        // evalution, but head and tail can be used. In case of sequences those
        // 2 functions are very inefective - each tail links to previous seq,
        // which enumerate and use the rest -> do not use them !!!
        let rec internalSearch result files =
            match Seq.isEmpty files with
            | true -> List.rev result
            | false ->
                internalSearch ((Seq.head files)::result) (Seq.tail files)
                
        internalSearch [] files

    let searchHeadAndTailCached files =
        // small variation to previous implementation - problem is still the same,
        // but since sequnces are now cached, evalution is much more quicker
        let rec internalSearch result files =
            match Seq.isEmpty files with
            | true -> List.rev result
            | false ->
                internalSearch ((Seq.head files)::result) (Seq.tail files)
                
        internalSearch [] (Seq.cache files)

    let searchFold files =
        // same implementation as above, but using built in function fold
        // (it passes element and accumulator along - so we need only to join
        // actual element to accumulator). It is as quick as mutable implementation,
        // so in case of usind functional style, this is the best option
        let result = Seq.fold (fun acc el -> el::acc) [] files        
        List.rev result
        
    let searchForLoop files =
        // when enumerate dictly via for loop, it is not possbile to do it without
        // mutable varialbe. Everything is expression and so is for loop, but it
        // returns value is unit, so no result can be passed out
        // but it is quickes enumeration possible I belive with time +- same as
        // with searchFold method, so fold is probably implementd by loop
        let mutable result = [] 
        for file in files do
            result <- file::result
        List.rev result
        
    Log.write ""
    
    Log.write "Recursive implementation using seq head and tail(very inefective):"
    let ls1 = enumFiles2 "/home/toho/Documents/abra/" |> Timer.measure searchHeadAndTail
    Log.writeVal "size" ls1.Length
    
    Log.write "Recursive implementation using seq head and tail cached(not ideal, but way better):"
    let ls2 = enumFiles2 "/home/toho/Documents/abra/" |> Timer.measure searchHeadAndTailCached
    Log.writeVal "size" ls2.Length
    
    Log.write "Recursive implementation using seq fold:"
    let ls3 = enumFiles2 "/home/toho/Documents/abra/" |> Timer.measure searchFold 
    Log.writeVal "size" ls3.Length
    
    Log.write "Mutable implementation by enumerating sequence:"
    let ls4 = enumFiles2 "/home/toho/Documents/abra/" |> Timer.measure searchForLoop 
    Log.writeVal "size" ls4.Length
    
    0

(* 
And here are some numbers from test run with 429 files:

"Recursive implementation using seq head and tail(very inefective):"
CPU time = 7060ms
Absolute time = 7048ms
size=429
"Recursive implementation using seq head and tail cached(not ideal, but way better):"
CPU time = 400ms
Absolute time = 399ms
size=429
"Recursive implementation using seq fold:"
CPU time = 10ms
Absolute time = 13ms
size=429
"Mutable implementation by enumerating sequence:"
CPU time = 20ms
Absolute time = 13ms
size=429    
*)    
    
    
#if INTERACTIVE
fsi.CommandLineArgs |> Array.toList |> List.tail |> List.toArray |> main
#else
[<EntryPoint>]
let entryPoint args = main args
#endif