namespace Tom

open System.Diagnostics
open System.Text.RegularExpressions

module Log =

    let write x =
        printfn "%A" x

    let writeVal n v =
        printfn "%s=%A" n v

module Timer =

    let measure f x =
        let proc = Process.GetCurrentProcess()
        let cpu_time_stamp = proc.TotalProcessorTime
        let timer = new Stopwatch()
        timer.Start()
        try
            f x
        finally
            let cpu_time = (proc.TotalProcessorTime-cpu_time_stamp).TotalMilliseconds
            printfn "CPU time = %dms" (int64 cpu_time)
            printfn "Absolute time = %dms" timer.ElapsedMilliseconds

    let measure2 f =
        let proc = Process.GetCurrentProcess()
        let cpu_time_stamp = proc.TotalProcessorTime
        let timer = new Stopwatch()
        timer.Start()
        try
            f
        finally
            let cpu_time = (proc.TotalProcessorTime-cpu_time_stamp).TotalMilliseconds
            printfn "CPU time = %dms" (int64 cpu_time)
            printfn "Absolute time = %dms" timer.ElapsedMilliseconds

    let repeatit n f x =
        for i in 1 .. n-1 do
            f x |> ignore
        f x

module RX =

    let (|RegexMatch|_|) pattern input =
        let matches = Regex.Matches(input, pattern)
        if matches.Count > 0 then Some [ for m in matches -> m ]
        else None

    let find input pattern =
        match input with
        | RegexMatch pattern m ->
            m
        | _ -> []

    let print m f =

        let rec iprint m =
            match m with
            | h::t ->
                f h
                iprint t
            | [] -> ()

        iprint m

    let replace (input: string) pattern replacement =

        let rec ireplace (m: Match list) output pos =
            match m with
            | h::t ->
                let newoutput = output + input.[pos..h.Index-1] + (h.Result replacement)
                ireplace t newoutput (h.Index + h.Value.Length)
            | [] -> output

        match input with
        | RegexMatch pattern m ->
            ireplace m "" 0
        | _ -> input

