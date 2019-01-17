namespace Tom

open System.Diagnostics

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
