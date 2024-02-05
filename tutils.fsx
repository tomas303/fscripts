namespace Tom

open System
open System.IO
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

module FI =

    type FileName = FileName of string
    type ErrMessage = ErrMessage of string

    type FileTag =
        | OK of FileName
        | Error of FileName * ErrMessage

    let isDirAccessible dir =
        try
            Directory.EnumerateFiles dir |> ignore
            (true, "")
        with
        | exn -> (false, exn.Message)

    let files dir =

        let rec ifiles dir =
            seq {
                match isDirAccessible dir with
                | (true,  msg) ->
                    for file in Directory.EnumerateFiles dir do
                        yield OK (FileName file)
                    for dir in Directory.EnumerateDirectories dir do
                        yield! ifiles dir
                | (false,  msg) ->
                    yield Error (FileName dir, ErrMessage msg)
            }

        ifiles dir

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

module MBX =

    type SigMessage =
    | Signal
    | Wait of AsyncReplyChannel<unit>

    type Signaller() = 

        let mutable signaled = false
        let mutable waitChannel = None

        let mb = MailboxProcessor<SigMessage>.Start(fun inbox ->
            let rec loop () = async {
                let! msg = inbox.Receive()
                // printfn "\nsignaller message: %A" msg |> ignore
                match msg with
                | Wait replyChannel ->
                    match signaled with
                    | true -> replyChannel.Reply()
                    | false -> waitChannel <- Some(replyChannel)
                | Signal ->
                    match waitChannel with
                    | Some x -> x.Reply()
                    | None -> signaled <- true
                return! loop ()
            }
            loop ()
        )
        
        member this.Signal() = 
            mb.Post(Signal)

        member this.Wait() = 
            mb.PostAndReply(fun reply -> Wait reply)


    type CoordinatorObservable<'a>() =
        let observers = Collections.Generic.List<IObserver<'a>>()  
        
        interface System.IObservable<'a> with
            member this.Subscribe observer =
                observers.Add observer
                { new System.IDisposable with 
                    member this.Dispose() = 
                        observers.Remove observer |> ignore 
                }
        member this.NotifyNext(value) =
            for x in observers do
                try
                    x.OnNext value
                with e ->
                    ignore()
        member this.NotifyError(ex) =
            for x in observers do
                try
                    x.OnError ex
                with e ->
                    ignore()
        member this.NotifyCompleted() =
            for x in observers do
                try
                    x.OnCompleted ()
                with e ->
                    ignore()

    type Coordinator<'a, 'b> (jobFunc, workerCount) as this =
        let jobFunc = jobFunc
        let workers = [1..workerCount] |> List.map (fun _ -> new Worker<'a, 'b>(this))
        let queue = new System.Collections.Generic.Queue<'a>()
        let signaller = new Signaller()
        let coordinatorObservable = new CoordinatorObservable<'b>()
        let mutable stop = false
        let mutable tasks = 0

        

        let mb = MailboxProcessor<CoordinatorMessage<'a, 'b>>.Start(fun inbox ->
            let rec loop () = async {
                let! msg = inbox.Receive()
                // printfn $"stop: {stop} tasks: {tasks}   message: %A{message}\n" |> ignore
                match msg with
                | Job x ->
                    queue.Enqueue x
                    tasks <- tasks + 1
                | RequestJob repch ->
                    if stop && tasks=0 then
                        repch.Reply JKFinito
                    else
                        if queue.Count = 0 then
                            // printf "nojobs\n" |> ignore
                            repch.Reply JKNoWork
                        else
                            repch.Reply (JKJob (jobFunc, queue.Dequeue()))
                | JobFinished x ->
                    coordinatorObservable.NotifyNext x
                    tasks <- tasks-1
                    if stop && tasks=0 then
                        coordinatorObservable.NotifyCompleted()
                        signaller.Signal() |> ignore
                | NoMoreJobs -> 
                    stop <- true
                return! loop ()
            }
            loop ())

        interface System.IObservable<'b> with
            member this.Subscribe(observer) =
                (coordinatorObservable :> System.IObservable<'b>).Subscribe(observer)
        member this.PostJob(x) =
            mb.Post(Job(x))
        member this.PostFinished() =
            mb.Post(NoMoreJobs)
        member this.GetJob() =
            mb.PostAndReply (fun repch -> RequestJob repch)
        member this.JobFinished(x) =
            mb.Post (JobFinished(x))
        member this.WaitAllJobs() =
            signaller.Wait()

    and JobFunc<'a, 'b> = 'a -> 'b
    and JobRun<'a, 'b> = JobFunc<'a, 'b> * 'a

    and WorkerMessage<'a, 'b> =
    | JKJob of JobRun<'a, 'b>
    | JKFinito
    | JKNoWork
    
    and CoordinatorMessage<'a, 'b> =
    | Job of 'a
    | RequestJob of AsyncReplyChannel<WorkerMessage<'a, 'b>>
    | JobFinished of 'b
    | NoMoreJobs

    and Worker<'a, 'b>(coordinator: Coordinator<'a, 'b>) =
        let coordinator = coordinator
        let mb = MailboxProcessor.Start(fun inbox -> 
            let rec loop () = async {
                let job = coordinator.GetJob()
                // printfn "\njobkind: %A\n" jobkind |> ignore
                match job with
                | JKJob (f, x) ->
                    coordinator.JobFinished(f(x))
                    return! loop ()
                | JKFinito ->
                    return ()
                | JKNoWork -> 
                    do! Async.Sleep(10)
                    return! loop ()
            }
            loop ()
        )
