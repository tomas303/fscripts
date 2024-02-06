#!/usr/bin/dotnet fsi

open System
// open FSharp.Control.Reactive
// open System.Reactive.Disposables
// open System.Reactive.Linq

//observable outside only subscription
// coordinator will call it ... and depend if was subscribef or not

type mx<'a>() =
    interface IDisposable with
        member this.Dispose() =
            printf "disposed"

    interface IObserver<'a> with
        member this.OnCompleted() =
            printf "completed"
        
        member this.OnError(error) =
            printfn "error %A" error

        member this.OnNext(value) =
            printfn "value %A" value

    member this.text<'b>() =
        printf "test method"

type mo<'a>() =
    interface IObservable<'a> with
        member this.Subscribe observer =
            printf "ahoj"
            { new System.IDisposable with 
                member this.Dispose() = 
                    printf "disposed" 
            }
