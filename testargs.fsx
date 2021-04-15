#!/usr/bin/fsharpi --exec

#load "argsparser.fsx"
open Argsparser

type TestOptions = {
    bo: bool;
    command: string;
    subcmd: string;
    nr: int;
    name: string;
    }

let defaultOptions = {
        bo = false;
        command = "kuk";
        subcmd = "";
        nr = 1;
        name = "";
        }


(*
let opts = defaultOptions
let pcommand = (parg "-cmd") |>> fun x -> { opts with command = x }
let pnr = parg "-nr" |>> fun x -> { opts with nr = int x }
let pnname = parg "-name" |>> fun x -> { opts with name = x }

let testCmd = pcommand .>>. many (pnr <|> pnname)
//let testCmd = many1 (pcommand <|> pnr <|> pnname)

let x = run testCmd ["-cmd"; "kvak"; "-nr"; "6"; "-name"; "bob"]
printfn "result is %A" x
*)

(*
let pcommand = parg "cmd" (fun acum x -> { acum with command = x })
let pnr = parg "nr" (fun acum x -> { acum with nr = int x })
let pnname = parg "name" (fun acum x -> { acum with name = x })

let testCmd = pcommand .>>. many (pnr <|> pnname)
let opts = run testCmd defaultOptions ["-cmd"; "kvak"; "-nr"; "6"; "-name"; "bob"]
*)
let pfind = parg "find" (fun acum x -> { acum with command = x })
let pbo = parg "bo" (fun acum x -> { acum with bo = true })
let psubcmd = parg "*" (fun acum x -> { acum with subcmd = x })
let pnr = parg "nr" (fun acum x -> { acum with nr = int x })
let pnname = parg "name" (fun acum x -> { acum with name = x })
let testCmd = pfind .>>. optional psubcmd .>>. many (pnr <|> pnname <|> pbo)
//let opts = run testCmd defaultOptions ["find"; "somesub"; "-nr"; "6"; "-name"; "bob"]
let opts = run testCmd defaultOptions ["find"; "-nr"; "6"; "-bo"; "-name"; "bob"]



printfn "result is %A" opts



