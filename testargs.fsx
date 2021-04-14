#!/usr/bin/fsharpi --exec

#load "argsparser.fsx"
open Argsparser

type TestOptions = {
    command: string;
    nr: int;
    name: string;
    }

let defaultOptions = {
        command = "kuk";
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

let pcommand = parg "-cmd" (fun acum x -> { acum with command = x })
let pnr = parg "-nr" (fun acum x -> { acum with nr = int x })
let pnname = parg "-name" (fun acum x -> { acum with name = x })

let testCmd = pcommand .>>. many (pnr <|> pnname)
let opts = run testCmd defaultOptions ["-cmd"; "kvak"; "-nr"; "6"; "-name"; "bob"]

printfn "result is %A" opts