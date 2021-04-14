module Argsparser

open System
open System.Text.RegularExpressions

type ParseResult<'a> =
    | Success of 'a
    | Failure of string

//type Parser<'T> = Parser of (string list -> ParseResult<'T * string list>)
type Parser<'T> = Parser of ('T -> string list -> ParseResult<'T * string list>)


// parser for parse one argument
let parg arg f =
    let innerFn acum args =
        match args with
            | x::y::z when x = arg ->
                Success (f acum y, z)
            | x::z when x = arg ->
                Success (f acum "", z)
            | [] ->
                let msg = "No more input"
                Failure msg
            | _ ->
                let msg = sprintf "Expecting '%s'. Got '%s'" arg args.Head
                Failure msg
    Parser innerFn


// run parser on input
let run parser acum args =
    let (Parser innerFn) = parser
    innerFn acum args


// match of 2 parsers
let andThen parser1 parser2 =
    let innerFn acum args =
        let result1 = run parser1 acum args
        match result1 with
        | Failure err ->
            Failure err
        | Success (value1, remaining1) ->
            let result2 =  run parser2 value1 remaining1
            match result2 with
            | Failure err ->
                Failure err
            | Success (value2,remaining2) ->
                Success (value2,remaining2)
    Parser innerFn

let ( .>>. ) = andThen


// or match of 2 parsers
let orElse parser1 parser2 =
    let innerFn acum args =
        let result1 = run parser1 acum args
        match result1 with
        | Success result ->
            result1
        | Failure err ->
            let result2 = run parser2 acum args
            result2
    Parser innerFn

let ( <|> ) = orElse

(*
let choice listOfParsers =
    List.reduce ( <|> ) listOfParsers


let anyOf listOfArgs =
    listOfArgs
    |> List.map parg // convert into parsers
    |> choice         // combine them


let mapP f parser =
    let innerFn input =
        let result = run parser input
        match result with
        | Success (value,remaining) ->
            let newValue = f value
            Success (newValue, remaining)
        | Failure err ->
            Failure err
    Parser innerFn

let ( <!> ) = mapP
let ( |>> ) x f = mapP f x
*)

// helper function for zero or more match
let rec parseZeroOrMore parser acum args =
    let firstResult = run parser acum args
    match firstResult with
    | Failure err ->
        (acum, args)  // key, always success
    | Success (firstValue,inputAfterFirstParse) ->
        let (subsequentValues,remainingInput) =
            parseZeroOrMore parser firstValue inputAfterFirstParse
        (subsequentValues,remainingInput)


let many parser =
    let innerFn acum args =
        // parse the input -- wrap in Success as it always succeeds
        Success (parseZeroOrMore parser acum args)
    Parser innerFn
