module Argsparser

open System
open System.Text.RegularExpressions

type ParseResult<'a> =
    | Success of 'a
    | Failure of string

type Parser<'T> = Parser of ('T -> string list -> ParseResult<'T * string list>)


let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let argmatch x y =
    match y with
    | Prefix "--" z -> z = x
    | Prefix "-" z -> z = x
    | _ -> false


let isarg x =
    match x with
    | Prefix "--" z -> true
    | Prefix "-" z -> true
    | _ -> false


/// parser for parse one argument arg, f takes accumulator and parsed value and
/// produces new accumaltor
let parg arg f =
    let innerFn acum args =
        match args with
            | x::y::z when (argmatch arg x) && not (isarg y) ->
                Success (f acum y, z)       //param and value
            | x::z when argmatch arg x ->
                Success (f acum "", z)      //param without value(exists)
            | x::z when arg = x ->
                Success (f acum x, z)       //command
            | x::z when arg = "*" && not (isarg x) ->
                Success (f acum x, z)       //wild argument for fluid input
            | [] ->
                let msg = "No more input"
                Failure msg
            | _ ->
                let msg = sprintf "Expecting '%s'. Got '%s'" arg args.Head
                Failure msg
    Parser innerFn


/// helper function for run parser
let run parser acum args =
    let (Parser innerFn) = parser
    innerFn acum args


/// will succeed when match both parsers
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


/// will succeed when match any parser
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


/// will succeed when match any of givev parsers
let choice listOfParsers =
  List.reduce ( <|> ) listOfParsers


/// Choose any of a list of characters
let anyOf listOfArgs f =
  listOfArgs
  |> List.map ( fun x -> parg x f ) // convert into parsers
  |> choice         // combine them


/// helper function for repetitive match
let rec parseZeroOrMore parser acum args =
    let firstResult = run parser acum args
    match firstResult with
    | Failure err ->
        (acum, args)  // key, always success
    | Success (firstValue,inputAfterFirstParse) ->
        if inputAfterFirstParse.Length < args.Length then
            parseZeroOrMore parser firstValue inputAfterFirstParse
        else
            (firstValue,inputAfterFirstParse)

/// will succeed when match zero or more times
let many parser =
    let innerFn acum args =
        // parse the input -- wrap in Success as it always succeeds
        Success (parseZeroOrMore parser acum args)
    Parser innerFn


/// will succeed when parse all arguments
let all parser =
    let innerFn acum args =
        let result = run parser acum args
        match result with
        | Failure err ->
            Failure err
        | Success (value, rest) ->
            match rest with
            | [] ->
                Success (value, rest)
            | _ ->
                let msg = sprintf "Unknown argument '%s'" rest.Head
                Failure msg
    Parser innerFn


/// helper function for optional parse
let parseZeroOrOne parser acum args =
    let firstResult = run parser acum args
    match firstResult with
    | Failure err ->
        (acum, args)  // key, always success
    | Success (firstValue,inputAfterFirstParse) ->
        (firstValue,inputAfterFirstParse)


/// will succeed when match zero or one time
let optional parser =
    let innerFn acum args =
        // parse the input -- wrap in Success as it always succeeds
        Success (parseZeroOrOne parser acum args)
    Parser innerFn
