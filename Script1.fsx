// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

open System

type terminal = 
    Add | Sub | Rem | Mul | Div | Exp | Lpar | Rpar | Dot | Num of int | Flt of float

let str2lst s = [for c in s -> c]
let isBlank c = System.Char.IsWhiteSpace c
let isDigit c = System.Char.IsDigit c

let digitCount number = (int) (log10 ((float) number)) + 1;
let lexError = System.Exception("Lexer error")
let intVal (c:char) = (int)((int)c - (int)'0')
let parseError = System.Exception("Parser error")

let rec scInt(iStr, iVal) = 
    match iStr with
    c :: tail when isDigit c -> scInt(tail, 10*iVal+(intVal c))
    | _ -> (iStr, iVal)
    
let scFloat(iStr, iVal) =
    let (tail, whole) = scInt(iStr, iVal)
    match tail with
    | '.' :: ntail -> let (rem, fractional) = scInt(ntail, 0)
                      let value = (float) whole + ((0.1**digitCount fractional) * (float) fractional)
                      (rem, value)
    | _ -> (tail, (float) whole)

let lexer input = 
    let rec scan input =
        match input with
        | [] -> []
        | '+'::tail -> Add :: scan tail
        | '-'::tail -> Sub :: scan tail
        | '%'::tail -> Rem :: scan tail
        | '*'::tail -> Mul :: scan tail
        | '/'::tail -> Div :: scan tail
        | '^'::tail -> Exp :: scan tail
        | '('::tail -> Lpar:: scan tail
        | ')'::tail -> Rpar:: scan tail
        | '.'::tail -> Dot :: scan tail
        | c :: tail when isBlank c -> scan tail
        | c :: tail when isDigit c -> let (iStr, iVal) = scFloat(tail, intVal c)
                                      if iVal = ((float) ((int) iVal)) then
                                          Num ((int) iVal) :: scan iStr
                                      else
                                          Flt iVal :: scan iStr
        | _ -> raise lexError
    scan (str2lst input)

let getInputString() : string = 
    Console.Write("Enter an expression: ")
    Console.ReadLine()

// Grammar in BNF:
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <I> <Topt>
// <Topt>     ::= "%" <I> <Topt> | "*" <I> <Topt> | "/" <I> <Topt> | <empty>
// <I>        ::= <VALf> <Iopt>
// <Iopt>     ::= "^" <VALf> <Iopt> | <empty>
// <VAL>     ::= "+" <FLT> | "-" <FLT> | <FLT>
// <FLT>      ::= "Flt" <value> | "(" <E> ")" | NR
// <NR>       ::= "Num" <value> | "(" <E> ")"

let parser tList = 
    let rec E tList = (T >> Eopt) tList         // >> is forward function composition operator: let inline (>>) f g x = g(f x)
    and Eopt tList = 
        match tList with
        | Add :: tail -> (T >> Eopt) tail
        | Sub :: tail -> (T >> Eopt) tail
        | _ -> tList
    and T tList = (I >> Topt) tList
    and Topt tList =
        match tList with
        | Rem :: tail -> (I >> Topt) tail
        | Mul :: tail -> (I >> Topt) tail
        | Div :: tail -> (I >> Topt) tail
        | _ -> tList
    and I tList = (VAL >> Iopt) tList
    and Iopt tList =
        match tList with
        | Exp :: tail -> (VAL >> Iopt) tail
        | _ -> tList
    and VAL tList =
        match tList with
        | Add :: tail -> FLT tail
        | Sub :: tail -> FLT tail
        | _ -> FLT tList
    and FLT tList =
        match tList with 
        | Flt _ :: tail -> tail
        | Lpar :: tail -> match E tail with 
                          | Rpar :: tail -> tail
                          | _ -> raise parseError
        | _ -> NR tList
    and NR tList =
        match tList with 
        | Num _ :: tail -> tail
        | Lpar :: tail -> match E tail with 
                          | Rpar :: tail -> tail
                          | _ -> raise parseError
        | _ -> raise parseError
    E tList
    
let parseNeval tList = 
    let rec E tList = (T >> Eopt) tList
    and Eopt (tList, value) = 
        match tList with
        | Add :: tail -> let (tLst, tval) = T tail
                         Eopt (tLst, value + tval)
        | Sub :: tail -> let (tLst, tval) = T tail
                         Eopt (tLst, value - tval)
        | _ -> (tList, value)
    and T tList = (I >> Topt) tList
    and Topt (tList, value) =
        match tList with
        | Rem :: tail -> if value = ((float) ((int) value)) then
                             
                             let (tLst, tval) = I tail
                             
                             if tval = 0 then raise parseError
                             
                             if tval = ((float) ((int) tval)) then
                                 Topt (tLst, value % tval)
                             else
                                 raise parseError
                         else
                             raise parseError
        | Mul :: tail -> let (tLst, tval) = I tail
                         Topt (tLst, value * tval)
        | Div :: tail -> let (tLst, tval) = I tail
                         if tval = 0 then raise parseError
                         Topt (tLst, value / tval)
        | _ -> (tList, value)
    and I tList = (VAL >> Iopt) tList
    and Iopt (tList, value) =
        match tList with
        | Exp :: tail -> let (tLst, tval) = VAL tail
                         Iopt (tLst, value ** tval)
        | _ -> (tList, value)
    and VAL tList =
        match tList with
        | Add :: tail -> let (tLst, tval) = FLT tail
                         (tLst, tval)
        | Sub :: tail -> let (tLst, tval) = FLT tail
                         (tLst, -tval)
        | _           -> let (tLst, tval) = FLT tList
                         (tLst, tval)
    and FLT tList =
        match tList with 
        | Flt value :: tail -> (tail, value)
        | Lpar :: tail -> let (tLst, tval) = E tail
                          match tLst with 
                          | Rpar :: tail -> (tail, tval)
                          | _ -> raise parseError
        | _ -> NR tList
    and NR tList =
        match tList with 
        | Num value :: tail -> (tail, value)
        | Lpar :: tail -> let (tLst, tval) = E tail
                          match tLst with 
                          | Rpar :: tail -> (tail, (int) tval)
                          | _ -> raise parseError
        | _ -> raise parseError
    E tList

let rec printTList (lst:list<terminal>) : list<string> = 
    match lst with
    head::tail -> Console.Write("{0} ",head.ToString())
                  printTList tail
                  
    | [] -> Console.Write("EOL\n")
            []

[<EntryPoint>]
let main argv  =
    Console.WriteLine("Simple Interpreter")
    let input:string = getInputString()
    let oList = lexer input
    let sList = printTList oList;
    let pList = printTList (parser oList)
    let Out = parseNeval oList
    Console.WriteLine("Result = {0}", snd Out)
    0
