module Parser

open FParsec

type Value = 
    | Variable of string
    | Int of int64
    | Float of float
    | String of string
    | Boolean of bool
    | Null
    | Enum of string
    | List of Value list
    | Object of ObjectField list
and ObjectField = {
    Name : string
    Value : Value
}

type Argument = {
    Name : string
    Value : Value
}

type Directive = {
    Name : string
    Arguments : Argument list
}

type Selection =
    | Field of FieldStruct
    | FragmentSpread of FragmentSpreadStruct
    | InlineFragment of InlineSpreadStruct
and FieldStruct = {
    Alias : string option
    Name : string
    Arguments : Argument list
    Directives : Directive list
    SelectionSet : Selection list
}
and FragmentSpreadStruct = {
    FragmentName : string
    Directives : Directive list
}
and InlineSpreadStruct = {
    TypeCondition : string option
    Directives : Directive list
    SelectionSet : Selection list
}

type OperationType =
    | Query
    | Mutation

type Type = 
    | NamedType of string
    | ListType of Type
    | NonNullType of Type

type VariableDefinition = {
    Name : string
    Type : Type
    DefaultValue : Value option
}

type Definition = 
    | OperationDefinition of OperationStruct
    | FragmentDefinition of FragmentStruct
and OperationStruct = {
    Type : OperationType
    Name : string option
    VariableDefinitions : VariableDefinition list
    Directives : Directive list
    SelectionSet : Selection list
}
and FragmentStruct = {
    Name : string
    TypeCondition : string
    Directives : Directive list
    SelectionSet : Selection list
}

let openSq = skipChar '['

let closeSq = skipChar ']'

let openP = skipChar '('

let closeP = skipChar ')'

let optList p = (attempt p) <|>% []

let separator = spaces >>. optional (skipChar ',') >>. spaces

let bp (p : Parser<'a, 'u>) (stream : CharStream<'u>) = 
    let r = p stream
    printfn "%A" (if r.Status = Error then "err" else r.Result.ToString())
    r

let name = 
    many1Satisfy2 
        (fun c -> c = '_' || isAsciiLetter c) 
        (fun c -> c = '_' || isAsciiLetter c || isDigit c)
    .>> spaces

let variable = skipChar '$' >>. name

let numericValue = pfloat |>> function | v when System.Math.Truncate(v) = v -> Int((int64)v) | v -> Float(v) 
let booleanValue = (stringCIReturn "true" true) <|> (stringCIReturn "false" false)
let nullValue = skipString "null"
let quote = skipChar '"'
let stringValue = 
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape = function
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pchar '\\' >>. (anyOf "\\nrt\"" |>> unescape)
    between quote quote
            (manyChars (normalChar <|> escapedChar))

let enumValue = name

let value = 
    choice [
        variable |>> Variable
        nullValue >>% Null;
        numericValue;
        booleanValue |>> Boolean;
        stringValue |>> String;
        enumValue |>> Enum;
    ]

let operationType = 
    ((stringReturn "query" Query) <|> stringReturn "mutation" Mutation) .>> spaces

let nextChar (p : Parser<'a, 'u>) c m n (stream : CharStream<'u>) = 
    match p stream with
    | success when success.Status = ReplyStatus.Ok ->
        if stream.Peek() = c
        then 
            stream.Read() |> ignore
            Reply(m(success.Result))
        else Reply(n(success.Result))
    | error -> Reply(Error, error.Error)

//let commentChars = restOfLine true

//let comment = skipChar '#' >>. commentChars

let alias = name .>> skipChar ':' .>> spaces

let typeCondition = skipString "on " >>. spaces >>. name

let ptype, ptypeImpl = createParserForwardedToRef()

let namedType = 
    nextChar (name .>> spaces) '!' (fun x -> NonNullType(NamedType(x))) (fun x -> NamedType(x))

let listType = 
    nextChar (between openSq closeSq ptype) '!' (fun x -> NonNullType(ListType(x))) (fun x -> ListType(x))

let defaultValue = spaces .>> skipChar '=' .>> spaces >>. value

do ptypeImpl := 
    spaces >>.
    choice [
        listType;
        namedType;
    ]
    .>> spaces

let variableDefinition = 
    tuple3 (variable .>> skipChar ':') ptype (opt defaultValue) .>> separator
    |>> fun (name, t, defaultValue) -> { Name = name; Type = t; DefaultValue = defaultValue }

let variableDefinitions = 
    between openP closeP (many variableDefinition)
    .>> spaces 

let argument = 
    spaces >>.
    name .>> spaces .>> skipChar ':' .>> spaces .>>. value |>> fun (name, value) -> { Name = name; Value = value }
    .>> separator

let arguments = 
    between openP closeP (many argument)
    .>> spaces

let directive = 
    skipChar '@' >>. name .>>. arguments 
    |>> fun (name, args) -> { Name = name; Arguments = args }

let directives = many directive

let fragmentName (stream : CharStream<'a>) =
    if stream.PeekString(3) <> "on " 
    then name stream
    else Reply(Error, unexpectedString "on ") 

let selection, selectionImpl = createParserForwardedToRef()
let selectionSet = 
    between 
        (skipChar '{') 
        (skipChar '}') 
        (many selection)

let field = 
    tuple5 (opt (attempt alias)) name (optList arguments) (attempt directives) (optList selectionSet) 
    |>> fun (alias, name, args, dirs, set) -> { Alias = alias; Name = name; Arguments = args; Directives = dirs; SelectionSet = set }

let fragmentSpread = 
    fragmentName .>> spaces .>>. directives
    |>> fun (name, dirs) -> { FragmentName = name; Directives = dirs }

let inlineFragment = 
    tuple3 (opt typeCondition) directives (optList selectionSet)
    |>> fun (condition, dirs, set) -> { TypeCondition = condition; Directives = dirs; SelectionSet = set }

do selectionImpl :=
    spaces >>.
    choice [ 
        field |>> Field; 
        skipString "..." >>. spaces >>. ((fragmentSpread |>> FragmentSpread) <|> (inlineFragment |>> InlineFragment));
    ]
    .>> separator

let explicitOperation =
        tuple5 operationType (opt name) (optList variableDefinitions) directives (optList selectionSet)
        |>> fun (t, name, vars, dirs, set) -> { Type = t; Name = name; VariableDefinitions = vars; Directives = dirs; SelectionSet = set }  

let implicitOperation =
    selectionSet |>> fun set -> { Type = Query; Name = None; VariableDefinitions = []; Directives = []; SelectionSet = set }

let operationDefinition = explicitOperation <|> implicitOperation

let fragmentDefinition =
    skipString "fragment" >>. spaces >>. tuple4 fragmentName typeCondition directives selectionSet
    |>> fun (name, condition, dirs, set) -> { Name = name; TypeCondition = condition; Directives = dirs; SelectionSet = set }

let definition : Parser<Definition, unit> = 
    spaces >>.
    choice [
        operationDefinition |>> OperationDefinition;
        fragmentDefinition |>> FragmentDefinition
    ]
    .>> separator

let document = many definition

let parse input = 
    run document input