open System
open FParsec
open Parser

let testPass parser input expected =
    match run parser input with
    | Success (result,_,_) when result = expected ->
        System.Console.ForegroundColor <- System.ConsoleColor.Green
        printfn "passed"
    | Success (result,_,_) ->
        System.Console.ForegroundColor <- System.ConsoleColor.Red
        printfn "failed %A" result
    | Failure(msg,_,_) -> 
        System.Console.ForegroundColor <- System.ConsoleColor.Red
        printfn "failed %s" msg

[<EntryPoint>]
let main argv =
    testPass openP "(" ()  
    testPass closeP ")" () 
    testPass openSq "[" ()
    testPass closeSq "]" ()
    
    testPass alias "_Ac0109:" "_Ac0109"
    testPass alias "_Ac0109 :" "_Ac0109"
    testPass alias "_Ac0109\n:" "_Ac0109"
    
    testPass argument "_azP0L:4" ({ Name="_azP0L"; Value=Int(4L) })
    testPass argument "_azP0L\n:400" ({ Name="_azP0L"; Value=Int(400L) })
    testPass argument "_azP0L\n:\n400" ({ Name="_azP0L"; Value=Int(400L) })
    testPass argument "_azP0L\n:\n-400" ({ Name="_azP0L"; Value=Int(-400L) })
    testPass argument "_azP0L:\"z\"" ({ Name="_azP0L"; Value=String("z") })
    testPass argument "_azP0L:\"Hello\nWorld\"" ({ Name="_azP0L"; Value=String("Hello\nWorld") })
    testPass argument "Name:ENUM_VALUE" ({ Name="Name"; Value=Enum("ENUM_VALUE") })
    testPass argument "Name:enum_value" ({ Name="Name"; Value=Enum("enum_value") })
    testPass argument "Name:1.23" ({ Name="Name"; Value=Float(1.23) })
    testPass argument "Name:1.E02" ({ Name="Name"; Value=Int(100L) })
    testPass argument "Name:true" ({ Name="Name"; Value=Boolean(true) })
    testPass argument "Name:false" ({ Name="Name"; Value=Boolean(false) })
    testPass argument "Name:null" ({ Name="Name"; Value=Null })
    testPass argument "Name:$myvariable" ({ Name="Name"; Value=Variable("myvariable") })
    testPass argument "Name:$_mYv4riAbl3" ({ Name="Name"; Value=Variable("_mYv4riAbl3") })

    testPass operationType "query" OperationType.Query
    testPass operationType "mutation" OperationType.Mutation

    NamedType("MyType") |> testPass ptype "MyType" 
    NonNullType(NamedType("MyType")) |> testPass ptype "MyType!" 
    ListType(NamedType("MyType")) |> testPass ptype "[MyType]" 
    ListType(NonNullType(NamedType("MyType"))) |> testPass ptype "[MyType!]" 
    NonNullType(ListType(NamedType("MyType"))) |> testPass ptype "[MyType]!" 
    
    { Name = "MyVar"; Type = NamedType("TestType"); DefaultValue = None } |> testPass variableDefinition "$MyVar: TestType"
    { Name = "MyVar"; Type = NamedType("TestType"); DefaultValue = Some(Int(9L)) } |> testPass variableDefinition "$MyVar: TestType = 9"
    { Name = "MyVar"; Type = NamedType("TestType"); DefaultValue = Some(Int(9L)) } |> testPass variableDefinition "$MyVar: TestType=9"
    { Name = "MyVar"; Type = NamedType("TestType"); DefaultValue = Some(Int(9L)) } |> testPass variableDefinition "$MyVar: TestType\n  =   9"

    [
        { Name = "MyVar1"; Type = NamedType("Boolean"); DefaultValue = Some(Boolean(false)) };
        { Name = "MyVar2"; Type = NamedType("Int"); DefaultValue = Some(Int(9L)) };
        { Name = "_MyVar3"; Type = NonNullType(NamedType("TestType")); DefaultValue = None }
    ]
    |> testPass variableDefinitions "($MyVar1 : Boolean = false $MyVar2 : Int = 9 $_MyVar3 : TestType!)"
    
    [
        { Name = "MyVar1"; Type = NamedType("Boolean"); DefaultValue = Some(Boolean(false)) };
        { Name = "MyVar2"; Type = NamedType("Int"); DefaultValue = Some(Int(9L)) };
        { Name = "_MyVar3"; Type = NonNullType(NamedType("TestType")); DefaultValue = None }
    ]
    |> testPass variableDefinitions "($MyVar1 : Boolean = false, $MyVar2 : Int = 9, $_MyVar3 : TestType!)"

    [
        { Name = "MyVar1"; Type = NamedType("Boolean"); DefaultValue = Some(Boolean(false)) };
        { Name = "MyVar2"; Type = NamedType("Int"); DefaultValue = Some(Int(9L)) };
        { Name = "_MyVar3"; Type = NonNullType(NamedType("TestType")); DefaultValue = None }
    ]
    |> testPass variableDefinitions "($MyVar1 : Boolean = false, $MyVar2 : Int = 9, $_MyVar3 : TestType!,)"
    
    0