module FParsecArithmOp

open FParsec

type Expr =
    | Number of float
    | Add of Expr * Expr
    | Subtract of Expr * Expr
    | Multiply of Expr * Expr
    | Divide of Expr * Expr


let ws = spaces

let parseNumber =
    pfloat |>> Number .>> ws

let opp = new OperatorPrecedenceParser<Expr, unit, unit>()
let expr = opp.ExpressionParser

let term = parseNumber <|> between (pchar '(' >>. ws) (pchar ')' >>. ws) expr

opp.TermParser <- term

opp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, fun x y -> Add(x, y)))
opp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, fun x y -> Subtract(x, y)))
opp.AddOperator(InfixOperator("*", ws, 2, Associativity.Left, fun x y -> Multiply(x, y)))
opp.AddOperator(InfixOperator("/", ws, 2, Associativity.Left, fun x y -> Divide(x, y)))

let rec eval expr =
    match expr with
    | Number n -> n
    | Add (a, b) -> eval a + eval b
    | Subtract (a, b) -> eval a - eval b
    | Multiply (a, b) -> eval a * eval b
    | Divide (a, b) -> eval a / eval b

let fparsecMain =
    let input = "(2 + 3) * 4 - 5 / 2"
    match run expr input with
    | Success(result, _, _) ->
        printfn "Разобранное выражение: %A" result
        printfn "Результат вычисления: %f" (eval result)
    | Failure(errorMsg, _, _) ->
        printfn "Ошибка парсинга: %s" errorMsg
