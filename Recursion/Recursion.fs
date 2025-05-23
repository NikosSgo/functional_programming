open System

// Задание 4 - сумма цифр рекурсией вверх
let rec sumDigitsUp num =
    if num = 0 then 0
    else (num % 10) + sumDigitsUp (num / 10)
// Задание 5 - сумма цифр рекурсией вниз
let sumDigitsDown num =
    let rec sumDigitsDownLoop accomul num =
        if num = 0 then accomul
        else sumDigitsDownLoop (accomul + num % 10) (num / 10)
    sumDigitsDownLoop 0 num
// Задание 6 - сумма или факториал в зависимотси от флага
let factDigit num =
    let rec factDigitLoop mult num =
        if num = 0 then mult
        else factDigitLoop (mult * num) (num - 1)
    factDigitLoop 1 num


let SumOrFact flag =
    match flag with
    | true -> sumDigitsDown
    | false -> factDigit

// Задание 7 - reduce для числа 
let rec thingsDigits num (func: int->int->int) acc =
    match num with
    | 0 -> acc
    | someth -> thingsDigits (someth / 10) func (func acc (abs(someth) % 10))

// Задание 8 - тестирование thingsDigits на сумме, минимуме и максимуме
let sumDigits num =
    thingsDigits num (fun x y -> x + y) 0

let minDigit num =
    thingsDigits num (fun x y -> if x < y then x else y) 10

let maxDigit num =
    thingsDigits num (fun x y -> if x > y then x else y) 0

// Задание 9 - reduce для числа с условием
let rec thingsDigitsCondition num (func: int->int->int) acc (condition: int->bool) =
    match num with 
    | 0 -> acc
    | someth ->
        let digit = someth % 10
        let flag = condition digit
        match flag with
        | true -> thingsDigitsCondition (someth / 10) func (func acc (someth % 10)) condition
        | false -> thingsDigitsCondition (someth / 10) func acc condition

// Задание 10 - тестирование thingsDigitsCondition на сумме, минимуме и максимуме четных цифр

let sumEvenDigits num =
    thingsDigitsCondition num (fun x y -> x + y) 0 (fun x -> x % 2 = 0)

let minEvenDigit num =
    thingsDigitsCondition num (fun x y -> if x < y then x else y) 10 (fun x -> x % 2 = 0)

let maxEvenDigit num =
    thingsDigitsCondition num (fun x y -> if x > y then x else y) 0 (fun x -> x % 2 = 0)

// Задание 11 - Ввод с консоли любимого языка
let chooseLanguage lang =
    match lang with
    | "F#" -> "Podliza"
    | "Prolog" -> "Ultra podliza"
    | "Ruby" -> ":))"
    | _ -> "Kryto"

// Задание 12 - main для 11 с суперпозицией и каррированием
let chooseLanguageSuperPos () = 
    Console.Write("Введите любимый язык: ")
    (Console.ReadLine >> chooseLanguage >> Console.WriteLine)

let chooseLanguageCurry () = 
    Console.Write("Введите любимый язык: ")
    let subChooseLanguageCurry reader func writer =
        let answer = reader()
        let langResult = func answer
        writer langResult
    subChooseLanguageCurry Console.ReadLine chooseLanguage Console.WriteLine


// Задание 13 - Обход взаимно простых чисел

let rec gcd x y = 
   match y with
   | 0 -> x
   | someth -> gcd y (x%y)

let obhodProst num (func: int->int->int) init  =
    let rec obhodProstLoop num func acc current =
        match current with
        | 0 -> acc
        | someth ->
            let newAcc =
                let result = gcd num current
                match result with
                | 1 -> func acc current
                | _ -> acc
            obhodProstLoop num func newAcc (current - 1)
    obhodProstLoop num func init num

// Задание 14 - Вычисления числа Эйлера

let EulerFinder num = 
    obhodProst num (fun x y -> x + 1) 0

// Задание 15 - Обход взаимно простых чисел с условием

let obhodProstCondition num (func: int->int->int) init (condition: int -> bool)  =
    let rec obhodProstLoop num func acc current condition =
        match current with
        | 0 -> acc
        | someth ->
            let newAcc =
                let result = gcd num current
                let flag = condition current
                match result, flag with
                | 1, true -> func acc current
                | _, _ -> acc
            obhodProstLoop num func newAcc (current - 1) condition
    obhodProstLoop num func init num condition


// Задание 16 - найти максимальный простой делитель числа

let isPrime n =
    if n <= 1 then false
    elif n = 2 then true
    elif n % 2 = 0 then false
    else
        let sqrtN = int (sqrt (float n)) + 1
        let rec checkDivisor d =
            if d > sqrtN then true
            elif n % d = 0 then false
            else checkDivisor (d + 2)
        checkDivisor 3

let rec maxDivisorWithCond num curDivisor maxDivisor (condition: int -> bool) =
    match curDivisor > num with 
    | true -> maxDivisor
    | false ->
        match num % curDivisor = 0 && condition curDivisor with
        | false -> maxDivisorWithCond num (curDivisor + 1) maxDivisor condition
        | true ->
            maxDivisorWithCond num (curDivisor + 1) (max maxDivisor curDivisor) condition

let maxPrimeDivisor num =
    maxDivisorWithCond (abs num) 2 -1 isPrime

// Задание 17 - Найти произведение цифр числа, не делящихся на 5

let prodDigitsNotDivBy5 num =
    let rec loop num prod =
        match num with
        | 0 -> prod 
        | _ -> 
            match num % 10 % 5 <> 0 with
            | true -> loop (num / 10) (prod * num % 10)
            | false -> loop (num / 10) prod

    loop (abs num) 1

// Задание 18 - Найти НОД максимального нечетного непростого делителя числа и про-зведения цифр данного числа.

let gcdMaxNonPrimeOddDivisorWithProdDigitsNum num =

    let maxNonPrimeOddDivisor = maxDivisorWithCond num 1 -1 (fun x -> x % 2 = 1 && not (isPrime x))
        
    let prodDigits = thingsDigits num (fun acc x -> acc * x) 1

    gcd maxNonPrimeOddDivisor prodDigits
    
let selectFunc = function
    | 1 -> maxPrimeDivisor
    | 2 -> prodDigitsNotDivBy5
    | 3 -> gcdMaxNonPrimeOddDivisorWithProdDigitsNum
    | _ -> failwith "Ошибка: номер функции должен быть от 1 до 3"

let mainCurry (n, m) =
    let func = selectFunc n
    let result = func m
    printfn "Результат: %d" result

let mainSuper = selectFunc >> (fun f -> f >> printfn "Результат: %d")

[<EntryPoint>]
let main argv = 
    maxPrimeDivisor 10 |>
        printfn "%d"

    0

