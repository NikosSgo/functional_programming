open System

let circleArea r = 
    let pi = 3.14159
    pi * r * r
 
let cylindreVolume r h =
    h * circleArea r

let rec sumOfDigits n =
     if n = 0 then
         0 
     else
         (n % 10) + sumOfDigits (n / 10) 

let digitSum2 n = 
     let rec sumCifr1 n curSum = 
         if n = 0 then curSum
         else
             let n1 = n/10
             let cifr = n%10
             let newSum = curSum + cifr
             sumCifr1 n1 newSum
     sumCifr1 n 0

let rec countDivisors num =
    let rec loop div = 
        match div with
        | 1 -> 1 
        | div when num % div = 0 -> 1 + loop (div - 1) 
        | _ -> loop (div - 1)  
    loop num


let countDivisorsTail num =
    let rec loop n acc =
        match n with
        | 0 -> acc
        | _ when num % n = 0 -> loop (n - 1) (acc + 1)
        | _ -> loop (n - 1) acc
    loop num 0

let SumOrCountDivisors flag num = 
    match flag with 
    | true -> sumOfDigits num
    | false -> countDivisors num


let rec mapDigits num (func: int->int->int) acc =
    match num with
    | 0 -> acc
    | nextNumber -> mapDigits (nextNumber / 10) func (func acc (abs(nextNumber) % 10))

let rec mapDigitsWithCondition num (func: int->int->int) acc (condition: int->bool) =
    match num with 
    | 0 -> acc
    | nextNumber ->
        let digit = nextNumber % 10
        let flag = condition digit
        match flag with
        | true -> mapDigitsWithCondition (nextNumber / 10) func (func acc (nextNumber % 10)) condition
        | false -> mapDigitsWithCondition (nextNumber / 10) func acc condition

let chooseLanguage lang =
    match lang with
    | "F#" -> "Podliza"
    | "Prolog" -> "Ultra podliza"
    | "Ruby" -> "Super nice"
    | someth -> "Nu takoe"



let chooseLanguageSuperPos () = 
    Console.Write("Enter your favorite language: ")
    (Console.ReadLine >> chooseLanguage >> Console.WriteLine)

let chooseLanguageCurry () = 
    Console.Write("Enter your favorite language: ")
    let subChooseLanguageCurry reader func writer =
        let answer = reader()
        let langResult = func answer
        writer langResult
    subChooseLanguageCurry Console.ReadLine chooseLanguage Console.WriteLine

let rec gcd x y = 
   match y with
   | 0 -> x
   | someth -> gcd y (x%y)

let processPrimeNumbers num (func: int->int->int) init  =
    let rec processPrimeNumbers num func acc current =
        match current with
        | 0 -> acc
        | someth ->
            let newAcc =
                let result = gcd num current
                match result with
                | 1 -> func acc current
                | _ -> acc
            processPrimeNumbers num func newAcc (current - 1)
    processPrimeNumbers num func init num

let EulerFinder num = 
    processPrimeNumbers num (fun x y -> x + 1) 0

let processPrimeNumbersWithCondition num (func: int->int->int) init (condition: int -> bool)  =
    let rec processPrimeNumbersLoop num func acc current condition =
        match current with
        | 0 -> acc
        | someth ->
            let newAcc =
                let result = gcd num current
                let flag = condition current
                match result, flag with
                | 1, true -> func acc current
                | _, _ -> acc
            processPrimeNumbersLoop num func newAcc (current - 1) condition
    processPrimeNumbersLoop num func init num condition

let countCoprimes n =
    let rec loop current acc =
        if current = 0 then acc
        else
            let acc' = if gcd n current = 1 then acc + 1 else acc
            loop (current - 1) acc'
    loop (n - 1) 0

let sumDigitsDivBy3 n =
    let rec loop num acc =
        if num = 0 then acc
        else
            let digit = abs num % 10
            let acc' = if digit % 3 = 0 then acc + digit else acc
            loop (num / 10) acc'
    loop n 0