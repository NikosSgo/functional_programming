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

let rec countDivisors num =
    let rec loop div = 
        match div with
        | 1 -> 1 
        | div when num % div = 0 -> 1 + loop (div - 1) 
        | _ -> loop (div - 1)  
    loop num

let div = countDivisors 10 
printfn $"Количество делителей: {div}"

let countDivisorsTail num =
    let rec loop n acc =
        match n with
        | 0 -> acc
        | _ when num % n = 0 -> loop (n - 1) (acc + 1)
        | _ -> loop (n - 1) acc
    loop num 0

let divTail = countDivisorsTail 10 
printfn $"Количество делителей: {divTail}"

let SumOrCountDivisors flag num = 
    match flag with 
    | true -> sumOfDigits num
    | false -> countDivisors num

let Sum = SumOrCountDivisors false 10


