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
