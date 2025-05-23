let sumRepUnitPrimeDivisors countDivisors powerRepUnit =
    let rec powmod baseVal exponent modulo =
        match exponent with
        | 0UL -> 1UL
        | _ ->
            let result = powmod baseVal (exponent >>> 1) modulo
            let squared = (result * result) % modulo
            match exponent &&& 1UL with
            | 1UL -> (squared * baseVal) % modulo
            | _ -> squared

    let rec isPrime num primeNumList =
        match primeNumList with 
        | [] -> true
        | head :: _ when uint64 head * uint64 head > num -> true
        | head :: _ when num % uint64 head = 0UL -> false
        | _ :: tail -> isPrime num tail

    let rec loop currentNum currentCountDivisors primeNumList result =
        match currentCountDivisors >= countDivisors with
        | true -> result
        | false ->
            match isPrime (uint64 currentNum) primeNumList with
            | false -> loop (currentNum + 1) currentCountDivisors primeNumList result
            | true ->
                let modulo = uint64 (currentNum * 9)
                match powmod 10UL (uint64 powerRepUnit) modulo with
                | 1UL -> 
                    loop (currentNum + 1) 
                         (currentCountDivisors + 1) 
                         (primeNumList @ [currentNum]) 
                         (result + currentNum)
                | _ -> 
                    loop (currentNum + 1) 
                         currentCountDivisors 
                         (primeNumList @ [currentNum]) 
                         result
    
    loop 3 0 [2] 0

let result = sumRepUnitPrimeDivisors 40 1_000_000_000
printfn "Сумма первых 40 простых делителей: %d" result

