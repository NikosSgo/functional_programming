let rec readList n =
    match n with
    | 0 -> []
    | _ -> 
        System.Console.ReadLine()
        |> System.Convert.ToInt32
        |>  fun head -> head :: readList (n-1) 

let readListConsole () = 
    System.Console.ReadLine() 
    |> System.Convert.ToInt32
    |> readList

let rec writeList list =
    match list with
    | [] -> 0
    | (head : int) :: tail ->
        System.Console.WriteLine(head)
        writeList tail

let rec reduceFilter list (f : int -> int -> int) (filter : int -> bool) acc =
    match list with
    | [] -> acc
    | head :: tail -> 
        let newAcc = f head acc 
        if filter head then 
            reduceFilter tail f filter newAcc 
        else 
            reduceFilter tail f filter acc

let listMin list =
    match list with
    | [] -> 0 
    | h :: t -> reduceFilter list (fun x acc -> if x > acc then acc else x) (fun x -> true) h  

let listSumEven list =
    reduceFilter list (fun x acc -> x + acc) (fun x -> x % 2 = 0) 0

let listCountOdd list =
    reduceFilter list (fun x acc -> 1 + acc) (fun x -> x % 2 = 1) 0

let mostFrequentEl list =
    let rec loop list el count =
        match list with
        | [] -> el
        | head :: tail ->
            let curCount = reduceFilter list (fun x acc -> acc + 1 ) (fun x -> x = head) 0
            if count > curCount then 
                loop tail el count
            else 
                loop tail head curCount
    loop list 0 0

let mostFrequentElList list =
    match list with
    | [] -> 0
    | _ -> 
        list
        |> List.countBy id
        |> List.maxBy snd
        |> fst

let numSqEl list =
    let squares = list |> List.map (fun x -> x * 2)
    list 
    |> List.filter (fun x -> List.contains x squares)
    |> List.length

let map3List (listA: int list) (listB: int list) (listC: int list) = 

    let sumDigits number =
        let rec loop number acc =
            match number with
            | 0 -> acc
            | _ -> loop (number / 10) (acc + number % 10)
        loop (abs number) 0

    let countDivisors n =
        if n = 0 then 0
        else
            let nAbs = abs n
            [1..nAbs] 
            |> List.filter (fun x -> nAbs % x = 0)
            |> List.length

    let sortedA = 
        listA 
        |> List.sortDescending

    let sortedB = 
        listB 
        |> List.sortBy (fun x -> (sumDigits x, -abs x))


    let sortedC = 
        listC 
        |> List.map countDivisors
        |> List.sortByDescending (fun x -> (countDivisors x, abs x))

    List.zip3 sortedA sortedB sortedC

let sortByLength strings =
    strings
    |> List.sortBy (fun (s: string) -> s.Length)

let findExtremeIndex (findMin: bool) (arr : int[]) =
    let rec loop extremeInd curInd =
        match curInd with
        | ci when ci >= arr.Length -> extremeInd
        | ci when findMin && arr.[ci] < arr.[extremeInd] -> loop ci (ci + 1)
        | ci when not findMin && arr.[ci] > arr.[extremeInd] -> loop ci (ci + 1)
        | _ -> loop extremeInd (curInd + 1)
    loop 0 1

let indMinEl (arr : int[]) = 
    findExtremeIndex true arr

let indMaxEl (arr : int[]) = 
    findExtremeIndex false arr

let swapBetweenMinMax (arr : int[]) =
    if arr.Length = 0 then
        arr
    else
        let indMax = indMaxEl arr
        let indMin = indMinEl arr
        let startIdx = min indMin indMax
        let endIdx = max indMin indMax
        
        let before = arr.[0..startIdx]
        let middle = arr.[startIdx+1..endIdx-1] |> Array.rev
        let after = arr.[endIdx..]
        
        Array.concat [before; middle; after]

let countLocalMaxima (arr: int[]) =
    if arr.Length < 3 then
        0 
    else
        [|1 .. arr.Length - 2|]
        |> Array.filter (fun i -> arr.[i] > arr.[i-1] && arr.[i] > arr.[i+1])
        |> Array.length

let countMinInRange (a: int) (b: int) (arr: int[]) =
    if arr.Length = 0 || a > b || b >= arr.Length then
        0 
    else
        let subArray = arr.[a..b]
        
        let minValue = Array.min subArray
        
        subArray 
        |> Array.filter (fun x -> x = minValue) 
        |> Array.length

let elementsBelowAverageWithIndices (arr: int[]) =
    if arr.Length = 0 then
        [||]
    else
        let avg = Array.averageBy float arr |> int
        arr
        |> Array.mapi (fun i x -> (i, x))
        |> Array.filter (fun (_, x) -> x < avg)

let findElementsBelowAverage (arr: int[]) =
    let average = Array.averageBy float arr

    arr 
    |> Array.filter (fun x -> float x < average)

let processList (inputList: int list) =
    // 1. Четные элементы, деленные на 2
    let list1 = 
        inputList 
        |> List.filter (fun x -> x % 2 = 0) 
        |> List.map (fun x -> x / 2)

    // 2. Элементы из list1, делящиеся на 3
    let list2 = 
        list1 
        |> List.filter (fun x -> x % 3 = 0)

    // 3. Квадраты элементов list2
    let list3 = 
        list2 
        |> List.map (fun x -> x * x)

    // 4. Элементы list3, которые есть в list1
    let list4 = 
        list3 
        |> List.filter (fun x -> List.contains x list1)

    // 5. Все элементы list2, list3 и list4
    let list5 = 
        List.concat [list2; list3; list4]

    (list1, list2, list3, list4, list5)


type 'string btree = 
    Node of 'string * 'string btree * 'string btree
    | Nil

let prefix root left right = (root(); left(); right())
let infix root left right = (left(); root(); right())
let postfix root left right = (left(); right(); root())


[<EntryPoint>]
let main argv = 
    let list = readListConsole()

    list
    |> listMin
    |> printfn "Минимальный элемент списка: %d"
    
    list
    |> listSumEven
    |> printfn "Сумма чётных элементов списка: %d"

    list
    |> listCountOdd
    |> printfn "Количество нечётных элементов списка: %d"

    list
    |> mostFrequentEl
    |> printfn "Самый часто встречаемый элемент: %d"

    list
    |> mostFrequentElList
    |> printfn "Самый часто встречаемый элемент с помощью класса List: %d"

    list
    |> numSqEl
    |> printfn "Количество квадратов каких-то элементов списка с помощью класса List: %d"

    let listA = [10; 5; 8; 3]
    let listB = [123; 45; 67; 89]
    let listC = [12; 7; 24; 30]

    let result = map3List listA listB listC

    let strings = ["Боже помоги пожалуйста"; "За шо.."; "Может хватит?"]
    strings
    |> sortByLength
    |> printfn "Осортированный список строк %A"
    
    let Array = [|5; 2; 8; 2; 5; 3; 1; 4|]
    Array
    |> printfn "Массив: %A"

    Array
    |> indMinEl
    |> printfn "Индекс минимального элемента массива: %d"
    
    Array
    |> swapBetweenMinMax
    |> printfn "Перестановка между минимальным и максимальным элементами массива: %A"
    
    let a = 0
    let b = 3

    Array
    |> countMinInRange a b
    |> printfn "Количество минимумов в отрезке от %d до %d: %d" a b

    Array
    |> countLocalMaxima
    |> printfn "Количество локальных максимумов: %d"

    Array
    |> findElementsBelowAverage
    |> printfn "Элементы массива меньше среднего арифметического элементов массива: %A"
    


    
    0
