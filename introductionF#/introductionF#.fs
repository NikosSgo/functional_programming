open System

Console.WriteLine("Hello world")

type SolveResult =
    None
    | Linear of float
    | Quadratic of float*float

let solve a b c =
    let D = b * b - 4. * a * c
    if a = 0. then
        if b = 0. then None
        else Linear(-c / b)
    else
        if D < 0. then None
        else Quadratic((((-b + sqrt(D)) / (2. * a)), ((-b - sqrt(D)) / (2. * a))))

let circle r = Math.PI * r * r
let multiply s h = s * h
let cylinderVolumeSuperPosition = circle >> multiply
let cylinderVolumeCurry r h = (circle r) * h

let rec sumDigitsUp num =
    if num = 0 then 0
    else (num % 10) + (sumDigitsUp (num / 10))

let sumDigitsDown num =
    let rec sumDigitsDownLoop num current =
        if num = 0 then current
        else sumDigitsDownLoop (num / 10) (current + (num % 10))
    sumDigitsDownLoop num 0


[<EntryPoint>]
let main argv =
    let result = solve 1. 2. 3.
    match result with
        None -> Console.WriteLine("There are no solutions.")
        | Linear(x) -> Console.WriteLine($"Linear equation: {x}.")
        | Quadratic(x1, x2) -> Console.WriteLine($"Quadratic equation: {x1}, {x2}.")
    Console.Write("Enter radius:")
    let r = Console.ReadLine() |> float
    Console.Write("Enter height:")
    let h = Console.ReadLine() |> float
    Console.WriteLine($"Cylinder volume (superposition): {cylinderVolumeSuperPosition r h}")
    Console.WriteLine($"Cylinder volume (curry): {cylinderVolumeCurry r h}")
    Console.Write("Enter number: ")
    let number = Console.ReadLine() |> int
    Console.WriteLine($"Sum of digits (used upward recursion): {sumDigitsUp number}")
    Console.WriteLine($"Sum of digits (used downward recursion): {sumDigitsDown number}")
    0