module FigureUnion

type FigureUnion =
    | RectangleDescriminator of width: float * height: float
    | SquareDescriminator of side: float
    | CircleDescriminator of radius: float

 let area figure =
    match figure with
    | RectangleDescriminator (w, h) -> w * h
    | SquareDescriminator s -> s * s
    | CircleDescriminator r -> System.Math.PI * r * r

let unionMain =
    let f1 = RectangleDescriminator (3.0, 4.0)
    let f2 = SquareDescriminator 5.0
    let f3 = CircleDescriminator 2.5

    printfn "\nПлощадь прямоугольника: %f" (area f1)
    printfn "Площадь квадрата: %f" (area f2)
    printfn "Площадь круга: %f" (area f3)
