module Functors

type Box<'T> = Box of 'T

module Box =
    
    // Функтор
    let map f (Box x) = Box (f x)

    // Аппликативный функтор
    let pure x = Box x

    // Аппликативный функтор
    let apply (Box f) (Box x) = Box (f x)

    // Монада
    let bind f (Box x) = f x

// Закон функтора: map id = id
let functorIdentityTest = Box.map id (Box 42) = Box 42

// Закон функтора: map (f >> g) = map f >> map g
let f = (+) 1
let g = (*) 2

let composed = Box.map (f >> g) (Box 3)
let sequential = Box.map f >> Box.map g
let functorCompositionTest = composed = sequential (Box 3)

// Закон аппликативного функтора: pure id <*> v = v
let applicativeIdentityTest = Box.apply (Box.pure id) (Box 5) = Box 5

// Гомоморфизм: pure f <*> pure x = pure (f x)
let applicativeHomomorphismTest = Box.apply (Box.pure ((+) 2)) (Box.pure 3) = Box.pure ((+) 2 |> (fun f -> f 3))

// Интерчейндж: u <*> pure y = pure ($ y) <*> u
let u = Box ((+) 2)
let left = Box.apply u (Box 3)
let right = Box.apply (Box (fun f -> f 3)) u
let applicativeInterchangeTest = left = right

// Закон монады: return x >>= f = f x
let m = Box 5
let monadLeftIdentityTest = Box.bind (fun x -> Box (x + 1)) (Box 5) = Box 6

// Закон монады: m >>= return = m
let monadRightIdentityTest = Box.bind Box (Box 5) = Box 5

// Закон монады: ассоциативность
let fM x = Box (x + 1)
let gM x = Box (x * 2)

let leftMonad = Box.bind gM (Box.bind fM (Box 3))
let rightMonad = Box.bind (fun x -> Box.bind gM (fM x)) (Box 3)
let monadAssociativityTest = leftMonad = rightMonad

let functorMain =
    printfn "\nФунктор: закон тождественности: %b" functorIdentityTest
    printfn "Функтор: закон композиции: %b" functorCompositionTest
    printfn "Аппликативный функтор: закон тождественности: %b" applicativeIdentityTest
    printfn "Аппликативный функтор: гомоморфизм: %b" applicativeHomomorphismTest
    printfn "Аппликативный функтор: закон перестановки (interchange): %b" applicativeInterchangeTest
    printfn "Монада: левый нейтральный элемент: %b" monadLeftIdentityTest
    printfn "Монада: правый нейтральный элемент: %b" monadRightIdentityTest
    printfn "Монада: ассоциативность: %b" monadAssociativityTest

