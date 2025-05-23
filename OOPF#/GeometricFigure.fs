module GeometricFigure

type IPrint =
    abstract member Print : unit -> unit

[<AbstractClass>]
type GeometicFigure() =
    abstract member Area : unit -> float
    override this.ToString() = "Фигура"

type Rectangle(width: float, height: float) =
    inherit GeometicFigure()
    member this.Width = width
    member this.Height = height
    override this.Area() = this.Width * this.Height
    override this.ToString() =
        sprintf "Прямоугольник: ширина = %.2f, высота = %.2f, площадь = %.2f" this.Width this.Height (this.Area())
    interface IPrint with
        member this.Print() = printfn "%s" (this.ToString())

type Square(side: float) =
    inherit Rectangle(side, side)
    override this.ToString() =
        sprintf "Квадрат: сторона = %.2f, площадь = %.2f" side (this.Area())
    interface IPrint with
        member this.Print() = printfn "%s" (this.ToString())

type Circle(radius: float) =
    inherit GeometicFigure()
    member this.Radius = radius
    override this.Area() = System.Math.PI * radius * radius
    override this.ToString() =
        sprintf "Круг: радиус = %.2f, площадь = %.2f" this.Radius (this.Area())
    interface IPrint with
        member this.Print() = printfn "%s" (this.ToString())

let OOPmain = 
    let shapes : IPrint list = [
        Rectangle(3.0, 4.0) :> IPrint
        Square(5.0) :> IPrint
        Circle(2.5) :> IPrint
    ]

    for shape in shapes do
        shape.Print()