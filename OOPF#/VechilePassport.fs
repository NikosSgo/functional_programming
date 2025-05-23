module VechilePassport

open System
open System.Text.RegularExpressions

type IPrintable =
    abstract member Print : unit -> unit

type VehiclePassport(series: string, number: string, make: string, model: string, vin: string, year: int) =
    member val Series = series with get, set
    member val Number = number with get, set
    member val Make = make with get, set
    member val Model = model with get, set
    member val VIN = vin with get, set
    member val Year = year with get, set

    // Метод валидации VIN
    member this.ValidateVin() =
        let pattern = "^[A-HJ-NPR-Z0-9]{17}$" // VIN: 17 символов, исключены I, O, Q
        Regex.IsMatch(this.VIN, pattern)

    // Валидация серии: 2 заглавные буквы
    member this.ValidateSeries() =
        Regex.IsMatch(this.Series, @"^[A-Z]{2}$")

    // Валидация номера: 6 цифр
    member this.ValidateNumber() =
        Regex.IsMatch(this.Number, @"^\d{6}$")

    // Метод валидации всех полей
    member this.IsValid() =
        this.ValidateVin() && this.ValidateSeries() && this.ValidateNumber()

    // Переопределение метода ToString
    override this.ToString() =
        $"ПТС: Серия {this.Series}, Номер {this.Number}, Марка: {this.Make}, Модель: {this.Model}, VIN: {this.VIN}, Год: {this.Year}"

    // Реализация интерфейса
    interface IPrintable with
        member this.Print() =
            if this.IsValid() then
                printfn "%s" (this.ToString())
            else
                printfn "Некорректные данные ПТС"

    // Переопределение Equals и GetHashCode для сравнения
    override this.Equals(obj) =
        match obj with
        | :? VehiclePassport as other ->
            this.Series = other.Series && this.Number = other.Number
        | _ -> false

    override this.GetHashCode() =
        hash (this.Series, this.Number)

let vechilePassportMain =
    let pts1 = VehiclePassport("AB", "123456", "Toyota", "Camry", "JH4KA8270MC012345", 2020)
    let pts2 = VehiclePassport("AB", "123456", "Honda", "Civic", "1HGCM82633A004352", 2021)
    let pts3 = VehiclePassport("XY", "654321", "Ford", "Focus", "INVALIDVIN1234567", 2019)

    let printDocument (doc: IPrintable) =
        doc.Print()

    printfn "\nДокумент 1:"
    printDocument (pts1 :> IPrintable)

    printfn "\nДокумент 2:"
    printDocument (pts2 :> IPrintable)

    printfn "\nДокумент 3 (с ошибками):"
    printDocument (pts3 :> IPrintable)

    printfn "\nСравнение документа 1 и 2 (по серии и номеру): %b" (pts1.Equals(pts2))
    printfn "Сравнение документа 1 и 3: %b" (pts1.Equals(pts3))

