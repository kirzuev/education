print("Hello World!")
// constants and variables
let x = 5
var y = 10.0

var explicitType: Int = 11 // явно указали тип переменной

//x + y //ошибка, так как в Swift нет явного приведения типов

Double(x) + y //привели x к типу Double(можно было сразу указать тип)

let label = "width is "
let width = 42
//let widthLabel = label + width // ошибка
let widthLabel = label + String(width) //конкатенация двух строк

let newLabel = "width is \(width)" //значение внутри скобок авто приводится к String и вставляется в строку

"x + y = \(Double(x) + y)" //сначала вычисляется это выражение, потом вызывается конструктор String и все вычисляется

//------------------------
//массив

//var arr = [1, 2, 3, 4, 5] //тип не указываем,т.к. Swift и так определил авто, что это массив Int
var arr: [Int] = [1, 2, 3, 4, 5] //явно задали массив Intов
//let arr: [Int] = [1, 2, 3, 4, 5] //сделав массив из переменного константным, получим ошибки, так как консты трогать нельзя

//arr = [] //обнулили массив, теперь тут нет Intов

var emptyArray = [Int]() //задали пустой массив

arr.append(7) //добавили новый элемент к массиву

arr[1] //выбрали по номеру элема его значение

//------------------------
//stack, list, queue

arr.insert(0, at: 0) //вставка элема под нужный номер с нужным значением

//------------------------
//dictionary

let dictionary = ["Monday" :  "Понедельник", "Tuesday" : "Вторник", ] //можно оставить запятую в конце (тип если удалилась строка то можно не париться осталась запятая или нет)

print("Monday in Russia is \(dictionary["Monday"])") //Optional записался после is, так как ...

//-----------

var ddictionary = ["Monday" :  "Понедельник", "Tuesday" : "Вторник", "Thursday" : "Четверг"]

var xx : Int = 10

//опциональный тип = контейнер в нем либо есть тип, либо нет

var yy : Int? //y может не иметь значения, а может иметь значение типа Int

var firstName = "John"
var lastName : String?

if lastName == nil{
    print("ok")
}
//lastName! //ошибка так как попытались достать значения которого еще нет

lastName = "Smith"
if lastName != nil{
    //force unwrapping
    //точно знаем что есть значение и пытаемся достать его, но это небезопасно, так как Swift про безопасность
    print("Hello, \(firstName) \(lastName!)")
}

lastName = nil
if let nonOptional = lastName{
    print("Hello, \(firstName) \(lastName!)")
}

ddictionary["Wednesday"] = "Среда"
if let wednesday = dictionary["Wednesday"] {
    print("Wednesday in Russian is \(wednesday)")
}

//control flow
let numbers = [-10, 12, 1, -20]

//for  //добавлять элементы на этапе цикла в numbers нельзя - ошибка, как и изменять  n
var sum = 0
for n in numbers{
    sum += n
}
sum

//while repeat
while sum < 100 {
    sum += 10
}

//controlflow - просмотри в документации все возможности

//функции, каждая функция всегда имеет тип . filter состоит из массива интов и инта
//Бляяяяяяяяяять Псиииииииииих - Джон бомбанул с Бугаева XD
let Numbers = [-10, 1, 5 ,-20, 100, -4]

func filter(arr: [Int]) -> [Int] { // отбираем отрицательные элемы
    var result = [Int]()
    
    for n in arr {
        if n < 0 {
            result.append(n)
        }
    }

    return result;
}

filter(arr: Numbers)

//возвращает положительнве элемы , тип [Int] -> Bool
func isPositive(n : Int) -> Bool {
    return n > 0
}

//в Swifte функции можно передавать в качестве аргументов
func filter(arr: [Int], condition: (Int) -> Bool) -> [Int] {
    var result = [Int]()
    
    for n in arr {
        if condition(n){
            result.append(n)
        }
    }
    
    return result
}

filter(arr: Numbers, condition: isPositive)

//Closure, Lambda, Block, замыкания
filter(arr: Numbers, condition: { (n:Int) -> Bool in return n > 0}) // написали внутри описание функции вместого чтобы пол дня писать                                                                  отдельно ее
//уберем return
filter(arr: Numbers, condition: { (n:Int) -> Bool in n > 0})
//уберем Bool
let noTypes = filter(arr: Numbers, condition: { n in n > 0 } )
noTypes
//уберем n
let noName = filter(arr: Numbers, condition: { $0 > 0}) //$0 - первый аргумент нашего блока
noName
//блок передадим внутри -> получили кратчайшую запись
let nothing = filter(arr: Numbers) { $0 > 0 }
nothing

//объявление, создание классов
class ShoppingList {
    //var items = [String]() //публичное Public - любой может обратиться и вытащить значения
    // потому что даже добавив проверку на то что можно добавлять кирпич или нет все равно любой может добавить кирпич -> Private
    private(set) var items = [String]()
    let shop : String
    init(shop : String) {
        self.shop = shop
    }
    
    func addItem(name: String) -> Bool { //возвращает Bool
        // api - можно ли купить товар с name
        items.append(name)
        return true
    }
}

let list = ShoppingList(shop: "Metro")
list.addItem(name: "Вода")
list.addItem(name: "Кока-Кола")

list.items

//list.items.append("Кирпич") // дописав Private сразу здесь схватываем ошибку, до этого не было

//наследование

//протоколы Protocol - Interface
protocol SpeakingBeing {
    func say() -> String //существо может сказать какую-то фразу
    
}
class Person : SpeakingBeing {
    var FirstName: String
    var LastName:  String
    
    init(FirstName: String, LastName: String) {
        self.FirstName = FirstName
        self.LastName = LastName
    }
    func say() -> String{
        return "Hi, I am \(FirstName) \(LastName)"
    }
}


class Student : Person {
    var course : Int
    
    init(FirstName: String,
         LastName: String,
         course : Int = 1) {
        self.course = course
        
        super.init(FirstName: FirstName,
                   LastName: LastName)
        
    }
    
}

let s = Student(FirstName: "John", LastName: "Smith")
s.course

extension Person : CustomStringConvertible {
    var description: String {
        return "Hi, I am \(FirstName) \(LastName)"
    }
}



let p : SpeakingBeing = Person(FirstName: "Alex", LastName: "Poroshin")
p.say()
"\(p)"

class Dog : SpeakingBeing {
    var name = "Thuzik"
    
    func say() -> String {
        return "gav!"
    }
}


