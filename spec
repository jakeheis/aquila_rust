// Aquila lang .aq

constdecl let i = 

type Box<T> {
    let value: T
}

def square(i: ref int) {

}

def double(i: cpy int) {

}

def add_one(i: cpy int) {

}

def main(): int {
    let i = 0
    square(i.ref)
    double(i)


    let longLive: ref Int = Box.alloc(0)
    square(i)
    double(i.copy)

    let array = [1, 2, 3, 4];

    let 
    
    return i
}





// Known

// Dynamic knowns can only look at any property
NonEmpty: runtime Known = func (val: Array): Bool {
    return val.count > 0
}

type Array {
    known NonEmpty(object: Array): Bool {
        return object.count > 0 
    }

    init(item1: any, rest: any...) [guarantees NonEmpty] {
        
    }
}

func printNonEmptyArray(val: Array) where val is NonEmpty {
    print(val);
}

let single = ["a"]; // type: Array known to be [NonEmpty]
printNonEmptyArray(single)

let none = []; // array: Array
printNonEmptyArray(none) // Error: none not known to be NonEmpty

if none is NonEmpty {
    printNonEmptyArray(none) // Fine
}

guarantee none is NonEmpty // Nonchecked
printNonEmptyArray(none) // Fine



SameLength: compile Known = func(val: Vec, as: Vec): Bool {
    return val.length == as.length
}

type Vec<length: Int> {

    // compile knowns can only look at types / variadic parameters
    #compilerProvable
    known SameLength(object: Vec, as: Vec): Bool {
        return object.length == as.length
    }

    let storage: Array
    
    def dot(other: Vec): Int
        where other is SameLength(as: self)
    {
        return zip(storage, other.storage).map(*).reduce(+)
    }

}

let vec1 = Vec(0, 1, 2)
let vec2 = Vec(3, 4, 5)
let vec3 = Vec(3, 4, 5, 6)

print(vec1.dot(vec2)) // Compiler checks if they are Known to be same length

print(vec1.dot(vec3)) // Error: vec3 not known to be SameLength

if vec3 is SameLength(as: vec1) { // Warning: condition will never be true
    print(vec1.dot(vec3))
}

guarantee vec3 is SameLength(as: vec1)

print(vec1.dot(vec3)) // Runtime error in debug mode, undefined in release mode


// Type unions



// Extending (somewhere between subclass / C fake subclass)

abstract type Object {
    weak var next: Object
}

type StringObject extends Object {
    var num: Int
}

type FuncObject extends Object {
    var call: Function
}

// compiler synthesizes

ObjectSubtype: Known = func(val: Object, type: Object.Type): Bool {
    return val.type == type;
}

abstract type Object {
    enum Type {
        case StringObject
        case FuncObject
    }

    let type: Type
    weak var next: Object

    def asStringObject: StringObject? {
        if type is ObjectSubtype(Type.StringObject) {
            // type becomes Object | StringObject union
            return type;
        }
    }
}

type StringObject {
    let type: Object.Type
    weak var next: Object
    var num: Int
}

type FuncObject {
    let type: Object.Type
    weak var next: Object
    var call: Function
}

// More knowns

type Request {
    known Executing(object: Request): Bool {
        return object.executing;
    }

    var executing = false

    guarantees [not Executing]
    init() {

    }

    // In debug mode, checks at runtime if a function guarantees a known that
    // the known is actually true after the function completes
    
    def execute()
        where self is not Executing
        guarantees [Executing]
    {
        executing = true
    }

    when self is Executing
    guarantees self is [not Executing]
    def cancel() {
        executing = false
    } 

    when self is Executing {
        def cancel() {
            executing = false
        }
        guarantees self is [not Executing]
    }

}

type Person {
    known Important

    guarantees [Important]
    init(president: Int)  {

    }

    guarantees [not Important]
    init(citizen: String)  {

    }
}

// Arrays / ponters as windows

let storage = Storage(Int, 250) // Storage<length: 250, Int>
// set to [0, 1, 2, 3, ...]

let firstFrame = storage.window() // Window<offset:0, length: 250, Int>
firstFrame[0] // 0
firstFrame[1] // 1
firstFrame[250] // Error: out of bounds

let nextFrame = firstFrame.offset(10) // Window<offset:10, length: 240, Int>
nextFrame[0] // 10
nextFrame[1] // 11
firstFrame[240] // Error: out of bounds

/*
let slider = storage.slider(at: 0) // Slider<length: 250, Int> Known [Offset<0>]
slider.current() // 0
slider.assign(10)
slider.current() // 10
slider.forward() // Known [Offset<1>]
*/

let stack = storage.stackWindow(start: 0) // StackWindow<length: 250, Int>
stack.push(4)
stack.pop()

// Everything in windows?

type DoubleInt {
    var first: Int
    var second: Int
}

let storage = ByteStorage(100) // ByteStorage<length: 80>

let intWindow = storage.window(as: Int) // Window<offset: 0, length: 20, Int>

let doubleIntWindow = storage.window(as: DoubleInt) // Window<offset: 0, length: 10, DoubleInt>

// Pointer = SingleWindow?

let storage = Storage(Int, 250)

let second = storage.window(to: 1) // SingleWindow <offset: 1, Int>

let anInt: Int = 10
let window = anInt.window() // SingleWindow <offset: 0, Int>
let window2 = &anInt // SingleWindow <offset: 0, Int>

type SingleWindow<offset: Int, T> {



}

// Compiles down to

int storage[250]

int* firstFrame = storage
firstFrame[0]
firstFrame[1]

int* nextFrame = firstFrame + 10
firstFrame[0]
firstFrame[1]

// Maybe known indexing?

type Window<offset: Int, length: Int, T> {

    #compilerProvable
    known InBounds(object: Int, in: Window): Bool  {
        return index < object.length
    }

    def [](index: Int): T where index is InBounds(in: self) {

    }

    // Optional if not known in bounds?
    def [](index: Int): T? {
        
    }

}
