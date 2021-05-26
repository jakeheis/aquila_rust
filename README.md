# Aquila

Toy language to learn Rust and how to build a compiler.

Based on the ideas found in the fantastic https://craftinginterpreters.com/contents.html

```
pub type Vec[T] {

    let storage: ptr T;
    pub let count: int;
    pub let capacity: int;

    pub meta def new(): Vec[T] {
        return Vec[T](cast[ptr T](&0), 0, 0);
    }

    pub def push(object: T) {
        if count == capacity {
            if capacity == 0 {
                capacity = 10;
                storage = Memory.allocate_block[T](capacity);
            } else {
                capacity = capacity * 2;
                storage = Memory.reallocate[T](storage, capacity);
            }
        }

        copy_in(count, &object);

        count = count + 1;
    }

    pub def get(index: int, #caller): ptr T {
        if index >= count {
            fatal_error_location("index out of bounds", caller);
        }
        return Memory.offset[T](storage, index);
    }

    pub def first(): Optional[ptr T] {
        if count > 0 {
            return Optional[ptr T].some(storage);
        } else {
            return Optional[ptr T].none();
        }
    }

    pub def replace(index: int, object: T, #caller) {
        if index > count {
            fatal_error_location("index out of bounds", caller);
        }
        if index == count {
            push(object);
        } else {
            copy_in(index, &object);
        }
    }

    pub def pop(#caller): T {
        if count == 0 {
            fatal_error_location("can't pop from empty vector", caller);
        }
        count = count - 1;
        return *Memory.offset[T](storage, count);
    }

    def copy_in(index: int, object: ptr T) {
        let location = Memory.offset[T](storage, index);
        Memory.copy[T](object, location);
    }

}

impl Vec: Writable {
    def write(buffer: ref StringBuffer) {
        buffer.push("Vec");
        #if T: Writable {
            buffer.push("(");
            let first = true;
            for obj in self {
                if first == false {
                    buffer.push(", ");
                }
                first = false;
                obj.write(buffer);
            }
            buffer.push(")");
        }
    }
}

impl Vec: Iterable {}

impl Vec: Freeable {
    def free() {
        #if T: AnyObject {
            for obj in self {
                obj.deinit();
            }
        }
        if capacity > 0 {
            Memory.drop[T](storage);
        }
    }
}
```
