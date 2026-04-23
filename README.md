# OCaml Lambda Calculus Interpreter

An interpreter for an extended, statically typed Lambda Calculus, written entirely in OCaml. The system implements a complete pipeline including lexical analysis, parsing, type-checking, and evaluation.

## Features & Supported Types

The interpreter extends standard lambda calculus with basic data types, complex structures, and a global execution context.

* **Primitives:** `Nat` (Natural numbers with `succ`, `pred`, `iszero`), `Bool` (`true`, `false`, `if-then-else`), and `String` (with `concat`).
* **Complex Structures:** * `Lists`: Homogeneous sequences (e.g., `[1, 2, 3]`) with `hd`, `tl`, `isempty`, and `::` (cons) operators.
  * `Tuples`: Heterogeneous sequences with index-based projection (`#`).
  * `Records`: Named field structures (`{name="John"; age=32}`).
  * `Variants`: Tagged unions (`<pos=3> as Int`) with `case-of` pattern matching.
* **Typing System:** Static type checking with support for Type Aliases (`type N3 = Nat->Nat->Nat`) and structural **Subtyping** (for records and functions).
* **Recursion:** Supported via `letrec`, implemented internally using a fixed-point operator (Y-combinator).
* **Global Context:** Persistent variable and function declarations using `let ID = TERM` without requiring an immediate `in` clause.

## Technology Stack

* **Language:** OCaml (Tested on 4.14.1)
* **Lexer:** `ocamllex`
* **Parser:** `ocamlyacc`

## Setup & Compilation

**Prerequisites:** 
* OCaml compiler toolchain.
* `ledit` (optional, for line editing capabilities in the REPL).

**Linux / Windows (WSL) / macOS:**
```bash
# Compile the project
make

# Launch the interactive REPL
make run
```

## Usage Examples

The interpreter supports multiline input. Statements must be terminated with `;;`.

**1. Basic Functions & Tuples**
```ocaml
>> let x = (1, true, "hello", 24, Lx:Nat.x);;
x : (Nat, Bool, String, Nat, Nat -> Nat)

>> x #1;;
1 : Nat
```

**2. Recursion (Fibonacci)**
```ocaml
>> let fib = letrec sum:Nat->Nat->Nat =
    Ln:Nat.Lm:Nat. if iszero n then m else succ (sum (pred n) m)
  in letrec fib:Nat->Nat =
    Ln:Nat. if iszero n then 0 else if iszero (pred n) then 1 else sum (fib (pred n)) (fib (pred (pred n)))
  ;;
fib : Nat -> Nat
```

**3. Records & Subtyping**
```ocaml
>> type Person = {name:String; age:Nat};;
Person = {name:String; age:Nat}

>> let p = Person {name = "John"; age = 32};;
p : Person = {name:String; age:Nat}

>> p.name;;
"John" : String
```

**4. Variants & Pattern Matching**
```ocaml
>> type Int = <pos:Nat; zero:Bool; neg:Nat>;;
Int = <pos:Nat; zero:Bool; neg:Nat>

>> let p3 = <pos=3> as Int;;
p3 : Int = <pos:Nat; zero:Bool; neg:Nat>

>> let abs = Lx:Int.
        case x of
          <pos=p> -> <pos=p> as Int
        | <zero=z> -> <zero=z> as Int
        | <neg=n> -> <pos=n> as Int
;;
abs : Int = <pos:Nat; zero:Bool; neg:Nat> -> Int = <pos:Nat; zero:Bool; neg:Nat>
```

## ⚖️ License

Copyright (C) 2026 Pablo Soage Rodas

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
