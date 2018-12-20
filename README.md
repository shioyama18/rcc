# Rust C Compiler (rcc)
[![](http://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

## About
rcc is a toy C compiler written in Rust (In Progress). Compiler is following the instruction from the article - [*Writing a C Compiler*](https://norasandler.com/archive/).  Tests are taken from the author's [repository](https://github.com/nlsandler/write_a_c_compiler).

## Usage
This program will output x64 Assembly in Intel syntax. Given the c program below as the first argument, rcc will emit the assembly language to stdout.

```c
// test.c
int add(int a, int b) {
    return a + b;
}

int main() {
    int sum = add(1 + 2, 4);
    return sum + sum;
}
```
```bash
$ cargo run test.c > test.s
$ gcc -o test test.s && ./test
$ echo $?
14
```

## Progress
Although "int" is the only supported type, you can define variables and functions. Below are list of implemented features:
1. Arithmetic Operator 
   * Addition (+)
   * Subtraction (-) 
   * Multiplication (*)
   * Division (/)
   * Modulus (%)
2. Logical Operator 
   * AND (&&)
   * OR (||) 
   * NOT (!)
3. Bitwise Operator
   * AND (&) 
   * OR (|)
   * XOR (^)
   * Complement (~)
   * Left Shift (<<) 
   * Right Shift (>>)
4. Assignment Operator
   * Assignment (=)
   * Add and assign (+=)
   * Subtract and assign (-=)
   * Multiply and assign (*=)
   * Divide and assign (/=)
   * Modulo and assign (%=)
5. Relational Operator
   * Equal (==)
   * Not Equal (!=)
   * Less Than (<)
   * Less Than Or Equal (<=)
   * Greater Than (>)
   * Greater Than or Equal (>=)
6. Conditional
   * if
   * Ternary Operator (?)
7. Loops 
   * for
   * while
   * do .. while
   * break
   * continue


