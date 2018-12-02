#!/bin/bash                                                                                      

try() {
  expected="$1"
  input="$2"

  cargo run "$input" > tmp.s
  gcc -o tmp tmp.s
  ./tmp
  actual="$?"

  if [ "$actual" != "$expected" ]; then
    echo "$input expected, but got $actual"
    exit 1
  fi
}

try 2 test.c

echo OK
