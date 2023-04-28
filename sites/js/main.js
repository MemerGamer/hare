// This file is a demonstration that the webserver can serve js files.

// Simple Hello World!
console.log("Hello Hare!");


// Basic arithmetic
console.log("Basic Arithmetic Operations");
let a = 5, b = 10;

console.log("a: " + a);
console.log("b: " + b);

let sum = a+b, diff = a-b, prod = a*b, div = a/b;

console.log("a+b = " + sum);
console.log("a-b = " + diff);
console.log("a*b = " + prod);
console.log("a/b = " + div);

function isEven(n) {
  return n % 2 == 0;
}

console.log("Basic Functions");

let c = 16;

console.log("c: " + c);

if(isEven(a)) console.log("'a' is Even!");
else console.log("'a' is Odd!");
if(isEven(c)) console.log("'c' is Even!");
else console.log("'c' is Odd!");
