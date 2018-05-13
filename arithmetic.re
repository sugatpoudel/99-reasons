open Printf;

/* 31. determine if the given number if prime */
let prime = (x: int) : bool => {
  let rec aux = num =>
    switch (abs(num)) {
    | 0
    | 1 => true
    | n => x mod n != 0 && aux(n - 1)
    };
  aux(x - 1);
};

/* 32. greatest common divisor of two positive integers using euclid's algorith */
let gcd = (a: int, b: int) : int => {
  let (a, b) = a > b ? (a, b) : (b, a);
  let rec aux = (a, b) => {
    let rem = a mod b;
    rem == 0 ? b : aux(b, rem);
  };
  aux(a, b);
};

/* 33. determine if two numbers are coprime */
let coprime = (a: int, b: int) : bool => gcd(a, b) == 1;
