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

/* 32. greatest common divisor of two positive integers */
let gcd = (a: int, b: int) : int => 0;

/* ------------------------------------------------------------------------------------- */
let main = () => {
  let test = 4;
  let result = prime(test);
  printf("%d : %s\n", test, string_of_bool(result));
};

main();
