open Lists;

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

/*
  34. calculate euler's totient function. This function is defined as the number
  coprime integers r (1 <= r < m) to some prime integer m.
 */
let phi = (a: int) : int => {
  let rec aux = (acc, x) =>
    switch x {
    | 0
    | 1 => acc + 1
    | _ => coprime(a, x) ? aux(acc + 1, x - 1) : aux(acc, x - 1)
    };
  aux(0, a - 1);
};

/*
   35. determine all prime factors for a given number; there are a couple of
   optimizations to consider for this function. We won't need to check if a
   divisor is prime because we start from 2. We don't need to reset i when recursing
   on a factor becuse anything smaller would have already been found. We use the current
   value of i to account for any multiplicities.
 */
let factors = (n: int) : list(int) => {
  let rec aux = (n, i) =>
    switch n {
    | 1 => []
    | _ => n mod i == 0 ? [i, ...aux(n / i, i)] : aux(n, i + 1)
    };
  aux(n, 2);
};

/* 36. find the prime factors for a given number along with their multiplicity */
let factors_m = (n: int) : list((int, int)) => encode(factors(n));

/* 37. calculate euler's totient using the factors forumla */
let rec pow = (base: int, exp: int) : int =>
  switch exp {
  | 0 => 1
  | _ => base * pow(base, exp - 1)
  };

let phi_improved = (n: int) : int => {
  let rec aux = acc =>
    fun
    | [] => acc
    | [(m, f), ...tl] => aux(acc * (f - 1) * pow(f, m - 1), tl);
  aux(1, factors_m(n));
};

/* 38. time the two phi functions, phi_improved is faster by a factor of 10^2 */
let timeit = (func: 'a => 'b, arg: 'a) : float => {
  let t_start = Sys.time();
  ignore(func(arg));
  let t_end = Sys.time();
  t_end -. t_start;
};

/* 39. generate a list of all primes within a given range */
let all_primes = (l: int, u: int) : list(int) => {
  let rec aux = (acc, cur) =>
    switch cur {
    | _ when cur > u => acc
    | _ => prime(cur) ? aux([cur, ...acc], cur + 1) : aux(acc, cur + 1)
    };
  aux([], l);
};

/* 40. find two prime integers that sum up to the given integer */
let goldbach = (n: int) : (int, int) => {
  let rec aux = c => prime(c) && prime(n - c) ? (c, n - c) : aux(c + 1);
  aux(2);
};

/* 41. output the list of goldbach compositions for all even numbers within the range */
let goldbach_list = (l: int, h: int) : list((int, (int, int))) => {
  let rec aux = i =>
    switch i {
    | _ when i > h => []
    | _ => [(i, goldbach(i)), ...aux(i + 2)]
    };
  aux(l mod 2 == 0 ? l : l + 1);
};
