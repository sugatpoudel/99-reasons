open Printf;

/* 1. find the last element of a list */
let rec last = (lst: list('a)) : option('a) =>
  switch lst {
  | [] => None
  | [hd] => Some(hd)
  | [_, ...tl] => last(tl)
  };

/* 2. find the second to last element of a list */
let rec penultimate = (lst: list('a)) : option('a) =>
  switch lst {
  | []
  | [_] => None
  | [hd, _] => Some(hd)
  | [_, ...tl] => penultimate(tl)
  };

/* 3. find the nth element of a list */
let rec find_nth = (n: int, lst: list('a)) : option('a) =>
  switch lst {
  | [] => None
  | [hd, ...tl] => n == 1 ? Some(hd) : find_nth(n - 1, tl)
  };

/* 4. find the length of a list */
let rec length = (lst: list('a)) : int =>
  switch lst {
  | [] => 0
  | [_, ...tl] => 1 + length(tl)
  };

/* 4.1. find the length of a list (tail call) */
let length_tc = (lst: list('a)) : int => {
  let rec aux = (acc: int, lst: list('a)) : int =>
    switch lst {
    | [] => acc
    | [_, ...tl] => aux(acc + 1, tl)
    };
  aux(0, lst);
};

/* 5. reverse a list */
let rec reverse = (lst: list('a)) : list('a) =>
  switch lst {
  | [] => []
  | [hd, ...tl] => reverse(tl) @ [hd]
  };

/* 6. determine if a list is a palindrome */
let palindrome = (lst: list('a)) : bool => lst == reverse(lst);

type node('a) =
  | One('a)
  | Many(list(node('a)));

/*
  7. flatten a nested list structure; this can be made tail call recursive
  by passing around an accumulated list instead of concatenating on separate
  recursive calls
 */
let rec flatten = (lst: list(node('a))) : list('a) =>
  switch lst {
  | [] => []
  | [One(x), ...tl] => [x] @ flatten(tl)
  | [Many(y), ...tl] => flatten(y) @ flatten(tl)
  };

/* 8. eliminate consecutive duplicates within a list */
let rec compress = (lst: list('a)) : list('a) =>
  switch lst {
  | [x, ...[y, ..._] as tl] => x == y ? compress(tl) : [x] @ compress(tl)
  | smaller => smaller
  };

/* 9. pack consecutive duplicates of list elements into sublists */
let pack = (lst: list('a)) : list(list('a)) => {
  let rec aux = (acc, cur) =>
    fun
    | [] => []
    | [hd] => [[hd, ...cur], ...acc]
    | [x, ...[y, ..._] as tl] =>
      x == y ? aux(acc, [x, ...cur], tl) : aux([[x, ...cur], ...acc], [], tl);
  reverse(aux([], [], lst));
};

/* 10. determine the run length encoding of a list */
let encode = (lst: list('a)) : list((int, 'a)) => {
  let rec aux = (acc, count) =>
    fun
    | [] => []
    | [hd] => [(count + 1, hd), ...acc]
    | [x, ...[y, ..._] as tl] =>
      x == y ? aux(acc, count + 1, tl) : aux([(count + 1, x), ...acc], 0, tl);
  reverse(aux([], 0, lst));
};

type rle('a) =
  | One('a)
  | Many(int, 'a);

/* 11. modify the encode function to map the encoding into an rle */
let encode_rle = (lst: list('a)) : list(rle('a)) => {
  let map = ((count, str)) =>
    switch count {
    | 1 => One(str)
    | _ => Many(count, str)
    };
  List.map(map, encode(lst));
};

/* repeat a element n times */
let rec repeat = (acc: list('a), e: 'a, n: int) : list('a) =>
  n == 0 ? acc : repeat([e, ...acc], e, n - 1);

/*
  12. convert an run length encoded list into the original list.
  this process is easier implmented with a helper function that
  repeats a character n times.
 */
let decode = (lst: list(rle('a))) : list('a) => {
  let rec aux = (acc, lst) =>
    switch lst {
    | [] => acc
    | [One(e), ...tl] => aux([e, ...acc], tl)
    | [Many(count, e), ...tl] => aux(repeat(acc, e, count), tl)
    };
  aux([], reverse(lst));
};

/* 14. duplicate the elements of a list */
let rec duplicate = (lst: list('a)) : list('a) =>
  switch lst {
  | [] => []
  | [hd, ...tl] => [hd, hd] @ duplicate(tl)
  };

/* 15. replicate the elements of a list a given number of times */
let replicate = (lst: list('a), n: int) : list('a) => {
  let rec aux = acc =>
    fun
    | [] => acc
    | [hd, ...tl] => aux(repeat(acc, hd, n), tl);
  aux([], reverse(lst));
};

/* 16. drop every nth element from a list */
let drop = (lst: list('a), n: int) : list('a) => {
  let rec aux = count =>
    fun
    | [] => []
    | [hd, ...tl] => count == 1 ? aux(n, tl) : [hd, ...aux(count - 1, tl)];
  aux(n, lst);
};

/* 17. split a list into two parts */
let split = (lst: list('a), n: int) : (list('a), list('a)) => {
  let rec aux = (acc, count) =>
    fun
    | [] => (acc, [])
    | [hd, ...tl] => count == n ? (acc @ [hd], tl) : aux(acc @ [hd], count + 1, tl);
  aux([], 1, lst);
};

/* 18. extract a slice from a list from i to k (inclusive) */
let slice = (lst: list('a), i: int, k: int) : list('a) => {
  let rec aux = (acc, count) =>
    fun
    | [] => acc
    | [hd, ...tl] =>
      count >= i && count <= k ? aux([hd, ...acc], count + 1, tl) : aux(acc, count + 1, tl);
  reverse(aux([], 0, lst));
};

/* 19. rotate a list N places to the left */
let rotate = (lst: list('a), n: int) : list('a) => {
  let lst = n < 0 ? reverse(lst) : lst;
  let n_1 = n < 0 ? n * (-1) : n;
  let rec aux = (acc, count) =>
    fun
    | [] => acc
    | [hd, ...tl] as l => count == n_1 ? l @ acc : aux(acc @ [hd], count + 1, tl);
  let res = aux([], 0, lst);
  n < 0 ? reverse(res) : res;
};

/* 20. remove the kth element of a list */
let remove_at = (k: int, lst: list('a)) : list('a) => {
  let rec aux = (acc, i) =>
    fun
    | [] => acc
    | [hd, ...tl] => i == k ? acc @ tl : aux(acc @ [hd], i + 1, tl);
  aux([], 0, lst);
};

/* 21. insert an element at a given position into a list */
let insert_at = (x: 'a, k: int, lst: list('a)) : list('a) => {
  let rec aux = (acc, i) =>
    fun
    | [] => acc
    | [hd, ...tl] as l => i == k ? acc @ [x] @ l : aux(acc @ [hd], i + 1, tl);
  aux([], 0, lst);
};

/* 22. create a list containing all integers in the given range */
let range = (i: int, j: int) : list('a) => {
  let rec aux = (i, j) => i > j ? [] : [i, ...aux(i + 1, j)];
  i > j ? reverse(aux(j, i)) : aux(i, j);
};

/*
   23. extract a given number of randomly selected elements from a list.
    1. get the current length of list
    2. pick a random index within the list
    3. extract element at index, add to result list
    4. perform the above steps with the remaining list n times
 */
let rand_select = (lst: list('a), n: int) : list('a) => {
  /* returns the kth element of the list, along with the remaining elements */
  let rec extract = (acc, k) =>
    fun
    | [] => raise(Not_found)
    | [hd, ...tl] => k == 0 ? (hd, acc @ tl) : extract(acc @ [hd], k - 1, tl);
  /* calls extract with a random k given a list and it's length */
  let extract_rand = (len, lst) => extract([], Random.int(len), lst);
  /* a bit verbose, but a recursive random selector until n is exhausted */
  let rec aux = (acc, n, len, lst) =>
    if (n == 0) {
      acc;
    } else {
      let (r_val, rest) = extract_rand(len, lst);
      aux(acc @ [r_val], n - 1, len - 1, rest);
    };
  let len = length(lst);
  /* min accounts for edge case when n greater than size of list */
  aux([], min(len, n), length(lst), lst);
};

/* 24. draw n different numbers from a list */
let lotto_select = (n: int, m: int) : list(int) => rand_select(range(1, m), n);

/* ------------------------------------------------------------------------------------- */
/* helper function to convert a list into a string representation */
let list_to_string = (lst: list('a), str: 'a => string) : string => {
  let combine = (x, y) => x == "" ? str(y) : x ++ ", " ++ str(y);
  "[" ++ List.fold_left(combine, "", lst) ++ "]";
};

let main = () => {
  /* let test = ["a", "b", "c", "d", "e", "f", "g", "h"]; */
  let result = lotto_select(3, 10);
  print_endline(list_to_string(result, x => string_of_int(x)));
};

main();
