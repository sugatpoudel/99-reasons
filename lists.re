open Printf;

/* find the last element of a list */
let rec last (lst: list 'a) :option 'a =>
  switch lst {
  | [] => None
  | [hd] => Some hd
  | [_, ...tl] => last tl
  };

/* find the second to last element of a list */
let rec penultimate (lst: list 'a) :option 'a =>
  switch lst {
  | []
  | [_] => None
  | [hd, _] => Some hd
  | [_, ...tl] => penultimate tl
  };

/* find the nth element of a list */
let rec find_nth (n: int, lst: list 'a) :option 'a =>
  switch lst {
  | [] => None
  | [hd, ...tl] => n == 1 ? Some hd : find_nth (n - 1, tl)
  };

/* find the length of a list */
let rec length (lst: list 'a) :int =>
  switch lst {
  | [] => 0
  | [_, ...tl] => 1 + length tl
  };

/* find the length of a list (tail call) */
let length_tc (lst: list 'a) :int => {
  let rec aux (acc: int, lst: list 'a) :int =>
    switch lst {
    | [] => acc
    | [_, ...tl] => aux (acc + 1, tl)
    };
  aux (0, lst)
};

/* reverse a list */
let rec reverse (lst: list 'a) :list 'a =>
  switch lst {
  | [] => []
  | [hd, ...tl] => reverse tl @ [hd]
  };

/* determine if a list is a palindrome */
let palindrome (lst: list 'a) :bool => lst == reverse lst;

type node 'a =
  | One 'a
  | Many (list (node 'a));

/*
  flatten a nested list structure; this can be made tail call recursive
  by passing around an accumulated list instead of concatenating on separate
  recursive calls
 */
let rec flatten (lst: list (node 'a)) :list 'a =>
  switch lst {
  | [] => []
  | [One x, ...tl] => [x] @ flatten tl
  | [Many y, ...tl] => flatten y @ flatten tl
  };

/* eliminate consecutive duplicates within a list */
let rec compress (lst: list 'a) :list 'a =>
  switch lst {
  | [x, ...[y, ..._] as tl] => x == y ? compress tl : [x] @ compress tl
  | smaller => smaller
  };

/* pack consecutive duplicates of list elements into sublists */
let pack (lst: list 'a) :list (list 'a) => {
  let rec aux acc cur =>
    fun
    | [] => []
    | [hd] => [[hd, ...cur], ...acc]
    | [x, ...[y, ..._] as tl] => x == y ? aux acc [x, ...cur] tl : aux [[x, ...cur], ...acc] [] tl;
  reverse (aux [] [] lst)
};

/* determine the run length encoding of a list */
let encode (lst: list 'a) :list (int, 'a) => {
  let rec aux acc count =>
    fun
    | [] => []
    | [hd] => [(count + 1, hd), ...acc]
    | [x, ...[y, ..._] as tl] =>
      x == y ? aux acc (count + 1) tl : aux [(count + 1, x), ...acc] 0 tl;
  reverse (aux [] 0 lst)
};

type rle 'a =
  | One 'a
  | Many (int, 'a);

/* modify the encode function to map the encoding into an rle */
let encode_rle (lst: list 'a) :list (rle 'a) => {
  let map (count, str) =>
    switch count {
    | 1 => One str
    | _ => Many (count, str)
    };
  List.map map (encode lst)
};

/* repeat a element n times */
let rec repeat (acc: list 'a) (e: 'a) (n: int) :list 'a =>
  n == 0 ? acc : repeat [e, ...acc] e (n - 1);

/*
  convert an run length encoded list into the original list.
  this process is easier implmented with a helper function that
  repeats a character n times.
 */
let decode (lst: list (rle 'a)) :list 'a => {
  let rec aux acc lst =>
    switch lst {
    | [] => acc
    | [One e, ...tl] => aux [e, ...acc] tl
    | [Many (count, e), ...tl] => aux (repeat acc e count) tl
    };
  aux [] (reverse lst)
};

/* duplicate the elements of a list */
let rec duplicate (lst: list 'a) :list 'a =>
  switch lst {
  | [] => []
  | [hd, ...tl] => [hd, hd] @ duplicate tl
  };

/* replicate the elements of a list a given number of times */
let replicate (lst: list 'a) (n: int) :list 'a => {
  let rec aux acc lst n =>
    switch lst {
    | [] => acc
    | [hd, ...tl] => aux (repeat acc hd n) tl n
    };
  aux [] (reverse lst) n
};

/* drop every nth element from a list */
let drop (lst: list 'a) (n: int) :list 'a => {
  let rec aux count =>
    fun
    | [] => []
    | [hd, ...tl] => count == 1 ? aux n tl : [hd, ...aux (count - 1) tl];
  aux n lst
};

/* split a list into two parts */
let split (lst: list 'a) (n: int) :(list 'a, list 'a) => {
  let rec aux acc count =>
    fun
    | [] => (acc, [])
    | [hd, ...tl] => count == n ? (acc @ [hd], tl) : aux (acc @ [hd]) (count + 1) tl;
  aux [] 1 lst
};

/* extract a slice from a list from i to k (inclusive) */
let slice (lst: list 'a) (i: int) (k: int) => {
  let rec aux acc count =>
    fun
    | [] => acc
    | [hd, ...tl] =>
      count >= i && count <= k ? aux [hd, ...acc] (count + 1) tl : aux acc (count + 1) tl;
  reverse (aux [] 0 lst)
};

/* rotate a list N places to the left */
let rotate (lst: list 'a) (n: int) => {
  let lst = n < 0 ? reverse lst : lst;
  let n_1 = n < 0 ? n * (-1) : n;
  let rec aux acc count =>
    fun
    | [] => acc
    | [hd, ...tl] as l => count == n_1 ? l @ acc : aux (acc @ [hd]) (count + 1) tl;
  let res = aux [] 0 lst;
  n < 0 ? reverse res : res
};

/* remove the kth element of a list */
let remove_at (k: int) (lst: list 'a) => {
  let rec aux acc i =>
    fun
    | [] => acc
    | [hd, ...tl] => i == k ? acc @ tl : aux (acc @ [hd]) (i + 1) tl;
  aux [] 0 lst
};

/* ------------------------------------------------------------------------------------- */
/* helper function to convert a list into a string representation */
let list_to_string (lst: list 'a) (str: 'a => string) :string => {
  let combine x y => x == "" ? str y : x ^ ", " ^ str y;
  "[" ^ List.fold_left combine "" lst ^ "]"
};

let main () => {
  let test = ["a", "b", "c", "d", "e", "f", "g", "h"];
  let result = remove_at 3 test;
  print_endline (list_to_string result (fun x => x))
};

main ();