open Printf;

type bool_expr =
  | Var(string)
  | Not(bool_expr)
  | And(bool_expr, bool_expr)
  | Or(bool_expr, bool_expr);

type entry = {
  key: string,
  value: bool
};

/* 46/47/48. calculate the truth table for a two variable expression */
/* find the value of the given key */
let find_entry = (key, entries) => List.find(e => e.key == key, entries);

/* evaluate a given expression using the entries list */
let eval = (entries: list(entry), expr: bool_expr) : bool => {
  let rec aux = expr =>
    switch expr {
    | Var(v) => find_entry(v, entries).value
    | Not(e) => ! aux(e)
    | And(e1, e2) => aux(e1) && aux(e2)
    | Or(e1, e2) => aux(e1) || aux(e2)
    };
  aux(expr);
};

/* string representation of a bool expr */
let rec string_of_bool_expr = (expr: bool_expr) : string =>
  switch expr {
  | Var(v) => v
  | Not(e) => sprintf("!(%s)", string_of_bool_expr(e))
  | And(e1, e2) =>
    sprintf("(%s ^ %s)", string_of_bool_expr(e1), string_of_bool_expr(e2))
  | Or(e1, e2) =>
    sprintf("(%s v %s)", string_of_bool_expr(e1), string_of_bool_expr(e2))
  };

/* determine all combinations of truth values for the given list of variables */
let rec table = (vars: list(string)) : list(list(entry)) =>
  switch vars {
  | [] => [[]]
  | [hd, ...tl] =>
    let combos = table(tl);
    List.map(l => [{key: hd, value: true}, ...l], combos)
    @ List.map(l => [{key: hd, value: false}, ...l], combos);
  };

/* evaluate the given expression with the given variable */
let table2 = (vars: list(string), expr: bool_expr) : list(list(bool)) => {
  let mapper = entries =>
    List.map(e => e.value, entries) @ [eval(entries, expr)];
  List.map(mapper, table(vars));
};

/*
   49. gray code, a sequence of n-bit strings the represents all enumerations of n-bits.
   Essentially, it is a sequence of all n bit numbers.
 */
let rec gray_code = (n: int) : list(string) =>
  switch n {
  | 0 => [""]
  | _ =>
    let sub_gray_code = gray_code(n - 1);
    List.map(a => "0" ++ a, sub_gray_code)
    @ List.map(a => "1" ++ a, sub_gray_code);
  };

/*
  50. huffman, given a frequency table, determine the huffman code for each character.
    1.  Start with as many leaves as there are symbols.
    2.  Enqueue all leaf nodes into the first queue (by probability in increasing order so that the least
        likely item is in the head of the queue).
    3.  While there is more than one node in the queues:
          1. Dequeue the two nodes with the lowest weight by examining the fronts of both queues.
          2. Create a new internal node, with the two just-removed nodes as children (either node can be
              either child) and the sum of their weights as the new weight.
          3. Enqueue the new node into the rear of the second queue.
    4. The remaining node is the root node; the tree has now been generated.
 */
module PriorityQueue = {
  type pq('a) = list(('a, int));
  let rec insert = (p: pq('a), e: 'a, w: int) : pq('a) =>
    switch p {
    | [] => [(e, w)]
    | [(he, hw), ...tl] =>
      w < hw ? [(e, w), ...p] : [(he, hw), ...insert(tl, e, w)]
    };
};

type binary_tree('a) =
  | Leaf('a)
  | Node(binary_tree('a), binary_tree('a));

let rec huffman_tree = q =>
  switch q {
  | [] => raise(Not_found)
  | [(n, _)] => n
  | [(n1, w1), (n2, w2), ...tl] =>
    huffman_tree(PriorityQueue.insert(tl, Node(n1, n2), w1 + w2))
  };

let huffman_code = t => {
  let rec aux = (acc, c, t) =>
    switch t {
    | Leaf(s) => [(s, c), ...acc]
    | Node(l, r) => aux(acc, c ++ "0", l) @ aux(acc, c ++ "1", r)
    };
  aux([], "", t);
};

let huffman = (fs: list((string, int))) : list((string, string)) => {
  let q =
    List.fold_left((q, (e, w)) => PriorityQueue.insert(q, Leaf(e), w), [], fs);
  huffman_code(huffman_tree(q));
};
