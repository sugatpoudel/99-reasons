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
  | And(e1, e2) => sprintf("(%s ^ %s)", string_of_bool_expr(e1), string_of_bool_expr(e2))
  | Or(e1, e2) => sprintf("(%s v %s)", string_of_bool_expr(e1), string_of_bool_expr(e2))
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
  let mapper = entries => List.map(e => e.value, entries) @ [eval(entries, expr)];
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
    List.map(a => "0" ++ a, sub_gray_code) @ List.map(a => "1" ++ a, sub_gray_code);
  };
