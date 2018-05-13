open Printf;

open Lists;

open Arithmetic;

/* helper function to convert a list into a string representation */
let string_of_list = (lst: list('a), str: 'a => string) : string => {
  let combine = (x, y) => x == "" ? str(y) : x ++ ", " ++ str(y);
  "[" ++ List.fold_left(combine, "", lst) ++ "]";
};

let string_of_string_list = lst => string_of_list(lst, x => x);

let string_of_int_list = lst => string_of_list(lst, x => string_of_int(x));

let main = () => {
  print_endline("Running test.");
  let res = factors_m(315);
  let res_str = string_of_list(res, ((c, i)) => sprintf("(%d, %d)", i, c));
  print_endline(res_str);
};

main();
