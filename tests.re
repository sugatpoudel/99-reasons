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
  print_endline(string_of_int(gcd(13, 27)));
  print_endline(string_of_int(gcd(20536, 7826)));
};

main();
