open Printf;

/* helper function to convert a list into a string representation */
let string_of_list = (lst: list('a), str: 'a => string) : string => {
  let combine = (x, y) => x == "" ? str(y) : x ++ ", " ++ str(y);
  "[" ++ List.fold_left(combine, "", lst) ++ "]";
};

let string_of_string_list = lst => string_of_list(lst, x => x);

let string_of_int_list = lst => string_of_list(lst, x => string_of_int(x));

let string_of_bool_list = lst => string_of_list(lst, x => string_of_bool(x));

let main = () => {
  print_endline("Running test.");
  let result = Logic.gray_code(4);
  string_of_string_list(result) |> print_endline;
};

main();
