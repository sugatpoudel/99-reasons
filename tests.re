open Printf;

/* helper module to convert a list into a string representation */
module StringOfList = {
  let of_list = (lst: list('a), str: 'a => string) : string => {
    let combine = (x, y) => x == "" ? str(y) : x ++ ", " ++ str(y);
    "[" ++ List.fold_left(combine, "", lst) ++ "]";
  };
  let of_string_list = lst => of_list(lst, x => x);
  let of_int_list = lst => of_list(lst, x => string_of_int(x));
  let of_bool_list = lst => of_list(lst, x => string_of_bool(x));
};

let main = () => {
  let fs = [("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)];
  let result = Logic.huffman(fs);
  let string_of_tup = ((e, w)) => sprintf("(%s, %d)", e, w);
  StringOfList.of_list(result, string_of_tup) |> print_endline;
};

main();
