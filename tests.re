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
  let result = Trees.cbal_tree(4);

  let rec string_of_tree = (tree, to_str) => {
    switch tree {
    | Trees.Empty => "Empty"
    | Trees.Node(v, l, r) => sprintf("Node('%s', %s, %s)", to_str(v), string_of_tree(l, to_str), string_of_tree(r, to_str))
    };
  };

  printf("Length: %d\n", List.length(result));

  let string_of_char_tree = (tree) => string_of_tree(tree, c => c);
  StringOfList.of_list(result, string_of_char_tree) |> print_endline;
};

main();
