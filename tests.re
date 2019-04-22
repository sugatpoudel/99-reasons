open Printf;

/**
 * Provides method to convert a list into a string.
 */
module StringOfList = {
  let of_list = (lst: list('a), str: 'a => string) : string => {
    let combine = (x, y) => x == "" ? str(y) : x ++ ", " ++ str(y);
    "[" ++ List.fold_left(combine, "", lst) ++ "]";
  };
  let of_string_list = lst => of_list(lst, x => x);
  let of_int_list = lst => of_list(lst, x => string_of_int(x));
  let of_bool_list = lst => of_list(lst, x => string_of_bool(x));
};

/**
 * Provides method to convert a binary tree into a string.
 */
module StringOfTree = {
  let rec of_tree = (t : Trees.binary_tree('a), to_str : 'a => string) : string => {
    switch t {
    | Trees.Empty => "Empty"
    | Trees.Node(v, l, r) => sprintf("Node('%s', %s, %s)", to_str(v), of_tree(l, to_str), of_tree(r, to_str))
    };
  };
  let of_string_tree = (tree) => of_tree(tree, c => c);
  let of_int_tree = (tree) => of_tree(tree, i => string_of_int(i));
};

let main = () => {
  /* let result = Trees.is_symmetric(Trees.construct([5, 3, 18, 1, 4, 12, 21])); */
  /* string_of_bool(result) |> print_endline; */

  let tree = Trees.construct([3, 2, 5, 7, 1]);
  StringOfTree.of_int_tree(tree) |> print_endline;
};

main();
