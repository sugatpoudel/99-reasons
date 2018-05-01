open Printf;

open Yojson.Basic.Util;

type test('a, 'b) = ('a, 'b);

type method('a, 'b) = 'a => 'b;

/* opens the specified json file and converts tests into a the test tuple */
let get_tests = (fname: string) : list(test('a, 'b)) => {
  let json = Yojson.Basic.from_file(fname);
  let to_test = json => {
    let input = json |> member("input") |> to_list |> List.map(to_int);
    let output = json |> member("output") |> to_list |> List.map(to_int);
    (input, output);
  };
  convert_each(to_test, json);
};

/* Nicely formatted string representation of a list */
let list_to_string = (lst: list(int)) : string => {
  let join = (acc, x) => {
    let str_x = string_of_int(x);
    switch acc {
    | "" => str_x
    | _ => acc ++ ", " ++ str_x
    };
  };
  let joined = List.fold_left(join, "", lst);
  "[ " ++ joined ++ " ]";
};

/* Runs a test case against the given function */
let print_results = (fname, f) => {
  let tests = get_tests(fname);
  let tester = case => {
    let (i, o) = case;
    let valid = f(i) == o;
    printf("%5s %20s => %15s\n", string_of_bool(valid), list_to_string(i), list_to_string(o));
  };
  List.iter(tester, tests);
};
