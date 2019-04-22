open Printf;

type binary_tree('a) =
  | Empty
  | Node('a, binary_tree('a), binary_tree('a));

/*
   55. Construct a list of all completely balanced binary trees for the
       given number of nodes.

    Since we are only worried about balanced binary trees, any difference
    between the length of sibling subtrees will be at most one.

    For any given value of i, discounting the parent node we can recursively
    call cbal_tree for it's left and right sub-trees.

    In the case when (i - 1) is even then we can simply get
    cbal_tree((i - 1) / 2) and combine the result for as the left and right subtrees.

    In the case when (i - 1) is odd then we will need to find cbal_tree
    for (i - 1) / 2 and i - ((i - 2) / 2) - 1 and combine the two results.

    The combine function is simply a double loop over the left and right subtrees
    and combining each entry into a node.
 */
let cbal_tree = (i: int) : list(binary_tree(string)) => {
  let combine = (l, r) => {
    let mapper = (n1, lst) => List.map(n2 => Node("x", n1, n2), lst);
    List.fold_left((a, b) => mapper(b, r) @ a, [], l);
  };
  let rec aux = i =>
    switch i {
    | 0 => [Empty]
    | _ when (i - 1) mod 2 == 0 =>
      let half = aux((i - 1) / 2);
      combine(half, half);
    | _ =>
      let half = (i - 1) / 2;
      let l = aux(half);
      let r = aux(i - 1 - half);
      combine(l, r) @ combine(r, l);
    };
  aux(i);
};

/**
 * 56. Symmetric binary trees
 * 
 * A tree is considered symmetric when one side of the tree is a mirror image
 * of the other. Write a function that checks whether a given binary tree is
 * symmetric.
 */
let is_symmetric = (t : binary_tree('a)) : bool => {
  let rec aux = (l, r) => switch (l, r) {
  | (Empty, Empty) => true
  | (Node(_, l1, r1), Node(_, l2, r2)) => aux(l1, r2) && aux(r1, l2)
  | _ => false
  };
  switch t {
  | Empty => true
  | Node(_, l, r) => aux(l, r)
  };
};

/**
 * 57. Construct a binary search tree from a list of integers
 * 
 * Simple insertion algorithm that does not re-balance tree thus
 * for a sorted list, we would essentially have a linked list.
 */
let construct = (lst : list(int)) : binary_tree(int) => {
  let rec insert = (t, k) => 
    switch t {
    | Empty => Node(k, Empty, Empty)
    | Node(v, l, r) when k == v => t
    | Node(v, l, r) when k < v => Node(v, insert(l, k), r)
    | Node(v, l, r) => Node(v, l, insert(r, k))
    };
  List.fold_left((t, i) => insert(t, i), Empty, lst);
};