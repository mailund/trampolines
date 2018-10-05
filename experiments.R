library(pmatch)

List := Nil | List(head, tail : List)

lst <- List(1, List(2, List(3, List(4, Nil))))
lst

list_length <- function(lst) {
    cases(lst,
          Nil -> 0,
          List(head, tail) -> 1 + list_length(tail))
}

list_length(lst)

# Tail recursion
list_length <- function(lst, acc = 0) {
    cases(lst,
          Nil -> acc,
          List(head, tail) -> list_length(tail, acc + 1))
}
list_length(lst)

# Continuation passing
list_length <- function(lst, cont = identity) {
    add_one <- function(len) cont(len + 1)
    cases(lst,
          Nil -> cont(0),
          List(head, tail) -> list_length(tail, add_one))
}
list_length(lst)



Tree := Leaf(val) | Tree(left : Tree, right : Tree)

tree <- Tree(Tree(Leaf(1), Leaf(2)), Tree(Leaf(3), Tree(Leaf(4), Leaf(5))))
tree

tree_size <- function(tree) {
    cases(tree,
          Leaf(.) -> 1,
          Tree(left, right) -> tree_size(left) + tree_size(right))
}
tree_size(tree)

tree_size <- function(tree, acc = 0) {
    cases(tree,
          Leaf(.) -> acc + 1,
          Tree(left, right) -> tree_size(left, tree_size(right, acc)))
}
tree_size(tree)

tree_size <- function(tree, cont = identity) {
    cases(tree,
          Leaf(.) -> cont(1),
          Tree(left, right) -> {
              go_right <- function(left_res) {
                  add_results <- function(right_res) cont(left_res + right_res)
                  tree_size(right, add_results)
              }
              tree_size(left, go_right)
          })
}
tree_size(tree)

## Thunks and trampolines

lst <- purrr::reduce(1:5000, ~ List(.y, .x), .init = Nil)
list_length(lst)

make_thunk <- function(f, ...) {
    force(f)
    args <- list(...)
    function() f(...)
}

f <- function(x, y) x + y
f_thunk <- make_thunk(f, 2, 3)
f_thunk()

list_length <- function(lst, cont = identity) {
    add_one <- function(len) make_thunk(cont, len + 1)
    cases(lst,
          Nil -> cont(0),
          List(head, tail) -> make_thunk(list_length, tail, add_one))
}
lst <- purrr::reduce(1:5000, ~ List(.y, .x), .init = Nil)
list_length(lst)

jump <- function(f) {
    while (is.function(f))
        f <- f()
    f
}

jump(list_length(lst))


tree_size <- function(tree, cont = identity) {
    cases(tree,
          Leaf(.) -> make_thunk(cont, 1),
          Tree(left, right) -> {
              go_right <- function(left_res) {
                  add_results <- function(right_res)
                      make_thunk(cont, left_res + right_res)
                  make_thunk(tree_size, right, add_results)
              }
              make_thunk(tree_size, left, go_right)
          })
}
jump(tree_size(tree))


tree_size_rec <- function(tree, cont = identity) {
    cases(tree,
          Leaf(.) -> make_thunk(cont, 1),
          Tree(left, right) -> {
              go_right <- function(left_res) {
                  add_results <- function(right_res)
                      make_thunk(cont, left_res + right_res)
                  make_thunk(tree_size_rec, right, add_results)
              }
              make_thunk(tree_size_rec, left, go_right)
          })
}
trampoline <- function(f) {
    force(f)
    function(...) jump(f(...))
}
tree_size <- trampoline(tree_size_rec)
tree_size(tree)