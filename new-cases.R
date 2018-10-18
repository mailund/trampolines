manual_contains <- function(llist, key) {
    while (!is.null(llist)) {
        if (llist$car == key) return(TRUE)
        llist <- llist$cdr
    }
    FALSE
}

manual_contains_rec <- function(llist, key) {
    if (is.null(llist))
        FALSE
    else if (llist$car == key)
        TRUE
    else
        manual_contains_rec(llist$cdr, key)
}

tr_manual_contains <- tailr::loop_transform(manual_contains_rec, byte_compile = FALSE)

llist1 <- purrr::reduce(1:10, ~ cons(.y, .x), .init = nil)

measures <- microbenchmark(
    tr_manual_contains(llist1, "qux"),
    manual_contains_rec(llist1, "qux"),
    manual_contains(llist1, "qux")
)
measures
autoplot(measures)


cases_contains <- function(llist, key) {
    cases(llist,
          NIL -> FALSE,
          CONS(car, cdr) ->
              if (car == key) TRUE else cases_contains(cdr, key))
}

tr_contains <- tailr::loop_transform(cases_contains, byte_compile = FALSE)

llist2 <- purrr::reduce(1:10, ~ CONS(.y, .x), .init = NIL)
#measures <- microbenchmark(
#    manual_contains_rec(llist1, "qux"),
#    manual_contains(llist1, "qux"),
#    cases_contains(llist2, "qux"),
#    tr_contains(llist2, "qux")
#)
#measures
#autoplot(measures)

#measures <- microbenchmark(
#    cases_contains(llist2, "qux"),
#    tr_contains(llist2, "qux")
#)
#measures
#autoplot(measures)


const_constructor_test <- function(pattern_expr, eval_env) {
    # Is it a constructor?
    if (rlang::is_symbol(pattern_expr) &&
        exists(rlang::as_string(pattern_expr), eval_env)) {

        name <- rlang::as_string(pattern_expr)
        val <- get(name, eval_env)
        val_constructor <- attr(val, "constructor_constant")

        if (!rlang::is_null(val_constructor)) {
            # We have a constructor but is it the actual constant?
            if (val_constructor == name) {
                test_expr <- rlang::expr({
                    expr_constructor <- attr(.match_expr, "constructor")
                    !rlang::is_null(expr_constructor) && !!name == expr_constructor
                })
                return(test_expr)
            }
        }
    }

    NULL
}

var_test <- function(pattern_expr, eval_env) {
    # This function *must* be tested after the const constructor test
    if (rlang::is_symbol(pattern_expr)) TRUE else NULL
}

transform_match <- function(pattern_expr, eval_expr, eval_env, next_case) {

    test <- const_constructor_test(pattern_expr, eval_env)
    if (!rlang::is_null(test)) {
        return(
            call("if", test, eval_expr, next_case)
        )
    }

    test <- var_test(pattern_expr, eval_env)
    if (!rlang::is_null(test)) {
        assignments <- rlang::expr(
            !!pattern_expr <- .match_expr
        )
        body <- call(
            "{",
            assignments,
            eval_expr
        )
        return(
            call("if", test, body, next_case)
        )
    }
}

transform_match(
    quote(NIL),
    quote(TRUE),
    environment(),
    NULL
)

transform_match(
    quote(x),
    quote(x + 2),
    environment(),
    NULL
)


case_func <- function(expr, ...) {
    matchings <- rlang::quos(...)

    match_cases <- rlang::expr({
        stop("None of the patterns match.")
    })
    for (i in rev(seq_along(matchings))) {
        eval_env <- rlang::get_env(matchings[[i]])
        match_expr <- rlang::quo_expr(matchings[[i]])

        # the order of test and result depend on the syntax... for `->` the
        # R parser will switch the two; for `~` it will not.
        switch(as.character(match_expr[[1]]),
               "<-" = {
                   pattern_expr <- match_expr[[3]]
                   eval_expr <- match_expr[[2]]
               },
               "~" = {
                   eval_expr <- match_expr[[2]]
                   pattern_expr <- match_expr[[3]]
               },
               stop(paste0("Unexpected pattern ", match_expr[[1]]))
        )

        match_cases <- transform_match(pattern_expr, eval_expr, eval_env, match_cases)
    }
    rlang::new_function(
        alist(.match_expr =, ... =),
        match_cases,
        rlang::get_env(matchings[[1]])
    )
}

f <- case_func(llist, NIL -> "foo", x -> x)
f
f(NIL)
f("bar")
