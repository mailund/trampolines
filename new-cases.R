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

measures <- microbenchmark::microbenchmark(
    manual_contains_rec(llist1, "qux"),
    manual_contains(llist1, "qux"),
    cases_contains(llist2, "qux"),
    tr_contains(llist2, "qux")
)
print(measures, order = "mean", unit = "relative")
ggplot2::autoplot(measures)

#measures <- microbenchmark(
#    cases_contains(llist2, "qux"),
#    tr_contains(llist2, "qux")
#)
#measures
#autoplot(measures)

func_constructor_test <- function(pattern_expr, nesting, eval_env) {
    if (rlang::is_call(pattern_expr)) {
        name <- rlang::as_string(pattern_expr[[1]])
        constructor <- get(name, eval_env)

        constructor_vars <- names(formals(constructor))

        if (!rlang::is_null(constructor) && inherits(constructor, "constructor")) {

            test_exprs <- rlang::expr(
                !!name == attr(!!nesting, "constructor")
            )

            for (i in 2:length(pattern_expr)) {
                var <- rlang::sym(constructor_vars[i - 1])
                nesting_nesting <- call("$", nesting, var)
                test_exprs <- c(
                    test_exprs,
                    transform_match(
                        pattern_expr[[i]],
                        nesting_nesting,
                        eval_env
                ))
            }
            return(as.call(c(quote(all), test_exprs)))
        }
    }
    NULL
}

const_constructor_test <- function(pattern_expr, nesting, eval_env) {
    # Is it a constructor?
    if (rlang::is_symbol(pattern_expr) &&
        exists(rlang::as_string(pattern_expr), eval_env)) {

        name <- rlang::as_string(pattern_expr)
        val <- get(name, eval_env)
        val_constructor <- attr(val, "constructor_constant")

        if (!rlang::is_null(val_constructor)) {
            # We have a constructor but is it the actual constant?
            if (val_constructor == name) {
                test_expr <- rlang::expr(
                    is.na(!!nesting) && attr(!!nesting, "constructor") == !!name
                )
                return(test_expr)
            }
        }
    }

    NULL
}

const_test <- function(pattern_expr, nesting, eval_env) {

    # This function *must* be tested after the const constructor test
    if (rlang::is_atomic(pattern_expr)) {
        rlang::expr({
            !!pattern_expr == !!nesting
        })
    } else {
        NULL
    }
}

var_test <- function(pattern_expr, nesting, eval_env) {

    # This function *must* be tested after the const constructor test
    if (rlang::is_symbol(pattern_expr)) {
        rlang::expr({
            !!pattern_expr <- !!nesting
            TRUE
        })
    } else {
        NULL
    }
}


transform_match <- function(pattern_expr, nesting, eval_env) {

    test_funcs <- c(
        func_constructor_test,
        const_constructor_test,
        const_test,
        var_test
    )
    for (func in test_funcs) {
        test <- func(pattern_expr, nesting, eval_env)
        if (!rlang::is_null(test)) {
            return(test)
        }
    }
    stop(glue::glue("Malformed pattern {pattern_expr}"))
}

transform_match(
    quote(NIL),
    quote(.match_expr),
    environment()
)

transform_match(
    quote(x),
    quote(.match_expr),
    environment()
)

transform_match(
    quote(CONS(1, x)),
    quote(.match_expr),
    environment()
)


case_func <- function(...) {

    matchings <- rlang::quos(...)
    func_args <- c()
    eval_env <- rlang::caller_env()

    match_cases <- rlang::expr({
        stop("None of the patterns match.")
    })
    for (i in rev(seq_along(matchings))) {
        match_expr <- rlang::quo_expr(matchings[[i]])
        if (rlang::is_symbol(match_expr)) {
            x <- list(rlang::missing_arg())
            names(x) <- rlang::as_string(match_expr)
            func_args <- c(func_args, x)
            next
        }
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
               # not a pattern, make it an argument
               {
                   func_args <- c(func_args, matchings[i])
                   next # not a pattern
               }
        )

        match_cases <-
            call("if",
                 transform_match(pattern_expr, quote(.match_expr), eval_env),
                 eval_expr,
                 match_cases)
    }

    func_args <- lapply(func_args, rlang::quo_squash)
    func_args <- c(list(.match_expr = rlang::missing_arg()), func_args)
    rlang::new_function(
        func_args,
        match_cases,
        eval_env
    )
}

factorial <- case_func(
    acc = 1,
    1 -> acc,
    n -> factorial(n - 1, acc * n)
)
factorial(1)
factorial(3)
factorial(4)

llength <- case_func(
    acc = 0,
    NIL -> acc,
    CONS(car, CONS(8, .)) -> car,
    CONS(., cdr) -> llength(cdr, acc + 1)
)
llength
llength(NIL)
llength(llist2)


new_cases_contains <- case_func(
    key,
    NIL -> FALSE,
    CONS(car, cdr) -> if (car == key) TRUE else new_cases_contains(cdr, key)
)

tr_contains <- tailr::loop_transform(cases_contains, byte_compile = FALSE)
new_tr_contains <- tailr::loop_transform(new_cases_contains, byte_compile = FALSE)

manual_contains_rec2 <- function(llist, key) {
    # NIL test
    if (is.na(llist) && attr(llist, "constructor") == "NIL")
        FALSE
    else if (llist$car == key)
        TRUE
    else
        manual_contains_rec2(llist$cdr, key)
}

manual_contains_rec3 <- function(llist, key) {
    # NIL test
    if (is.na(llist) && attr(llist, "constructor") == "NIL")
        FALSE
    else if (attr(llist, "constructor") == "CONS") {
        if (llist$car == key)
            TRUE
        else
            manual_contains_rec3(llist$cdr, key)
    }
}

measures <- microbenchmark(
    manual_contains(llist1, "qux"),
    manual_contains_rec(llist1, "qux"),
    manual_contains_rec2(llist2, "qux"),
    manual_contains_rec3(llist2, "qux"),
    new_cases_contains(llist2, "qux"),
    cases_contains(llist2, "qux"),
    tr_contains(llist2, "qux"),
    new_tr_contains(llist2, "qux")
)
print(measures, unit = "relative", order = "mean")
