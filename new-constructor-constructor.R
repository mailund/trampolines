

structure_vector <- function(n) {
    i <- 1
    numbers <- c()
    while (i <= n) {
        numbers <- c(numbers, i)
        i <- i + 1
    }
}

library(pmatch)
llist := NIL | CONS(car, cdr : llist)

structure_pmatch_llist <- function(n) {
    i <- 1
    numbers <- NIL
    while (i <= n) {
        numbers <- CONS(i, numbers)
        i <- i + 1
    }
}

structure_pmatch_llist2 <- function(n) {
    i <- 1
    numbers <- NIL
    while (i <= n) {
        numbers <- CONS2(i, numbers)
        i <- i + 1
    }
}

structure_pmatch_llist3 <- function(n) {
    i <- 1
    numbers <- NIL
    while (i <= n) {
        numbers <- CONS3(i, numbers)
        i <- i + 1
    }
}

nil <- NULL
cons <- function(car, cdr) list(car = car, cdr = cdr)
structure_manual_llist <- function(n) {
    i <- 1
    numbers <- nil
    while (i <= n) {
        numbers <- cons(i, numbers)
        i <- i + 1
    }
}


make_constructor_function <- function(constructor, data_type_name, env) {
    constructor_name <- rlang::quo_name(constructor[[1]])
    constructor_arguments <- process_arguments(constructor[-1])

    # there is a bit of code involved here, but it doesn't matter
    # if it is slow. What matters is that the constructor is not.
    vars <- constructor_arguments$arg
    list_expr <- setNames(rlang::exprs(!!!rlang::syms(vars)), vars)

    no_typechecks <- 0
    typechecks <- list()
    for (i in seq_along(constructor_arguments)) {
        arg <- constructor_arguments[i,]
        if (!is.na(arg$type)) {
            var <- rlang::sym(arg$arg)
            type <- arg$type

            err_msg <- glue::glue(
                "The argument {var} should be of type {type}."
            )

            ex <- rlang::expr(
                if (!inherits(!!var, !!type))
                    stop(simpleError(!!err_msg, call = match.call()))
            )
            typechecks <- c(typechecks, ex)
            no_typechecks <- no_typechecks + 1
        }
    }
    length(typechecks) <- no_typechecks

    # create the constructor function
    func_args <- make_args_list(vars)
    body <- rlang::expr({
        !!!typechecks
        args <- !!list_expr
        class(args) <- !!data_type_name
        attr(args, "constructor") <- !!constructor_name
        args
    })
    constructor <- rlang::new_function(
        func_args, body, env
    )

    # set meta information about the constructor
    class(constructor) <- c("constructor", "function")

    # put the constructor in the binding scope
    assign(constructor_name, constructor, envir = env)
}

make_constructor_function(
    rlang::expr(CONS2(car, cdr : llist)),
    "llist",
    environment()
)
make_constructor_function(
    rlang::expr(CONS3(car, cdr)),
    "llist",
    environment()
)


library(microbenchmark)
measures <- microbenchmark(
    structure_pmatch_llist(100),
    structure_pmatch_llist2(100),
    structure_pmatch_llist3(100),
    structure_manual_llist(100)
)
measures
autoplot(measures)



