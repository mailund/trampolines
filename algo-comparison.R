# Application: sample numbers until you get a collision

app_vector <- function(n) {
    i <- 1
    numbers <- c()
    while (i <= n) {
        numbers <- c(numbers, i)
        i <- i + 1
    }
}

library(pmatch)
llist := Nil | Cons(car, cdr : llist)

app_llist <- function(n) {
    i <- 1
    numbers <- Nil
    while (i <= n) {
        numbers <- Cons(i, numbers)
        i <- i + 1
    }
}

nil <- NULL
cons <- function(car, cdr) list(car = car, cdr = cdr)
app_llist2 <- function(n) {
    i <- 1
    numbers <- nil
    while (i <= n) {
        numbers <- cons(i, numbers)
        i <- i + 1
    }
}

# not used...
contains <- function(llist, key) {
    cases(llist,
          Nil -> FALSE,
          Cons(car, cdr) ->
              if (car == key) TRUE else contains(cdr, key))
}

contains2 <- function(llist, key) {
    while (!is.null(llist)) {
        if (llist$car == key) return(TRUE)
        llist <- llist$cdr
    }
    FALSE
}
###



measures <- microbenchmark(
    app_vector(100),
    app_llist(100),
    app_llist2(100)
)
measures
plot(measures)


timeit <- function(f) {
    Vectorize(
        function(n) {
            times <- purrr::rerun(100, {
                then <- Sys.time()
                f(n)
                now <- Sys.time()
                now - then
            })
            mean(unlist(times))
        }
    )
}

ns <- seq(100, 1000, by = 100)
app_vector_times <- tibble::tibble(
    n = ns,
    app = "Vector",
    times = timeit(app_vector)(ns)
)
app_llist2_times <- tibble::tibble(
    n = ns,
    app = "Linked list",
    times = timeit(app_llist2)(ns)
)

library(magrittr)
library(ggplot2)
rbind(app_vector_times, #app_llist_times,
      app_llist2_times) %>%
    ggplot(aes(x = n, y = times, colour = app)) +
    geom_line()

process_arg <- function(argument) {
    error_msg <- glue::glue(
        "The constructor argument is malformed.\n",
        "The expression {deparse(argument)} should either be ",
        "a bare symbol or on the form 'variable : type'."
    )
    if (rlang::is_lang(argument)) {
        if (argument[[1]] != ":") {
            stop(simpleError(error_msg, call = argument))
        }
        arg <- rlang::quo_name(argument[[2]])
        type <- rlang::quo_name(argument[[3]])
        tibble::tibble(arg = arg, type = type)
    } else if (rlang::is_symbol(argument)) {
        arg <- rlang::quo_name(argument)
        tibble::tibble(arg = arg, type = NA)
    } else {
        stop(simpleError(error_msg, call = argument))
    }
}

#' Construct a tibble from all the arguments of a constructor
#'
#' @param constructor_arguments The arguments provided in the constructor
#'   specification
#' @return The arguments represented as a tibble. The first column contain
#'   argument names, the second their types.
process_arguments <- function(constructor_arguments) {
    dplyr::bind_rows(purrr::map(as.list(constructor_arguments), process_arg))
}

new_process_constructor_function <- function(constructor, data_type_name, env) {
    constructor_name <- rlang::quo_name(constructor[[1]])
    constructor_arguments <- process_arguments(constructor[-1])

    # create the constructor function
    constructor <- function() {
        args <- rlang::as_list(environment())

        # type check!
        for (i in seq_along(args)) {
            arg <- args[[constructor_arguments$arg[i]]]
            type <- constructor_arguments$type[i]
            if (!rlang::is_na(type) && !inherits(arg, type)) {
                # FIXME: how do I get a "bad error message" error here? id:3 gh:42 ic:gh
                error_msg <- glue::glue(
                    "The argument {arg} is of type {class(arg)} ",
                    "but should be of type {type}."
                )
                stop(simpleError(error_msg, call = match.call()))
            }
        }

        structure(args, constructor = constructor_name, class = data_type_name)
    }
    formals(constructor) <- make_args_list(constructor_arguments$arg)

    # set meta information about the constructor
    class(constructor) <- c("constructor", "function")

    # put the constructor in the binding scope
    assign(constructor_name, constructor, envir = env)
}

new_process_constructor_function(rlang::expr(CONS2(car, cdr : llist2)), rlang::expr(llist2), environment())
