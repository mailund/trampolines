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


