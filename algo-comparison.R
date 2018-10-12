# Application: sample numbers until you get a collision

app_vector <- function(n, m) {
    samples <- sample(1:n, size = m, replace = TRUE)

    i <- 1
    numbers <- c()
    while ( !(samples[i] %in% numbers) && i <= m) {
        numbers <- c(numbers, samples[i])
        i <- i + 1
    }
    if (i == m + 1)
        stop("We ran out of samples")
    else
        i - 1
}

# linked lists
library(pmatch)
llist := Nil | Cons(car, cdr : llist)
contains <- function(llist, key) {
    cases(llist,
          Nil -> FALSE,
          Cons(car, cdr) ->
              if (car == key) TRUE else contains(cdr, key))
}


app_llist <- function(n, m) {
    samples <- sample(1:n, size = m, replace = TRUE)

    i <- 1
    numbers <- Nil
    while (!contains(numbers, samples[i]) && i <= m) {
        numbers <- Cons(samples[i], numbers)
        i <- i + 1
    }
    if (i == m + 1)
        stop("We ran out of samples")
    else
        i - 1
}

nil <- NULL
cons <- function(car, cdr) {
    list(car = car, cdr = cdr)
}
contains2 <- function(llist, key) {
    while (!is.null(llist)) {
        if (llist$car == key) return(TRUE)
        llist <- llist$cdr
    }
    FALSE
}

app_llist2 <- function(n, m) {
    samples <- sample(1:n, size = m, replace = TRUE)

    i <- 1
    numbers <- nil
    while (!contains2(numbers, samples[i]) && i <= m) {
        numbers <- cons(samples[i], numbers)
        i <- i + 1
    }
    if (i == m + 1)
        stop("We ran out of samples")
    else
        i - 1
}


library(microbenchmark)
microbenchmark(
    app_vector(100, 1000),
    app_llist(100, 1000),
    app_llist2(100, 1000)
)

microbenchmark(
    app_vector(1000, 1000),
#    app_llist(100, 1000),
    app_llist2(1000, 1000)
)

Tree := Empty | Tree(left : Tree, key, right : Tree)

insert <- function(tree, key) {
    cases(tree,
          Empty -> Tree(Empty, key, Empty),
          Tree(left, k, right) ->
              if (k < key) Tree(left, k, insert(right, key))
          else if (k > key) Tree(insert(left, key), k, right)
          else Tree(left, key, right)
    )
}

tree_contains <- function(tree, key) {
    cases(tree,
          Empty -> FALSE,
          Tree(left, k, right) ->
              if (k < key) tree_contains(right, key)
              else if (k > key) tree_contains(left, key)
              else TRUE)
}

app_tree <- function(n, m) {
    samples <- sample(1:n, size = m, replace = TRUE)

    i <- 1
    numbers <- Empty
    while (!tree_contains(numbers, samples[i]) && i <= m) {
        numbers <- insert(numbers, samples[i])
        i <- i + 1
    }
    if (i == m + 1)
        stop("We ran out of samples")
    else
        i - 1
}

microbenchmark(
    app_vector(100, 1000),
    #app_llist(100, 1000),
    app_llist2(100, 1000),
    app_tree(100, 1000)
)


empty <- NULL
tree <- function(left, key, right) {
    list(left = left, key = key, right = right)
}

insert2 <- function(tr, key) {
    if (is.null(tr)) tree(empty, key, empty)
    else if (tr$key < key) tree(tr$left, tr$key, insert2(tr$right, key))
    else if (tr$key > key) tree(insert2(tr$left, key), tr$key, tr$right)
    else tr
}

tree_contains2 <- function(tr, key) {
    if (is.null(tr)) FALSE
    else if (tr$key < key) tree_contains2(tr$right, key)
    else if (tr$key > key) tree_contains2(tr$left, key)
    else TRUE
}

app_tree2 <- function(n, m) {
    samples <- sample(1:n, size = m, replace = TRUE)

    i <- 1
    numbers <- empty
    while (!tree_contains2(numbers, samples[i]) && i <= m) {
        numbers <- insert2(numbers, samples[i])
        i <- i + 1
    }
    if (i == m + 1)
        stop("We ran out of samples")
    else
        i - 1
}

plot(microbenchmark(
    app_vector(10, 1000),
    app_llist(10, 1000),
    app_tree(10, 1000),
    app_llist2(10, 1000),
    app_tree2(10, 1000)
))

timeit <- function(f) {
    Vectorize(
        function(n) {
            times <- purrr::rerun(10, {
                then <- Sys.time()
                f(n)
                now <- Sys.time()
                now - then
            })
            mean(unlist(times))
        }
    )
}

app_vector <- function(n) {
    i <- 1
    numbers <- c()
    while (i <= n) {
        numbers <- c(numbers, i)
        i <- i + 1
    }
}

app_llist <- function(n) {
    i <- 1
    numbers <- Nil
    while (i <= n) {
        numbers <- Cons(samples[i], numbers)
        i <- i + 1
    }
}

app_llist2 <- function(n) {
    i <- 1
    numbers <- nil
    while (i <= n) {
        numbers <- cons(i, numbers)
        i <- i + 1
    }
}


ns <- seq(100, 200, by = 10)
app_vector_times <- tibble::tibble(
    n = ns,
    app = "Vector",
    times = timeit(app_vector)(ns)
)
app_llist_times <- tibble::tibble(
    n = ns,
    app = "Linked list (pmatch)",
    times = timeit(app_llist)(ns)
)
app_llist2_times <- tibble::tibble(
    n = ns,
    app = "Linked list",
    times = timeit(app_llist2)(ns)
)

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


