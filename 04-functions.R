
# pure function -----------------------------------------------------------

f <- function(x, y = 0) {
  x + y
}

f(1, 2)

f(x = 2)


# function with a side effect ---------------------------------------------


g <- function(x) {
  x + y
}


y <- 3
g(2)

y <- 5
g(2)


# lapply, sapply ---------------------------------------------------------------

ls <- list(
  norm = rnorm(10),
  zeros = integer(20),
  integers = sample(1:100, size = 5)
)

ls

mean_ls <- lapply(ls, mean)
mean_ls

mean_vec <- sapply(ls, mean)
mean_vec


# apply -------------------------------------------------------------------

mat <- matrix(
  data = sample(1:100, size = 12),
  ncol = 4
)

mat

colnames(mat) <- LETTERS[1:4]
rownames(mat) <- letters[1:3]

mat

apply(mat, 1, mean)
apply(mat, 2, mean)

mean






