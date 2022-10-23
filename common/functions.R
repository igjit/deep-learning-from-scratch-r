softmax <- function(a) {
  exp_a <- exp(a - max(a))
  exp_a / sum(exp_a)
}

sigmoid <- function(x) 1 / (1 + exp(-x))

rep_row <- function(x, n) matrix(rep(x, n), n, byrow = TRUE)
