softmax <- function(a) {
  exp_a <- exp(a - max(a))
  exp_a / sum(exp_a)
}

sigmoid <- function(x) 1 / (1 + exp(-x))

rep_row <- function(x, n) matrix(rep(x, n), n, byrow = TRUE)

cross_entropy_error <- function(y, t) {
  if (is.vector(y)) {
    y <- matrix(y, 1)
    t <- matrix(t, 1)
  }

  if (identical(dim(t), dim(y))) {
    t <- apply(t, 1, which.max) - 1
  }

  batch_size <- nrow(y)

  -sum(log(map2_dbl(1:batch_size, t, ~ y[.x, .y + 1]) + 1e-7)) / batch_size
}

numerical_gradient <- function(f, x) {
  h <- 1e-4
  if (is.vector(x)) x <- matrix(x, 1)
  grad <- matrix(0, nrow(x), ncol(x))

  for (i in 1:nrow(x)) {
    for (j in 1:ncol(x)) {
      x1 <- identity(x)
      x1[i, j] <- x[i, j] + h
      x2 <- identity(x)
      x2[i, j] <- x[i, j] - h
      grad[i, j] <- (f(x1) - f(x2)) / (2 * h)
    }
  }

  grad
}
