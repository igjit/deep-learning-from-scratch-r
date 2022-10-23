softmax <- function(a) {
  exp_a <- exp(a - max(a))
  exp_a / sum(exp_a)
}
