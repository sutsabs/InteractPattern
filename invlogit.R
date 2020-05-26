# Utils for the in-class examples

invlogit <- function(x) {
  exp(x) / (1 + exp(x))
}
