# Fuzzy supplementary poverty estimation.
#
# @param x a numeric vector
# @param y a numeric vector
#
# @return the sum of x,y
#
modifiedSum <- function(x, y) {

  replace(x, is.na(x), 0) + replace(y, is.na(y), 0)
}
