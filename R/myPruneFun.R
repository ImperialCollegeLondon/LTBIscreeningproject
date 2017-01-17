
myPruneFun <- function(x, leafName) {
  if (isNotLeaf(x)) return (TRUE)
  if (x$name!=leafName) return (FALSE)
  return (TRUE)
}

