##TODO##

#fill in missing probabilities using data.tree traversal functions

osNode <- osNode.health_pdistn

prob_fill <- function(node) {
  node$pmin <- sapply(node$children, function(child) child$p)
}

osNode$Do(prob_fill, traversal = "post-order")
