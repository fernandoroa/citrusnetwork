AnyItemIfTitle <- function(vector, xCasted, spaces, dots = 1, booltitle) {
  listOfRefs[[i]] <- list()

  xCasted[, setdiff(vector, colnames(xCasted))] <- NA
  for (k in 1) {
    listOfRefs[[i]][k] <- paste0(
      "  \n  \n",
      paste(xCasted[, which(colnames(xCasted) %in% vector[k])][i])
    )
  }
  if (length(vector) > 1) {
    for (k in 2:length(vector)) {
      listOfRefs[[i]][k] <- ifelse(!is.na(xCasted[, which(colnames(xCasted) %in% vector[k])][i]),
        paste0(
          paste0(c(rep(".", dots)), collapse = ""),
          paste0(c(rep("&nbsp;", spaces)), collapse = ""),
          paste(xCasted[, which(colnames(xCasted) %in% vector[k])][i])
        ),
        ""
      )
    }
  }
  return(listOfRefs)
}
