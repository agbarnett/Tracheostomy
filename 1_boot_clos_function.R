# 1_boot_clos_function.R
# from etm::clos help file
# Feb 2019

boot.clos <- function(data, state.names, tra, cens.name, s = 0, nboot) {
  res <- double(nboot)
  for (i in seq_len(nboot)) {
    index <- sample(unique(data$id), replace = TRUE)
    inds <- new.id <- NULL
    for (j in seq_along(index)){
      ind <- which(data$id == index[j])
      new.id <- c(new.id, rep(j, length(ind)))
      inds <- c(inds, ind)
    }
    dboot <- cbind(data[inds, ], new.id)
    dboot[, which(names(dboot) == "id")]
    dboot$id <- dboot$new.id
    tr.prob <- etm(dboot, state.names, tra, cens.name, s, cova = FALSE)
    res[i] <- etm::clos(tr.prob)$e.phi
  }
  res
}