layout_matrix2tlbr <- function(layout_matrix, n) {
  positions <- data.frame(
    do.call(rbind,
            lapply(sort(unique(as.vector(layout_matrix))),
                   function(ii) {
                     ind <- which(layout_matrix == ii, arr.ind = TRUE)
                     c(l = min(ind[, "col"], na.rm = TRUE),
                       r = max(ind[, "col"], na.rm = TRUE),
                       t = min(ind[, "row"], na.rm = TRUE),
                       b = max(ind[, "row"], na.rm = TRUE))
                   })
    )
  )
  positions[seq(n), ]
}
