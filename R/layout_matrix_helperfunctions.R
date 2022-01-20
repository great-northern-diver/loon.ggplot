layout_matrix2positions <- function(layout_matrix, n = NULL) {

  if(is.null(n)) n <- len_unique(na.omit(c(layout_matrix)))

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

positions2layout_matrix <- function(positions) {

  if(is.list(positions)) {
    t <- positions$t
    b <- positions$b
    l <- positions$l
    r <- positions$r
  } else if (is.matrix(positions)) {
    t <- positions[, "t"]
    b <- positions[, "b"]
    l <- positions[, "l"]
    r <- positions[, "r"]
  } else stop("Unknown Data Structure of `positions`")

  n <- length(t)

  nrow <- max(t, b)
  ncol <- max(l, r)

  layout_matrix <- matrix(NA, nrow = nrow, ncol = ncol)

  for(i in seq(n)) {
    layout_matrix[t[i]:b[i], l[i]:r[i]] <- i
  }

  layout_matrix
}

layout_matrixExtend <- function(layout_matrix, k, new_layout_matrix) {
  # layout_matrix: original layout_matrix, e.g.,
  # 1 2 NA
  # 3 3 NA
  # 3 3 4
  # k: which one, e.g., `3`, will be replaced re-arranged
  # new_layout_matrix: the layout_matrix of the replaced one, e.g.,
  # 5 5 6
  # 5 5 6
  # NA NA 7
  # The output matrix should be (`3` will be replaced by `new_layout_matrix`)
  # 1 1 1 2 2 2 NA NA NA
  # 1 1 1 2 2 2 NA NA NA
  # 1 1 1 2 2 2 NA NA NA
  # 5 5 5 5 6 6 NA NA NA
  # 5 5 5 5 6 6 NA NA NA
  # 5 5 5 5 6 6 NA NA NA
  # 5 5 5 5 6 6 4 4 4
  # NA NA NA NA 7 7 4 4 4
  # NA NA NA NA 7 7 4 4 4

  lcm <- function(x,y) {
    gcd <- function(x,y) {
      r <- x%%y;
      return(ifelse(r, gcd(y, r), y))
    }
    x*y/gcd(x,y)
  }

  ############ extend new_layout_matrix
  kmat <- which(layout_matrix == k, arr.ind = TRUE)
  krow <- len_unique(kmat[, "row"])
  kcol <- len_unique(kmat[, "col"])

  newrow <- nrow(new_layout_matrix)
  newcol <- ncol(new_layout_matrix)

  rowExtend <- lcm(newrow, krow)
  new_layout_matrix <- matrix(rep(new_layout_matrix, each = rowExtend/newrow), nrow = rowExtend)

  colExtend <- lcm(newcol, kcol)
  new_layout_matrix <- t(apply(new_layout_matrix, 1, function(x) rep(x, each = colExtend/newcol)))

  ############ extend layout_matrix

  nrow <- nrow(layout_matrix)
  ncol <- ncol(layout_matrix)

  extendedRow <- rowExtend/krow * nrow
  extendedCol <- colExtend/kcol * ncol

  layout_matrix <- matrix(rep(layout_matrix,
                              each = rowExtend/krow),
                          nrow = extendedRow)
  layout_matrix <- t(apply(layout_matrix, 1,
                           function(x) rep(x, each = colExtend/kcol)))

  layout_matrix[which(layout_matrix == k)] <- new_layout_matrix
  layout_matrix
}

# In a layout_matrix, any values greater than i will add an offset
# e.g.,
# 1 2 3
# 4 5 6
## set i = 4, offset = 5
## 1 2 3
## 4 10 11
update_layout_matrix <- function(layout_matrix, i, offset) {
  nrow <- nrow(layout_matrix)
  vm <- c(layout_matrix)
  vm[is.na(vm)] <- -1
  vm[vm > i] <- vm[vm > i] + offset
  vm[vm == -1] <- NA
  matrix(vm, nrow = nrow)
}
