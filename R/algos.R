# Conte ===================================================
# img should already be a 2D raster matrix
# x are coordinate positions where to start the algo (center point otherwise)
algo_Conte <- function(img, x=image_center(img)){

  # algo starts here
  while (abs(img[x[1], x[2]] - img[x[1] - 1, x[2]]) < 0.1) {
    x[1] <- x[1] + 1
  }
  a <- 1
  M <- matrix(c(
    0, -1, -1, -1,  0,  1, 1, 1,
    1,  1,  0, -1, -1, -1, 0, 1),
    nrow = 2, ncol = 8, byrow = TRUE)
  M <- cbind(M[, 8], M, M[, 1])
  X <- 0
  Y <- 0
  x1 <- x[1]
  x2 <- x[2]
  SS <- NA
  S <- 6
  while ((any(c(X[a], Y[a]) != c(x1, x2)) | length(X) < 3)) {
    if (abs(img[x[1] + M[1, S + 1], x[2] + M[2, S + 1]] - img[x[1], x[2]]) < 0.1) {
      a <- a + 1
      X[a] <- x[1]
      Y[a] <- x[2]
      x <- x + M[, S + 1]
      SS[a] <- S + 1
      S <- (S + 7) %% 8
    }
    else if (abs(img[x[1] + M[1, S + 2], x[2] + M[2, S + 2]] - img[x[1], x[2]]) < 0.1) {
      a <- a + 1
      X[a] <- x[1]
      Y[a] <- x[2]
      x <- x + M[, S + 2]
      SS[a] <- S + 2
      S <- (S + 7) %% 8
    }
    else if (abs(img[x[1] + M[1, S + 3], x[2] + M[2, S + 3]] - img[x[1], x[2]]) < 0.1) {
      a <- a + 1
      X[a] <- x[1]
      Y[a] <- x[2]
      x <- x + M[, S + 3]
      SS[a] <- S + 3
      S <- (S + 7) %% 8
    }
    else {
      S <- (S + 1) %% 8
    }
  }
  # in Julien's book and in retired Momocs
  # return(cbind((Y[-1]), ((dim(img)[1] - X))[-1]))
  res <- cbind(Y, X)

  # fix algo that may return first point as (0, 0)
  if (all(res[1, ] == c(0, 0)))
    res <- res[-1,, drop=FALSE]

  # return this beauty
  return(res)
}
