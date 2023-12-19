#' Generate a Xmas Tree
#'
#' Simple function that generates a unique christmas tree, varying
#' the colors based on the uniform distribution
#'
#' @return a ggplot2 object
#'
#' @import ggplot2
#'
#' @export
#'

xmastree <- function() {

  # 50 X 50 size pixel map
  holly <- 1:50
  jolly <- 1:50

  # expand to map
  df <- expand.grid(holly,jolly)
  colnames(df) <- c("holly", "jolly")

  xmas_vals <- c()
  for (i in 1:25) {
    if (i < 5) {
      row <- c(-runif(21, 3.5, 5),
               -runif(8, 0, 0.5),
               -runif(21, 3.5, 5))
    } else {

      tree <- runif(50-i*2, 2, 5)
      row <- c(-runif(i, 3.5, 5),
               tree,
               -runif(i, 3.5, 5))
    }
    xmas_vals <- c(xmas_vals, row)
    xmas_vals <- c(xmas_vals, row)
  }

  df$xmas <- xmas_vals

  p <- ggplot(df, aes(x=holly, y=jolly, fill=xmas)) +
    geom_tile() +
    scale_fill_gradient2(low="red", mid="darkgreen", high="green", midpoint=3) +
    theme_bw() +
    guides(fill="none")

  return(p)

}

