#' Plots Interaction between Two Interval-valued Variables
#'
#' This function plots the interaction between two variables by creating a
#' bounding box and center points for the variables.
#' @param int_data A data frame with 4 columns representing the centers and
#' ranges of the two variables. The columns should be named as "yc", "yr", "xc",
#' and "xr" respectively.
#' @param title A character string specifying the title of the plot.
#' @param xlabel A character string specifying the label of the x-axis.
#' @param ylabel A character string specifying the label of the y-axis.
#' @return A ggplot object containing the interaction plot.
#' @export
#' @examples
#' int_data <- data.frame(yc = 0, yr = 1, xc = 0, xr = 1)
#' int_plot(int_data, "Interaction Plot", "X-axis", "Y-axis")
#'
int_plot <- function(int_data, title, xlabel, ylabel) {
  if (!is.data.frame(int_data)) {
    # stop if int_data ! = data.frame
    rlang::abort("'int_data' argument must be of type data.frame.")
  }
  if (!length(int_data) == 4) {
    # stop if int_data's length is not 4
    rlang::abort("Length of 'int_data' argument must be 4.")
  }
  if (all(!(sapply(c(title, xlabel, ylabel), is.character))) == TRUE) {
    # stop if any of the labeling parameters is not a character type
    rlang::abort("Atleast one labeling parameter ('title', 'xlabel', 'ylabel')
is not a character type.")
  }
  rlang::warn("Ensure that 'int_data' follows this order.
1. Center of Y
2. Range of Y
3. Center of X
4. Range of X")
  xl <- xu <- yl <- yu <- xc <- yc <- NULL
  colnames(int_data) <- c("yc", "yr", "xc", "xr")
  box <- make_lu_int(int_data)
  colnames(box) <- c("xl", "xu", "yl", "yu")

  # plot
  p <- ggplot2::ggplot() +
    # Bounding Box
    ggplot2::geom_rect(box,
      mapping = ggplot2::aes(
        xmin = xl,
        xmax = xu,
        ymin = yl,
        ymax = yu
      ),
      fill = NA,
      size = .5,
      color = "darkgrey"
    ) +
    # center points
    ggplot2::geom_point(int_data,
      mapping = ggplot2::aes(
        x = xc,
        y = yc
      ),
      size = 0.5,
      color = "black"
    )
  p + ggplot2::ggtitle(title) +
    ggplot2::xlab(xlabel) +
    ggplot2::ylab(ylabel) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text = ggplot2::element_text(size=12),
                   axis.title = ggplot2::element_text(size=14),
                   plot.title = ggplot2::element_text(size=16,
                                             face="bold",
                                             hjust = 0.5))
}
