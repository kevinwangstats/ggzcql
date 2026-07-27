#' zcql Palette
#'
#' Color palette inspired by zcql.
#'
#' @param palette Palette type ("default").
#' @param alpha Transparency level, a number between 0 and 1.
#'
#' @export pal_zcql
#'
#' @examples
#' library("scales")
#' show_col(pal_zcql()(4))
pal_zcql <- function(palette = c("default"), alpha = 1) {
  palette <- match.arg(palette)

  if (alpha < 0 || alpha > 1) {
    stop("alpha must be in [0, 1]")
  }

  raw_cols <- ggzcql_palettes[["zcql"]][[palette]]

  function(n) {
    if (n > length(raw_cols)) {
      warning(paste0("zcql palette only has ", length(raw_cols), " colors, returning interpolated colors."))
      cols <- grDevices::colorRampPalette(raw_cols)(n)
    } else {
      cols <- raw_cols[1:n]
    }

    if (alpha < 1) {
      cols <- unname(sapply(cols, function(hex) {
        rgb_val <- grDevices::col2rgb(hex)
        grDevices::rgb(rgb_val[1], rgb_val[2], rgb_val[3], maxColorValue = 255, alpha = alpha * 255)
      }))
    }

    cols
  }
}

#' zcql Color Scales for ggplot2
#'
#' See [pal_zcql()] for details.
#'
#' @param palette Palette type ("default").
#' @param alpha Transparency level, a number between 0 and 1.
#' @param ... Additional arguments passed to [ggplot2::discrete_scale()].
#'
#' @export scale_color_zcql
#'
#' @examples
#' library("ggplot2")
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point() +
#'   scale_color_zcql() +
#'   theme_zcql()
scale_color_zcql <- function(palette = c("default"), alpha = 1, ...) {
  palette <- match.arg(palette)
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = pal_zcql(palette = palette, alpha = alpha),
    ...
  )
}

#' @rdname scale_color_zcql
#' @export scale_colour_zcql
scale_colour_zcql <- scale_color_zcql

#' @rdname scale_color_zcql
#' @export scale_fill_zcql
scale_fill_zcql <- function(palette = c("default"), alpha = 1, ...) {
  palette <- match.arg(palette)
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = pal_zcql(palette = palette, alpha = alpha),
    ...
  )
}

#' zcql Theme for ggplot2
#'
#' A custom ggplot2 theme with background color `#937642` inspired by the aged silk canvas of "A Thousand Li of Rivers and Mountains".
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#'
#' @export theme_zcql
#'
#' @examples
#' library("ggplot2")
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point() +
#'   scale_color_zcql() +
#'   theme_zcql()
theme_zcql <- function(base_size = 11, base_family = "") {
  bg_color <- ggzcql_bg[["zcql"]]

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = bg_color, color = NA),
      panel.background = ggplot2::element_rect(fill = bg_color, color = NA),
      legend.background = ggplot2::element_rect(fill = bg_color, color = NA),
      legend.box.background = ggplot2::element_rect(fill = bg_color, color = NA)
    )
}

# Pre-defined background color (Aged Silk Canvas from "A Thousand Li of Rivers and Mountains")
ggzcql_bg <- c(zcql = "#937642")

# Pre-defined palette colors (Fresh Sky, Celadon, Olive Wood, Tea Green)
ggzcql_palettes <- list(
  zcql = list(
    default = c(
      "#2AA4E5", # Fresh Sky
      "#80C79A", # Celadon
      "#856A3B", # Olive Wood
      "#D7FEBF"  # Tea Green
    )
  )
)
