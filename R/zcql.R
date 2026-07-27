#' zcql Palette
#'
#' Color palette inspired by "A Thousand Li of Rivers and Mountains" (千里江山图).
#'
#' @param palette Palette type ("default").
#' @param alpha Transparency level, a number between 0 and 1.
#'
#' @export pal_zcql
#'
#' @examples
#' library("scales")
#' show_col(pal_zcql()(5))
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
#' A custom ggplot2 theme with a light golden silk background (`#E8D7B0`) inspired by the luminous silk canvas of "A Thousand Li of Rivers and Mountains".
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
      legend.box.background = ggplot2::element_rect(fill = bg_color, color = NA),
      panel.grid.major = ggplot2::element_line(color = "#D4C299", linewidth = 0.5),
      panel.grid.minor = ggplot2::element_line(color = "#DECFA9", linewidth = 0.25),
      text = ggplot2::element_text(color = "#2C2216"),
      axis.text = ggplot2::element_text(color = "#3D301F")
    )
}

# Pre-defined light golden silk background color
ggzcql_bg <- c(zcql = "#E8D7B0")

# Pre-defined mineral palette colors (Azurite Blue, Malachite Green, Cinnabar Red, Spring Green, Raw Ochre)
ggzcql_palettes <- list(
  zcql = list(
    default = c(
      "#1E88E5", # Mineral Azurite Cyan-Blue
      "#388E3C", # Malachite Jade Green
      "#D32F2F", # Cinnabar Red
      "#7CB342", # Soft Spring Green
      "#8D6E63"  # Raw Ochre
    )
  )
)
