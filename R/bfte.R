#' @title Ready fonts for bfte theme options
#' @description Loads google fonts via the `showtext`` package.
#' @details Three fonts are loaded by default: *Roboto*, *Nothing You Could Do*, and *Major Mono Display*.
#' @import showtext, purrr
#' @param google_font_name_vector vector of Google font names
#' @param font_ref_vector vector of abbreviated font names
#' @return fonts loaded into namespace
#' @seealso \code{\link{theme_bfte_roboto}}, \code{\link{theme_bfte_nothing}}, \code{\link{theme_bfte_majormono}}
#' @export
#' @examples
#' ready_fonts()
ready_fonts <- function(google_font_name_vector = c("Roboto", "Nothing You Could Do", "Major Mono Display"), font_ref_vector = c("roboto", "nothing", "majormono")) {
  purrr::map2(.x = google_font_name_vector,
              .y = font_ref_vector,
              .f = ~ font_add_google(.x, .y)
  )
}


#' @title BFTE theme with Roboto font
#' @description Adds a themed appearance and guide to a `ggplot2` object.
#' @details The Roboto font is used in the theme.
#' @import ggplot2, ggtext, showtext
#' @return added to a 
#' @seealso \code{\link{ready_fonts}}, \code{\link{theme_bfte_nothing}}, \code{\link{theme_bfte_majormono}}
#' @export
#' @examples
#' ggplot(data = data(mtcars), aes(x = cyl, y = mpg)) + geom_point() + theme_bfte_roboto()
theme_bfte_roboto <- list(
  theme_classic(base_family = "roboto", base_size = 16) +
  theme(panel.grid.major = element_line(linetype = 3, color = "grey", linewidth = 0.5),
        legend.position = "none",
        axis.text.x = ggtext::element_markdown(angle = 90, vjust = 0.5, hjust = 1, color = "black"),
        axis.text.y = ggtext::element_markdown(color = "black")),
  guides(x = guide_axis(cap = "both"), y = guide_axis(cap = "both"))
)

