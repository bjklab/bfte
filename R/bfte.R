#' @title Ready fonts for bfte theme options
#' @description Loads google fonts via the `showtext`` package.
#' @details Three fonts are loaded by default: *Roboto*, *Nothing You Could Do*, and *Major Mono Display*.
#' @import showtext
#' @import sysfonts
#' @import purrr
#' @import jsonlite
#' @param google_font_name_vector vector of Google font names
#' @param font_ref_vector vector of abbreviated font names
#' @return fonts loaded into namespace
#' @seealso \code{\link{theme_bfte_dx_roboto}}, \code{\link{theme_bfte_dx_nothing}}, \code{\link{theme_bfte_dx_majormono}}
#' @export
#' @examples
#' ready_fonts()
ready_fonts <- function(google_font_name_vector = c("Roboto", "Nothing You Could Do", "Major Mono Display"), font_ref_vector = c("roboto", "nothing", "majormono")) {
  purrr::map2(.x = google_font_name_vector,
              .y = font_ref_vector,
              .f = ~ sysfonts::font_add_google(.x, .y)
  )
  showtext::showtext_auto()
  showtext::showtext_opts(dpi = 600)
}


#' @title BFTE theme with *Roboto* font
#' @description Adds a themed appearance and guide to a `ggplot2` object with a **continuous** x-axis variable.
#' @details The *Roboto* font is used in the theme.
#' @import ggplot2
#' @import ggtext
#' @import showtext
#' @return theme added to a `ggplot2` object
#' @seealso \code{\link{ready_fonts}}, \code{\link{theme_bfte_nothing}}, \code{\link{theme_bfte_majormono}}
#' @export
#' @examples
#' ggplot2::ggplot(data = data.frame(x = c(0,10,17), y = c(79,11,18)),
#'  ggplot2::aes(x = x, y = y)
#'  ) +
#'  ggplot2::geom_point() +
#'  theme_bfte_roboto()
theme_bfte_roboto <- function() {
  return(
    list(
      ggplot2::theme_classic(base_family = "roboto", base_size = 16) +
      ggplot2::theme(panel.grid.major = ggplot2::element_line(linetype = 3, color = "grey", linewidth = 0.5),
            legend.position = "none",
            axis.text.x = ggtext::element_markdown(color = "black"),
            axis.text.y = ggtext::element_markdown(color = "black"),
            axis.title.x = ggtext::element_markdown(color = "black"),
            axis.title.y = ggtext::element_markdown(color = "black"),
            ),
      ggplot2::guides(x = ggplot2::guide_axis(cap = "both"), y = ggplot2::guide_axis(cap = "both"))
    )
  )
}

#' @title BFTE theme with *Nothing You Could Do* font
#' @description Adds a themed appearance and guide to a `ggplot2` object with a **continuous** x-axis variable.
#' @details The *Nothing You Could Do* font is used in the theme.
#' @import ggplot2
#' @import ggtext
#' @import showtext
#' @return theme added to a `ggplot2` object
#' @seealso \code{\link{ready_fonts}}, \code{\link{theme_bfte_roboto}}, \code{\link{theme_bfte_majormono}}
#' @export
#' @examples
#' ggplot2::ggplot(data = data.frame(x = c(0,10,17), y = c(79,11,18)),
#'  ggplot2::aes(x = x, y = y)
#'  ) +
#'  ggplot2::geom_point() +
#'  theme_bfte_nothing()
theme_bfte_nothing <- function() {
  return(
    list(
      ggplot2::theme_classic(base_family = "nothing", base_size = 16) +
      ggplot2::theme(panel.grid.major = ggplot2::element_line(linetype = 3, color = "grey", linewidth = 0.5),
            legend.position = "none",
            axis.text.x = ggtext::element_markdown(color = "black"),
            axis.text.y = ggtext::element_markdown(color = "black"),
            axis.title.x = ggtext::element_markdown(color = "black"),
            axis.title.y = ggtext::element_markdown(color = "black")
            ),
      ggplot2::guides(x = ggplot2::guide_axis(cap = "both"), y = ggplot2::guide_axis(cap = "both"))
    )
  )
}

#' @title BFTE theme with *Major Mono Display* font
#' @description Adds a themed appearance and guide to a `ggplot2` object with a **continuous** x-axis variable.
#' @details The *Major Mono Display* font is used in the theme.
#' @import ggplot2
#' @import ggtext
#' @import showtext
#' @return theme added to a `ggplot2` object
#' @seealso \code{\link{ready_fonts}}, \code{\link{theme_bfte_roboto}}, \code{\link{theme_bfte_nothing}}
#' @export
#' @examples
#' ggplot2::ggplot(data = data.frame(x = c(0,10,17), y = c(79,11,18)),
#'  ggplot2::aes(x = x, y = y)
#'  ) +
#'  ggplot2::geom_point() +
#'  theme_bfte_majormono()
theme_bfte_majormono <- function() {
  return(
    list(
      ggplot2::theme_classic(base_family = "majormono", base_size = 16) +
      ggplot2::theme(panel.grid.major = ggplot2::element_line(linetype = 3, color = "grey", linewidth = 0.5),
            legend.position = "none",
            axis.text.x = ggtext::element_markdown(color = "black"),
            axis.text.y = ggtext::element_markdown(color = "black"),
            axis.title.x = ggtext::element_markdown(color = "black"),
            axis.title.y = ggtext::element_markdown(color = "black")
            ),
      ggplot2::guides(x = ggplot2::guide_axis(cap = "both"), y = ggplot2::guide_axis(cap = "both"))
    )
  )
}

#' @title BFTE theme with *Roboto* font
#' @description Adds a themed appearance and guide to a `ggplot2` object with a **discrete** x-axis variable.
#' @details The *Roboto* font is used in the theme.
#' @import ggplot2
#' @import ggtext
#' @import showtext
#' @return theme added to a `ggplot2` object
#' @seealso \code{\link{ready_fonts}}, \code{\link{theme_bfte_dx_nothing}}, \code{\link{theme_bfte_dx_majormono}}
#' @export
#' @examples
#' ggplot2::ggplot(data = data.frame(x = c("zero","ten","seventeen"), y = c(79,11,18)),
#'  ggplot2::aes(x = x, y = y)
#'  ) +
#'  ggplot2::geom_point() +
#'  theme_bfte_dx_roboto()
theme_bfte_dx_roboto <- function() {
  return(
    list(
      ggplot2::theme_classic(base_family = "roboto", base_size = 16) +
      ggplot2::theme(panel.grid.major = ggplot2::element_line(linetype = 3, color = "grey", linewidth = 0.5),
            legend.position = "none",
            axis.text.x = ggtext::element_markdown(angle = 90, vjust = 0.5, hjust = 1, color = "black"),
            axis.text.y = ggtext::element_markdown(color = "black"),
            axis.title.x = ggtext::element_markdown(color = "black"),
            axis.title.y = ggtext::element_markdown(color = "black")
            ),
      ggplot2::guides(x = ggplot2::guide_axis(cap = "both"), y = ggplot2::guide_axis(cap = "both"))
    )
  )
}

#' @title BFTE theme with *Nothing You Could Do* font
#' @description Adds a themed appearance and guide to a `ggplot2` object with a **discrete** x-axis variable.
#' @details The *Nothing You Could Do* font is used in the theme.
#' @import ggplot2
#' @import ggtext
#' @import showtext
#' @return theme added to a `ggplot2` object
#' @seealso \code{\link{ready_fonts}}, \code{\link{theme_bfte_dx_roboto}}, \code{\link{theme_bfte_dx_majormono}}
#' @export
#' @examples
#' ggplot2::ggplot(data = data.frame(x = c("zero","ten","seventeen"), y = c(79,11,18)),
#'  ggplot2::aes(x = x, y = y)
#'  ) +
#'  ggplot2::geom_point() +
#'  theme_bfte_dx_nothing()
theme_bfte_dx_nothing <- function() {
  return(
    list(
      ggplot2::theme_classic(base_family = "nothing", base_size = 16) +
      ggplot2::theme(panel.grid.major = ggplot2::element_line(linetype = 3, color = "grey", linewidth = 0.5),
            legend.position = "none",
            axis.text.x = ggtext::element_markdown(angle = 90, vjust = 0.5, hjust = 1, color = "black"),
            axis.text.y = ggtext::element_markdown(color = "black"),
            axis.title.x = ggtext::element_markdown(color = "black"),
            axis.title.y = ggtext::element_markdown(color = "black")
            ),
      ggplot2::guides(x = ggplot2::guide_axis(cap = "both"), y = ggplot2::guide_axis(cap = "both"))
    )
  )
}

#' @title BFTE theme with *Major Mono Display* font
#' @description Adds a themed appearance and guide to a `ggplot2` object with a **discrete** x-axis variable.
#' @details The *Major Mono Display* font is used in the theme.
#' @import ggplot2
#' @import ggtext
#' @import showtext
#' @return theme added to a `ggplot2` object
#' @seealso \code{\link{ready_fonts}}, \code{\link{theme_bfte_dx_roboto}}, \code{\link{theme_bfte_dx_nothing}}
#' @export
#' @examples
#' ggplot2::ggplot(data = data.frame(x = c("zero","ten","seventeen"), y = c(79,11,18)),
#'  ggplot2::aes(x = x, y = y)
#'  ) +
#'  ggplot2::geom_point() +
#'  theme_bfte_dx_majormono()
theme_bfte_dx_majormono <- function() {
  return(
    list(
      ggplot2::theme_classic(base_family = "majormono", base_size = 16) +
      ggplot2::theme(panel.grid.major = ggplot2::element_line(linetype = 3, color = "grey", linewidth = 0.5),
            legend.position = "none",
            axis.text.x = ggtext::element_markdown(angle = 90, vjust = 0.5, hjust = 1, color = "black"),
            axis.text.y = ggtext::element_markdown(color = "black"),
            axis.title.x = ggtext::element_markdown(color = "black"),
            axis.title.y = ggtext::element_markdown(color = "black")
            ),
      ggplot2::guides(x = ggplot2::guide_axis(cap = "both"), y = ggplot2::guide_axis(cap = "both"))
    )
  )
}

