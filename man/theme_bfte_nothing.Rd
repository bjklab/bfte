% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bfte.R
\name{theme_bfte_nothing}
\alias{theme_bfte_nothing}
\title{BFTE theme with *Nothing You Could Do* font}
\usage{
theme_bfte_nothing()
}
\value{
theme added to a `ggplot2` object
}
\description{
Adds a themed appearance and guide to a `ggplot2` object with a **continuous** x-axis variable.
}
\details{
The *Nothing You Could Do* font is used in the theme.
}
\examples{
ready_fonts()

ggplot2::ggplot(data = data.frame(x = c(0,10,17), y = c(79,11,18)),
 ggplot2::aes(x = x, y = y)
 ) +
 ggplot2::geom_point() +
 theme_bfte_nothing()
}
\seealso{
\code{\link{ready_fonts}}, \code{\link{theme_bfte_roboto}}, \code{\link{theme_bfte_majormono}}
}
