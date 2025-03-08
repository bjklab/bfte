% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bfte.R
\name{scale_x_bfte}
\alias{scale_x_bfte}
\title{BFTE scale for continuous x-axis}
\usage{
scale_x_bfte(axis_data)
}
\arguments{
\item{axis_data}{variable depicted on the x-axis}
}
\value{
scale for x-axis added to a `ggplot2` object
}
\description{
Adds an appropriate range for the `ggplot2` object's x-axis to match BFTE theming.
}
\details{
The source data is used to inform scale range and breaks.
}
\examples{
ready_fonts()

ggplot2::ggplot(data = data.frame(x = c(0,10,17), y = c(79,11,18)),
 ggplot2::aes(x = x, y = y)
 ) +
 ggplot2::geom_point() +
 scale_x_bfte(axis_data = c(0,10,17)) +
 theme_bfte_majormono()
}
\seealso{
\code{\link{ready_fonts}}, \code{\link{theme_bfte_roboto}}, \code{\link{theme_bfte_nothing}}
}
