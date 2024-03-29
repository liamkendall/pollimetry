% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wingloading.R
\name{wingloading}
\alias{wingloading}
\title{Converts intertegular distance (mm) to wing loading (mg mm2).}
\usage{
wingloading(IT)
}
\arguments{
\item{IT}{vector of intertegular distance measurements (mm).}
}
\value{
A dataframe with wing loading (mg mm2) is returned for each specimen.
}
\description{
Calculates wing loading (mg mm2) expressed as dry weight per total wing area using intertegular distance (mm) from Bullock (1999).
}
\examples{
wingloading(IT=c(10,5,2))
}
\references{
\itemize{
\item Kendall et al. (2019) Pollinator size and its consequences: Robust estimates of body size in pollinating insects. Ecology and Evolution, 9(4), 1702-1714. \doi{10.1002/ece3.4835}.
\item Bullock, S. H. (1999). Relationships among body size, wing size and mass in bees from a tropical dry forest in Mexico. Journal of the Kansas Entomological Society, 426-439.
}
}
