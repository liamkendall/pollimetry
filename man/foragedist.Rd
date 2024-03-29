% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/foragedist.R
\name{foragedist}
\alias{foragedist}
\title{(Deprecated) Converts body weight to measures of foraging range for bees.}
\usage{
foragedist(x, type = "GreenleafAll")
}
\arguments{
\item{x}{A vector of either bee intertegular spans (IT) measurements in mm, head width values in mm or dry weight values (mg).}

\item{type}{The type of foraging range desired. Options are "GreenleafAll", GrMhd ("Maximum homing distance"),
GrThd ("Typical homing distance"), GrMfd ("Maximum feeder training distance"), GrMcd("Maximum communication distance"),"Osmia", "MeliMR" , "MeliFT" or "MeliAll". See details in Greenleaf et al. (2007), Guedot et al. (2009) and van Nieuwstadt and Iraheta (1996).}
}
\value{
A dataframe with bee and foraging range (km) is returned for each bee species.
}
\description{
`r lifecycle::badge("deprecated")` This function is now deprecated and has been superceded by `foraging.range`

Calculates foraging range from Greenleaf et al. (2007) using intertegular distance (ITD) values,
 van Nieuwstadt and Iraheta (1996) using head width (HW) values for Meliponini (stingless bees) and 
 Guedot et al. (2009) using dry weight (mg) values for Osmia species.
}
\examples{
foragedist(c(10,5,2), type = "MeliMR")

}
\references{
\itemize{
\item Kendall et al. (2019) Pollinator size and its consequences: Robust estimates of body size in pollinating insects. \emph{Ecology and Evolution}, 9(4), 1702-1714. \doi{10.1002/ece3.4835}.
\item Greenleaf et al. (2007) Bee foraging ranges and their relationship to body size. \emph{Oecologia}, 153, 589-596. \doi{10.1007/s00442-007-0752-9}.
\item Guedot et al. (2009). Relationship between body size and homing ability in the genus Osmia (Hymenoptera; Megachilidae). \emph{Ecological Entomology}, 34(1), 158-161. \doi{10.1111/j.1365-2311.2008.01054.x}.
\item van Nieuwstadt, M. G. L., & Iraheta, C. R. (1996). Relation between size and foraging range in stingless bees (Apidae, Meliponinae). \emph{Apidologie}, 27(4), 219-228.
}
}
