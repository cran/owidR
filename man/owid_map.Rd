% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/owid_map.R
\name{owid_map}
\alias{owid_map}
\title{Create a choropleth world map using data from Our World in Data.}
\usage{
owid_map(
  data = data.frame(),
  col = 4,
  palette = "Reds",
  mode = "plot",
  year = NULL
)
}
\arguments{
\item{data}{A dataframe returned by owid(). This dataframe must have country names in the entity column, not all data returned by owid() will be like this.}

\item{col}{Either the column number to be treated as the value or a character string specifying the name of the column. Defaults to 3, which is the first possible value column.}

\item{palette}{The RColorBrewer palette to be used.}

\item{mode}{If "plot", the output will be a ggplot2 map. If "view", the output will be a leaflet interactive map.}

\item{year}{The year to be mapped. Defaults to NULL, which plots the most recent year with data available.}
}
\value{
Either a ggplot2 map (for mode = "plot") or a leaflet map (for mode = "view").
}
\description{
A function to easily create a choropleth world map using data from Our World in Data.
}
\examples{
\donttest{
mental <- owid("share-with-mental-and-substance-disorders")

# simple ggplot2 map
owid_map(mental)

# interavtive map with blue palette
owid_map(mental, mode = "view", palette = "Blues")
}
}
