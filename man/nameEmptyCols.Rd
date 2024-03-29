% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nameEmptyCols.R
\name{nameEmptyCols}
\alias{nameEmptyCols}
\title{Get the dataframe names of columns which are all NA}
\usage{
nameEmptyCols(df)
}
\arguments{
\item{df}{A dataframe to check. Attempts to coerce to df.}
}
\value{
Character vector of dataframe names
}
\description{
Returns names of dataframe columns that are all NA/NULL.
}
\examples{
df <- iris
df$Sepal.Width <- NA
nameEmptyCols(df)
df$Petal.Width <- NA
empty <- nameEmptyCols(df)
empty
df[!names(df) \%in\% nameEmptyCols(df)]

}
