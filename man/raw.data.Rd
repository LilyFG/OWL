% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/raw.data.R
\name{raw.data}
\alias{raw.data}
\title{create a data frame containing all the raw data for the participants}
\usage{
raw.data(data, task)
}
\arguments{
\item{data}{is the list of subject data (defaults to bySubject)}

\item{task}{is the task of interest}
}
\value{
a data frame containing raw data for all participants who have completed the task
}
\description{
create a data frame containing all the raw data for the participants
}
\examples{
raw.data(bySubject, "fp")
}

