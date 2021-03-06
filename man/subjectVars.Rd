% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subjectVars.R
\name{subjectVars}
\alias{subjectVars}
\title{run the source for each task in the list and merge the data}
\usage{
subjectVars(tasks, list = bySubject)
}
\arguments{
\item{tasks}{a character vector containing the names of the tasks to be extracted}

\item{list}{is a list of subjects' data}
}
\value{
a data frame of summarised data from all the tasks listed with one participant per row
}
\description{
run the source for each task in the list and merge the data
}
\examples{
subjectVars(c("fp", "learning", "bonus"), bySubject)
}

