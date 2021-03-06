% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exam_run_audit.R
\name{exam_run_audit}
\alias{exam_run_audit}
\title{Run audit}
\usage{
exam_run_audit(df, mark_col, width = 1, boundary = c(50, 65, 75, 85))
}
\arguments{
\item{df}{data frame of marks}

\item{mark_col}{colname of column with final marks}

\item{width}{width of band to filter on}

\item{boundary}{vector of boundary points.}
}
\value{
nothing - just prints to screen cases to check.
}
\description{
Run audit
}
\examples{
data(SMI_2018_marks_clean)
SMI_2018_marks_clean
run_audit(SMI_2018_marks_clean, Total)
}
