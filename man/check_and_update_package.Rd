\name{check_and_update_package}
\alias{check_and_update_package}
\title{Check for a newer version of Rgent}
\usage{
check_and_update_package(auto_update = FALSE, quiet = FALSE)
}
\arguments{
\item{auto_update}{If \code{TRUE}, install the newer version when there is one.
If \code{FALSE}, only report it.}
\item{quiet}{If \code{TRUE}, print nothing unless an update is found.}
}
\description{
Compares the installed version with the latest release on GitHub. Rgent already
checks once a day in the background, so this is only needed to check by hand.
After an update, restart R to use the new version.
}
\value{\code{TRUE} invisibly when a newer version was installed, \code{FALSE} otherwise.}
\examples{
\dontrun{
check_and_update_package()
}
}
