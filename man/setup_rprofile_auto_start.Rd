\name{setup_rprofile_auto_start}
\alias{setup_rprofile_auto_start}
\alias{auto_start_rgent}
\title{Start Rgent when RStudio starts}
\usage{
setup_rprofile_auto_start()

auto_start_rgent()
}
\description{
\code{setup_rprofile_auto_start()} adds a few lines to your \file{~/.Rprofile} that
load the package and open the chat pane once RStudio is ready. The package calls it
when it is loaded, so you don't need to run it yourself; it does nothing when the
lines are already there.

\code{auto_start_rgent()} is what those lines call. It opens the pane at most once
per R session.
}
\value{
\code{setup_rprofile_auto_start()} returns \code{TRUE} invisibly when the lines are
in place, \code{FALSE} when \file{.Rprofile} couldn't be written. \code{auto_start_rgent()} returns \code{TRUE} once the pane has been
started (or was already), so callbacks that call it can remove themselves.
}
\examples{
\dontrun{
setup_rprofile_auto_start()
}
}
