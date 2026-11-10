\name{run_rgent}
\alias{run_rgent}
\title{Open the Rgent chat pane}
\usage{
run_rgent()
}
\description{
Starts Rgent's local websocket server and opens the chat pane in the RStudio
Viewer. Calling it again restarts the server and reloads the pane. Rgent also
starts by itself when RStudio starts (see \code{\link{setup_rprofile_auto_start}}).
}
\details{
The pane needs an access code from \url{https://rgentai.com}. The code is saved on
your computer, so you only enter it once.
}
\value{\code{TRUE} invisibly when the pane opened, \code{FALSE} when it couldn't (for example outside RStudio).}
\examples{
\dontrun{
run_rgent()
}
}
