% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_receivers.R
\name{get_receivers}
\alias{get_receivers}
\title{Get receiver data}
\usage{
get_receivers(
  connection = con,
  receiver_id = NULL,
  application_type = NULL,
  status = NULL
)
}
\arguments{
\item{connection}{A connection to the ETN database. Defaults to \code{con}.}

\item{receiver_id}{Character (vector). One or more receiver ids.}

\item{application_type}{Character. \code{acoustic_telemetry} or \code{cpod}.}

\item{status}{Character. One or more statuses, e.g. \code{Available} or \code{Broken}.}
}
\value{
A tibble with receivers data, sorted by \code{receiver_id}. See also
\href{https://inbo.github.io/etn/articles/etn_fields.html}{field definitions}.
}
\description{
Get data for receivers, with options to filter results.
}
\examples{
\dontrun{
con <- connect_to_etn(your_username, your_password)

# Get all receivers
get_receivers(con)

# Get specific receivers
get_receivers(con, receiver_id = "VR2-4842c")
get_receivers(con, receiver_id = c("VR2AR-545719", "VR2AR-545720"))

# Get receivers for a specific application type
get_receivers(con, application_type = "cpod")

# Get receivers with a specific status
get_receivers(con, status = c("Broken", "Lost"))
}
}
