\name{dbReconnect,GiottoDB-method}
\alias{dbReconnect,GiottoDB-method}
\title{Reconnect a GiottoDB object}
\usage{
\S4method{dbReconnect}{GiottoDB}(x)
}
\arguments{
\item{x}{A \code{\link{GiottoDB}} object.}
}
\value{
A \code{\link{GiottoDB}} object with a valid top-level database connection.
}
\description{
Reconnects a stale \code{GiottoDB} object by reusing the canonical connection
from one persistent database-backed child object.
}
