% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/suso_gql_maps.R
\name{suso_gql_maps}
\alias{suso_gql_maps}
\title{Get all maps}
\usage{
suso_gql_maps(
  endpoint = NULL,
  workspace = NULL,
  user = NULL,
  password = NULL,
  fileName = NULL,
  importDateUtc = NULL,
  size = NULL,
  users = NULL,
  sortby_filename = NULL,
  sortby_importeddateutc = NULL,
  sortby_size = NULL,
  take = NULL,
  skip = NULL
)
}
\arguments{
\item{endpoint}{GraphQL endpoint of your server}

\item{workspace}{Server Workspace, if NULL uses default}

\item{user}{your API username}

\item{password}{API password}

\item{fileName}{name of the map on the server}

\item{importDateUtc}{Import date}

\item{size}{Size of the map}

\item{users}{Users to whom the maps are assigned}

\item{sortby_filename}{sort maps by file name, either ASC for ascending or DESC for descending}

\item{sortby_importeddateutc}{sort maps by import date in utc, either ASC for ascending or DESC for descending}

\item{sortby_size}{sort by map size, either ASC for ascending or DESC for descending}

\item{take}{take the specified integer numeber of maps}

\item{skip}{skip the first integer number of maps}
}
\description{
Allows the user to retrieve filtered or unfiltered map data.
}
