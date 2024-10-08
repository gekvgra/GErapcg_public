% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcg_import.R
\name{pcg_import}
\alias{pcg_import}
\title{Import PCG list}
\usage{
pcg_import(
  input,
  export_path = NULL,
  pcg_kurzname_col = 1,
  gtin_col = 4,
  pharmacode_col = 5,
  pharmacode_ddd_col = 7,
  schwellenwert_ddd_col = 8,
  schwellenwert_packungen_col = 9
)
}
\arguments{
\item{input}{\code{data.table} or \code{data.frame} with the PCG-list or
character vector with the path to an excel file.}

\item{export_path}{Character vector with a path where a excel file as
output should be generated.}

\item{pcg_kurzname_col, gtin_col, pharmacode_col, pharmacode_ddd_col, schwellenwert_ddd_col, schwellenwert_packungen_col}{Integer vector with the column number of the PCG-Kurzname, GTIN, Pharmacode,
Anzahl DDD pro Packung, DDD-Schwellenwert, Packungen-Schwellenwert.}
}
\value{
A \code{data.frame} with the modified PCG list to be used for upload in
SORA PCG and PB calculation.
}
\description{
Imports and modifies PCG list provided by BAG to be used in RA calculation.
}
\examples{
\dontrun{
pcg_import(test_pcglist)
pcg_import("path/to/pcg_list.xlsx")
}
}
\author{
Magnus Vieten, Yannick Schwarz
}
