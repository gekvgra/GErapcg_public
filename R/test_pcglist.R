#' PCG List 2022
#'
#' A dataset with the valid PCG as publicly provided by the BAG. Note that this
#'   dataset is missing several columns required by the parallel computation script.
#'   Hence the data should be processed using the function \code{GErapcg::pcg_import}
#'
#' @format A \code{data.table} with 7 variables and 5'972 rows.
#' \describe{
#'  \item{PCG-Kurzname}{Character with shortnames of PCG }
#'  \item{PCG-Name}{Character with fullnames of PCG}
#'  \item{ATC-Code}{Character with ATC Codes}
#'  \item{GTIN}{Numeric with 13 digits.}
#'  \item{Pharmacode}{Numeric with 5 to 7 digits.}
#'  \item{Arzneimittel}{Character with description of drug}
#'  \item{Anzahl.DDD.pro.Packungen}{Integer with daily designated doses (DDD) of drug per package}
#' }
#' @source https://www.bag.admin.ch/bag/de/home/versicherungen/krankenversicherung/krankenversicherung-versicherer-aufsicht/risikoausgleich.html
#' @examples
#' data("test_pcglist")
"test_pcglist"
