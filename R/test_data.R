#' Test Data for Risk Adjustment
#'
#' A dataset containing randomly generated data with 1000 rows for each canton
#'   for three insurers.
#' @note The data provided in the package is randomly generated with 1000 rows for
#'   each canton for three insurers. The randomly generated data serves the
#'   user for testing purposes but not for generating final invoices, according
#'   to SORA PCG. The output objects from this randomly generated data are
#'   thus clearly not the same as the output objects from SORA PCG. However when
#'   the GE KVG is using the original data provided by the insurers within
#'   this package the results are the same as in the output objects of SORA PCG.
#'   For a third party there is no possibility to reproduce these results
#'   because there is by law no way to get the original data provided by the
#'   insurers. The original data is accessible by law only to the GE KVG and the
#'   BAG (see VORA Art. 26).
#'
#' @format A \code{data.table} with 15 variables and 295'023 rows.
#' \describe{
#'  \item{BagNr}{Integer with the BAG-Nr.}
#'  \item{Jahr}{Integer with the year of the data.}
#'  \item{MonateHorizont}{Integer with the months since the first day of \code{Jahr}.}
#'  \item{AhvNr}{Numeric with 13 digits representing the AHV-Nr. of each individual. The first three digits are 756. The next digit represents a special test case if 0 or a randomly generated individual if 9.}
#'  \item{Kanton}{Character with the canton of the individual.}
#'  \item{Geburtsjahr}{Numeric with the year of birth.}
#'  \item{Geschlecht}{Character with the sex.}
#'  \item{Gtin}{Numeric with 13 digits.}
#'  \item{Pharmacode}{Numeric with 5 to 7 digits.}
#'  \item{Packungen}{Numeric with max. 2 decimals (rounded).}
#'  \item{Monate}{Numeric with max. 2 decimals (rounded).}
#'  \item{Kosten}{Numeric with max. 2 decimals (rounded).}
#'  \item{Kobe}{Numeric with max. 2 decimals (rounded).}
#'  \item{LinieNr}{Numeric with the line number of the uploaded csv-file.}
#' }
#' @source See \code{Testset_Generator.git}
#' @examples
#' data("test_data")
"test_data"
