#' aufenthalt_fun
#'
#' Shifts hospitalization status \code{Aufenthalt: J/N} of an \code{AhvNr} from a data set to another. If AhvNr isn't contained in source data, it assigns \code{N} in target. Used in RA calculation to create variable \code{Aufenthalt_Vj} ('previous year's hospitalization status').
#'
#' @param dt_T Target of the shift of hospitalzation status. A data.table featuring individual insurant data according to naming and data type standards used in data set \code{test_data}.
#' @param dt_T_1 Source of the shift of hospitalization status. A data.table featuring individual insurant data according to naming and data type standards used in data set \code{test_data}.
#' @importFrom data.table ":="
#' @importFrom data.table ".N"
#' @export
#' @return A data.table featuring entries of \code{dt_T} and variable \code{Aufenthalt_Vj}
#'  \item{Aufentalt_Vj}{hospitalization status of \code{dt_T_1} assigned to \code{dt_T} by \code{AhvNr}}
#' @author Magnus Vieten, Yannick Schwarz
#' @seealso \url{https://www.kvg.org/api/rm/S443P62E88YU756/200420-kuv-va-berechnungsformeln-fuer-den-risikoau.pdf}
#' @examples \dontrun{
#' aufenthalt_fun(
#' test_data[Jahr == 2019 & MonateHorizont == 14],
#' test_data[Jahr == 2018 & MonateHorizont == 26])
#' }
aufenthalt_fun <- function(dt_T, dt_T_1) {
  AhvNr <- Geburtsjahr <- Aufenthalt <- N_Aufenthalt <- Aufenthalt_Vj <- NULL # no visible binding NOTE in R CMD check
  # Prepare dt_T_1 for joining
  dt_T_1_prep <- unique(
    unique(dt_T_1[
      ,
      .(AhvNr, Geburtsjahr, Aufenthalt)
    ])[
      ,
      N_Aufenthalt := .N,
      by = .(AhvNr, Geburtsjahr)
    ][
      N_Aufenthalt > 1,
      Aufenthalt := "J"
    ][
      ,
      -c("N_Aufenthalt")
    ]
  )
  data.table::setnames(dt_T_1_prep, old = "Aufenthalt", new = "Aufenthalt_Vj")

  # Join
  dt_T_Aufenthalt <- dt_T_1_prep[
    dt_T,
    on = .(AhvNr, Geburtsjahr)
  ][
    ,
    Aufenthalt_Vj := ifelse(is.na(Aufenthalt_Vj), "N", Aufenthalt_Vj)
  ][
    , Aufenthalt := NULL
  ]
  return(dt_T_Aufenthalt)
}
