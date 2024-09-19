#' Versichererwechsler
#'
#' Returns status of insurance change \code{VW} for an \code{AhvNr} between two RA data sets. See determination rules in \href{https://www.kvg.org/de/datenerhebung-_content---1--1053.html}{Benutzeranleitung SORA PCG, Kap. 10}.
#'
#' @param T_1_26 A \code{data.table} featuring RA individual data according to naming and data type standards used in RA. 1st component of comparison ('previous year')
#' @param T_14 A \code{data.table} featuring RA individual data according to naming and data type standards used in RA. 2nd component of comparison.
#' @importFrom data.table ":="
#' @export
#' @return A \code{data.table} containing
#'  \item{BagNr}{Integer with the BAG-Nr.}
#'  \item{AhvNr}{Pseudo SSN}
#'  \item{Geburtsjahr}{Year of birthday - used with \code{AhvNr} to warrant identification}
#'  \item{VW}{J/N dummy variable indicating the status of insurance change}
#' @author Magnus Vieten, Yannick Schwarz
#' @examples \dontrun{
#' vw_fun(T_1_26_auf, T_14_auf)
#' }
vw_fun <- function(T_1_26, T_14) {
  # no visible binding NOTE in R CMD check
  Kanton <- Monate_all_KK_T_1 <- Monate_KK_T_1 <- i.Monate_KK_T_1 <- NULL
  i.Monate_all_KK_T_1 <- Monate <- BagNr <- AhvNr <- Geburtsjahr <- NULL

  T_1_26_vw <- unique(
    T_1_26[
      Monate != 0,
      .(BagNr, AhvNr, Geburtsjahr, Kanton, Monate)
    ]
  )[
    ,
    .(Monate_KK_T_1 = sum(Monate)),
    by = .(BagNr, AhvNr, Geburtsjahr)
  ][
    ,
    Monate_all_KK_T_1 := sum(Monate_KK_T_1),
    by = .(AhvNr, Geburtsjahr)
  ]

  T_14_vw <- unique(
    T_14[
      ,
      .(BagNr, AhvNr, Geburtsjahr)
    ]
  )[
    T_1_26_vw,
    on = .(BagNr, AhvNr, Geburtsjahr),
    Monate_KK_T_1 := i.Monate_KK_T_1
  ][
    T_1_26_vw,
    on = .(AhvNr, Geburtsjahr),
    Monate_all_KK_T_1 := i.Monate_all_KK_T_1
  ][
    ,
    `:=`(
      VW = ifelse(
        # AhvNr did not exist in T-1, 26M
        is.na(Monate_all_KK_T_1) |
          # AhvNr did exist in T-1, 26M and all month are in one BagNr
          (!is.na(Monate_KK_T_1) & Monate_KK_T_1 == Monate_all_KK_T_1),
        0,
        1
      ),
      Monate_KK_T_1 = NULL,
      Monate_all_KK_T_1 = NULL
    )
  ]

  # Return
  T_14_vw
}
