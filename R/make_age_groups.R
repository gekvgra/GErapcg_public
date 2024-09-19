#' Make age groups
#'
#' Make age groups in accordance to Art. 2 VORA from 19.10.2016 (Release 1.1.2022).
#'
#' @param DT A \code{data.table} containing variables "Jahr" and "Geburtsjahr" (both of type integer).
#' @param style Designate output format. \code{"normal"} gives type "36", \code{"sora"} gives type "36-40".
#' @export
#' @return Adds variable "Altersklasse" to input \code{data.table}
#' @author Yannick Schwarz
#' @importFrom data.table ":="
#' @seealso \href{https://www.fedlex.admin.ch/eli/cc/2016/674/de}{VORA}
#' @examples
#' make_age_groups(data.table::data.table(Jahr = 2021, Geburtsjahr = 1962))
#'
make_age_groups <- function(DT, style = "normal") {
  Alter <- Jahr <- Geburtsjahr <- Altersklasse <- NULL # no visible binding NOTE in R CMD check
  if (!data.table::is.data.table(DT)) {
    stop("DT is not a data.table")
  }
  DT[
    ,
    Alter := Jahr - Geburtsjahr
  ][
    ,
    Altersklasse := cut(Alter,
      breaks = c(
        -0.1,
        18.9,
        25.9,
        30.9,
        35.9,
        40.9,
        45.9,
        50.9,
        55.9,
        60.9,
        65.9,
        70.9,
        75.9,
        80.9,
        85.9,
        90.9,
        300
      ),
      if (style == "normal") {
        labels <- c(
          0,
          19,
          26,
          31,
          36,
          41,
          46,
          51,
          56,
          61,
          66,
          71,
          76,
          81,
          86,
          91
        )
      } else if (style == "sora") {
        labels <- c(
          "0-18",
          "19-25",
          "26-30",
          "31-35",
          "36-40",
          "41-45",
          "46-50",
          "51-55",
          "56-60",
          "61-65",
          "66-70",
          "71-75",
          "76-80",
          "81-85",
          "86-90",
          "91+"
        )
      }
    )
  ][
    ,
    `:=`(
      Alter = NULL,
      Altersklasse = as.character(Altersklasse)
    )
  ]
}
