#' create RA risk group grid
#'
#' Running \code{rg_grid()} returns a \code{data.table} of all 1560 risk groups featured in current RA.
#'
#' @param children Logical with default \code{FALSE}. If \code{TRUE} age group \code{"0-18"} is added to the grid.
#' @param kantonal Logical with default \code{TRUE} where Swiss cantons are added to the grid. \code{FALSE} returns risk group grid without cantons.
#' @importFrom magrittr "%>%"
#' @export
#' @return A \code{data.table} containing all combinations of
#'  \item{Kanton}{26 swiss cantons}
#'  \item{Altersklasse}{15 age groups (16 when \code{children = T})}
#'  \item{Geschlecht}{M/F}
#'  \item{Aufenthalt}{J/N dummy variable indicating whether person was hospitalized for at least 3 consecutive nights}
#' @author Magnus Vieten, Yannick Schwarz
#' @examples rg_grid()
#' @seealso \href{https://www.fedlex.admin.ch/eli/cc/2016/674/de#art_1}{Art. 1 & 2 VORA}
rg_grid <- function(children = FALSE,
                    kantonal = TRUE) {
  Kanton <- Altersklasse <- Geschlecht <- Aufenthalt <- NULL # no visible binding NOTE in R CMD check
  canton <- c(
    "LU", "SH", "SZ", "OW", "GR", "NE", "BL", "UR", "GE", "NW", "ZG", "ZH", "BE",
    "VS", "FR", "TI", "VD", "JU", "AG", "BS", "AI", "TG", "GL", "AR", "SO", "SG"
  )
  age <- if (children == TRUE) {
    c(
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
  } else if (children == FALSE) {
    c(
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
  sex <- c("M", "F")
  spitalflag <- c("J", "N")
  if (kantonal == TRUE) {
    all_rg <- expand.grid(list(canton, age, sex, spitalflag), stringsAsFactors = F) %>%
      dplyr::select("Kanton" = 1, "Altersklasse" = 2, "Geschlecht" = 3, "Aufenthalt" = 4) %>%
      dplyr::arrange(Kanton, Altersklasse, Geschlecht, Aufenthalt) %>%
      data.table::as.data.table()
  } else if (kantonal == FALSE) {
    all_rg <- expand.grid(list(age, sex, spitalflag), stringsAsFactors = F) %>%
      dplyr::select("Altersklasse" = 1, "Geschlecht" = 2, "Aufenthalt" = 3) %>%
      dplyr::arrange(Altersklasse, Geschlecht, Aufenthalt) %>%
      data.table::as.data.table()
  }
  return(all_rg)
}
