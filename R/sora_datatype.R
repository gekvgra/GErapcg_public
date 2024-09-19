#' Make the same datatypes as in SORA PCG
#'
#' @param data A \code{data.table} object.
#' @param Ahv_Char Logical if \code{TRUE} AhvNr should be a character vector or if \code{FALSE} a numeric vector.
#' @export
#' @importFrom data.table ":="
#'
sora_datatype <-
  function(data, Ahv_Char = TRUE) {
  BagNr <- Jahr <- Kanton <- Geburtsjahr <- Geschlecht <- Aufenthalt <- Gtin <- PharmaCode <- NULL
  Packungen <- Monate <- Kosten <- Kobe <- MonateHorizont <- AhvNr <- NULL # no visible binding NOTE in R CMD check
    data[, `:=`(
      BagNr = as.integer(BagNr),
      Jahr = as.integer(Jahr),
      Kanton = as.character(Kanton),
      Geburtsjahr = as.numeric(Geburtsjahr),
      Geschlecht = as.character(Geschlecht),
      Aufenthalt = as.character(Aufenthalt),
      Gtin = as.numeric(Gtin),
      PharmaCode = as.numeric(PharmaCode),
      Packungen = as.numeric(Packungen),
      Monate = as.numeric(Monate),
      Kosten = as.numeric(Kosten),
      Kobe = as.numeric(Kobe),
      MonateHorizont = as.integer(MonateHorizont)
    )]
    if (Ahv_Char == TRUE) {
      data[, AhvNr := as.character(AhvNr)]
    } else {
      data[, AhvNr := as.numeric(AhvNr)]
    }
  }
