#' Teuerung
#'
#' Calculates the cost increase ('Niveauteuerung pro Kanton') used as an extrapolation factor in risk equalization. Derivation see: \href{https://www.kvg.org/de/risikoausgleich-pcg-_content---1--3125.html}{Berechnungsformeln fuer den Risikoasugleich mit PCG ab 2020, Section B}.
#'
#' @param dt_T_14 A \code{data.table} featuring individual insurance data according to naming and data type standards used in data set \code{test_data}.
#' @param dt_T_1_14 A \code{data.table} featuring individual insurance data according to naming and data type standards used in data set \code{test_data}.
#' @importFrom magrittr "%>%"
#' @importFrom data.table ":="
#' @export
#' @return A \code{data.table} containing
#'  \item{Kanton}{Swiss canton}
#'  \item{pi_Niv_k}{Total cost increase component divided by structural cost increase component ('Niveateuerung')}
#'  \item{pi_Str_k1}{Structural cost increase component}
#'  \item{pi_Ges_k1}{Total cost increase component}
#' @author Magnus Vieten, Yannick Schwarz
#' @seealso \url{https://www.kvg.org/api/rm/S443P62E88YU756/200420-kuv-va-berechnungsformeln-fuer-den-risikoau.pdf}
#' @examples \dontrun{
#' pcg_grouping(data_input = some_RA_IndData_object, pcg_liste = "path/to/prepped/pcg_list.xlsx")
#' }
teuerung_fun <- function(dt_T_14, dt_T_1_14) {
  Monate <- Altersklasse <- BagNr <- AhvNr <- Geburtsjahr <- Kanton <- Geschlecht <- Aufenthalt_Vj <- Kosten <- Kobe <- Y_j_T <- Y_kr_T <- NULL # no visible binding NOTE in R CMD check
  Y_j_T_1 <- y_bar_kr_T_1 <- y_bar_r_T_1 <- y_bar_kr_T <- m_kr_T <- m_kr_T_1 <- Y_kr_T_1 <- y_bar_k_T_1 <- Y_k_T <- Y_k_T_1 <- pi_Str_k1 <- pi_Niv_k <- NULL

  # Create all RGs
  all_rg <- rg_grid(children = FALSE, kantonal = TRUE)

  # T: Durchschnittliche Nettokosten pro Kt und Rg
  mean_netto_kr_T <- unique(dt_T_14[
    Monate > 0 & Altersklasse != "0-18",
    .(
      BagNr,
      AhvNr,
      Geburtsjahr,
      Kanton,
      Altersklasse,
      Geschlecht,
      Aufenthalt_Vj,
      Monate,
      Kosten,
      Kobe
    )
  ])[
    , # Create Nettokosten pro Deckung j
    Y_j_T := Kosten - Kobe
  ][
    ,
    .( # Durchschnittliche Nettokosten pro Kt und Rg
      y_bar_kr_T = sum(Y_j_T) / sum(Monate),
      m_kr_T = sum(Monate),
      Y_kr_T = sum(Y_j_T)
    ),
    by = .(Kanton, Altersklasse, Geschlecht, Aufenthalt_Vj)
  ]
  # T: Durchschnittliche Nettokosten pro Kt ueber alle Rg
  sum_netto_k_T <- mean_netto_kr_T[
    ,
    .(
      Y_k_T = sum(Y_kr_T)
    ),
    by = .(Kanton)
  ]

  # T-1: Prep and create Nettokosten
  netto_kr_T_1 <- unique(dt_T_1_14[
    Monate > 0 & Altersklasse != "0-18",
    .(
      BagNr,
      AhvNr,
      Geburtsjahr,
      Kanton,
      Altersklasse,
      Geschlecht,
      Aufenthalt_Vj,
      Monate,
      Kosten,
      Kobe
    )
  ])[
    , # Create Nettokosten pro Deckung j
    Y_j_T_1 := Kosten - Kobe
  ]
  # T-1: Durchschnittliche Nettokosten pro Kt und Rg
  mean_netto_kr_T_1 <- netto_kr_T_1[
    ,
    .(
      y_bar_kr_T_1 = sum(Y_j_T_1) / sum(Monate),
      m_kr_T_1 = sum(Monate),
      Y_kr_T_1 = sum(Y_j_T_1)
    ),
    by = .(Kanton, Altersklasse, Geschlecht, Aufenthalt_Vj)
  ]
  # T-1: Durchschnittliche Nettokosten pro Rg in der ganzen CH
  mean_netto_r_T_1 <- netto_kr_T_1[
    ,
    .(y_bar_r_T_1 = sum(Y_j_T_1) / sum(Monate)),
    by = .(Altersklasse, Geschlecht, Aufenthalt_Vj)
  ]
  # T-1: Durchschnittliche Nettokosten pro Kt ueber alle Rg
  mean_netto_k_T_1 <- netto_kr_T_1[
    ,
    .(
      y_bar_k_T_1 = sum(Y_j_T_1) / sum(Monate),
      Y_k_T_1 = sum(Y_j_T_1)
    ),
    by = .(Kanton)
  ]

  # Calculate Teuerung
  pi_T <- mean_netto_kr_T %>%
    dplyr::right_join(all_rg,
      by = c(
        "Kanton",
        "Altersklasse",
        "Geschlecht",
        "Aufenthalt_Vj" = "Aufenthalt"
      )
    ) %>%
    dplyr::left_join(mean_netto_kr_T_1,
      by = c(
        "Kanton",
        "Altersklasse",
        "Geschlecht",
        "Aufenthalt_Vj"
      )
    ) %>%
    dplyr::left_join(mean_netto_r_T_1,
      by = c(
        "Altersklasse",
        "Geschlecht",
        "Aufenthalt_Vj"
      )
    ) %>%
    dplyr::left_join(mean_netto_k_T_1,
      by = "Kanton"
    ) %>%
    dplyr::left_join(sum_netto_k_T,
      by = "Kanton"
    ) %>%
    dplyr::mutate(
      y_bar_kr_T_1 = ifelse(is.na(y_bar_kr_T_1), y_bar_r_T_1, y_bar_kr_T_1),
      y_bar_kr_T = ifelse(is.na(y_bar_kr_T), 0, y_bar_kr_T),
      m_kr_T = ifelse(is.na(m_kr_T), 0, m_kr_T),
      m_kr_T_1 = ifelse(is.na(m_kr_T_1), 0, m_kr_T_1),
      Y_kr_T_1 = ifelse(is.na(Y_kr_T_1), 0, Y_kr_T_1)
    ) %>%
    dplyr::group_by(Kanton) %>%
    dplyr::summarise(
      pi_Niv_k = sum(m_kr_T * y_bar_kr_T) / sum(m_kr_T * y_bar_kr_T_1) - 1,
      # Strukturteuerung mit Summe der Nettokosten (Y_kr_T_1) / Summe aller Monate (m_kr_T_1)
      #pi_Str_k = (sum(m_kr_T * y_bar_kr_T_1) / sum(m_kr_T)) / (sum(Y_kr_T_1) / sum(m_kr_T_1)) - 1,
      # Strukturteuerung mit Summe (der Monate * durchschnittliche Nettokosten pro Kt und Rg) / Summe aller Monate (m_kr_T_1)
      #pi_Str_k2 = (sum(m_kr_T * y_bar_kr_T_1) / sum(m_kr_T)) / (sum(m_kr_T_1 * y_bar_kr_T_1) / sum(m_kr_T_1)) - 1,
      # Strukturteuerung mit Summe (der Monate * durchschnittliche Nettokosten pro Kt) / Summe aller Monate (m_kr_T_1)
      #pi_Str_k3 = (sum(m_kr_T * y_bar_kr_T_1) / sum(m_kr_T)) / (sum(m_kr_T_1 * y_bar_k_T_1) / sum(m_kr_T_1)) - 1,
      # Gesamtteuerung direkt berechnet
      #pi_Ges_k2 = (sum(m_kr_T * y_bar_kr_T) / sum(m_kr_T)) / (sum(m_kr_T_1 * y_bar_kr_T_1) / sum(m_kr_T_1)) - 1,
      # Gesamtteuerung aus Str und Niv Teuerung berechnet
      #pi_Ges_k = (pi_Str_k + 1) * (pi_Niv_k + 1) - 1,
      .groups = 'keep'
    ) %>%
    data.table::as.data.table()

  return(pi_T)
}
