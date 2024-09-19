#' Define Global Variables
#'
#' Round up all unbound variables, such that the note 'no visible binding for
#' global variable' when checking the package disappears.
#'
#' @importFrom broom tidy
#' @importFrom openxlsx read.xlsx
#' @import bit64
#' @import dplyr
#' @import magrittr
#'
#' @seealso \href{https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887}{Link1}, \href{https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/}{link2} and \href{https://stackoverflow.com/questions/40251801/how-to-use-utilsglobalvariables}{link3} # nolint: line_length_linter.
globals <- function() {
    "This is not a function"
}
utils::globalVariables(c(".", "..col_order"))
