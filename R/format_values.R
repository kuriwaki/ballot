#' Standardize race code
#'
#' @param vec Vector of unstandardized race names
#'
#' @export
#'
std_racecode <- function(vec) {
  prs_regex <- "President.*"
  pty_regex <- "Straight Party"

  # One per state
  gov_regex <- "^Governor"
  ltg_regex <- "^(Lieutenant|Lt) Governor"
  atg_regex <- "^Attorney General"
  sos_regex <- "Secretary of State"
  sad_regex <- "^Auditor"
  adj_regex <- "^Adjutant General"
  str_regex <- "^State Treasurer"
  cmp_regex <- "^Comp(tr|rt)oller General"
  ssi_regex <- "^State Superintendent.*"
  agr_regex <- "^Commissioner of Agri"

  # One per County or one per several counties level
  cad_regex <- "County Auditor"
  ctr_regex <- "^County Treasurer"
  cor_regex <- "Coroner"
  shf_regex <- "^Sheriff"
  clr_regex <- "[County ]?Clerk of Court"
  jpr_regex <- "^Probate Judge"
  ccc_regex <- "County Council Chair"


  wat_regex <- "Soil and Water.*"

  sen_regex <- "U\\.?\\s?S\\.? Senat(e|or)$"
  sn2_regex <- "U\\.?\\s?S\\.? Senat(e|or) \\(Unexpired Term\\)"


  inner <- function(input, regex_str, replacement, use_num = FALSE) {
    if (use_num) {
    }

    str_replace(input, regex(regex_str, ignore_case = TRUE), replacement)
  }

  vec %>%
    inner(prs_regex, "PRS0000 President")  %>%
    inner(gov_regex, "GOV0000 Governor")  %>%
    inner(ltg_regex, "LGV0000 Lieutenant Governor") %>%
    inner(pty_regex, "PTY0000 Straight Party") %>%
    inner(sos_regex, "SOS0000 Secretary of State") %>%
    inner(sad_regex, "AUD0000 Auditor") %>%
    inner(atg_regex, "ATG0000 Attorney General") %>%
    inner(adj_regex, "ADJ0000 Adjutant General") %>%
    inner(str_regex, "STRES00 State Treasurer") %>%
    inner(cmp_regex, "CMP0000 Comptroller General") %>%
    inner(ssi_regex, "SSI0000 State Superintendent of Education") %>%
    inner(agr_regex, "AGR0000 State Commissioner of Agriculture") %>%
    inner(cad_regex, "CAUD000 County Auditor") %>%
    inner(ctr_regex, "CTRES00 County Treasurer") %>%
    inner(cor_regex, "COR0000 Coroner") %>%
    inner(shf_regex, "SHF0000 Sheriff") %>%
    inner(clr_regex, "CLR0000 Clerk of Court") %>%
    inner(jpr_regex, "JPRB000 Probate Judge") %>%
    inner(ccc_regex, "CCL0000 County Coucil Chair") %>%
    inner(sol_regex, "CCL0000 County Coucil Chair") %>%
    inner(wat_regex, "WAT0000 Soil and Water District Commissioner") %>%
    inner(sen_regex, "USSEN01 US Senator") %>%
    inner(sn2_regex, "USSEN02 US Senator (Special)")
}


#' Standardize race with district number
#'
#' @param vec a character vector that contains some solicitor district values
#' @param is_solicitor a logical vector of the same length as \code{vec} that is
#' \code{TRUE} when the index is a solicitor, and \code{FALSE} otherwise
#'
#' raw <- c("Solicitor Circuit District 14", "Solicitor 10th Circuit", "County Council")
#' is_solicit <- c(TRUE, TRUE, FALSE)
#' std_sc_solicit(raw, is_solicit)

std_sc_solicit <- function(vec, is_solicit) {
  stopifnot(length(vec) == length(is_solicit))
  num <- str_pad(str_extract(vec, "\\d+"), width = "4", pad = "0")

  coded <- str_c("SOL", num, " ", vec)
  coded[!is_solicit] <- NA

  coded
}



#' Standardize options in referendum race
#'
#'
#' @param df
#'
#' @export
#'
std_ref_option <- function(df) {

  df %>%
    mutate(cand_name = replace(cand_name, grepl("In Favor", cand_name), "Yes"),
           cand_name = replace(cand_name, grepl("Opposed To The Question" , cand_name), "No"))
}
