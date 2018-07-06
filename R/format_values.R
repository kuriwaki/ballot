#' Standardize race variables to proper names with leading 7-character codes
#'
#' @param vec Vector of unstandardized race names
#'
#' @export
#'
#' @examples
#' vec <- c("CON0001 House 1", "CONG007 House 7", "U. S. Senator", "CCNL001 Council 1", "CCD0001 Council 1")
#' std_race(vec)
#'
std_race <- function(vec) {
  prs_regex <- "President.*"
  pty_regex <- "Straight Party"
  none_regx <- ".*NO VOTES CAST.*"

  # One per state
  gov_regex <- "^Governor"
  ltg_regex <- "^(Lieutenant|Lt)\\s+Governor"
  atg_regex <- "^Attorney General"
  sos_regex <- "Secretary of State"
  sad_regex <- "^Auditor"
  adj_regex <- "^Adjutant General"
  str_regex <- "^State Treasurer"
  cmp_regex <- "^Comp(tr|rt)oller General"
  ssi_regex <- "^State Superintendent.*"
  agr_regex <- "^Commissioner of Agri.*"

  # One per County or one per several counties level
  cad_regex <- "County Auditor"
  ctr_regex <- "^County Treasurer"
  cor_regex <- "Coroner"
  shf_regex <- "^Sheriff"
  clr_regex <- "(County )?Clerk of Cour(t|)"
  jpr_regex <- "^Probate Judge"
  ccc_regex <- "County Council Chair"
  rgd_regex <- "Register of Deeds"
  rmc_regex <- "Register of Mesne Convey(a|e)nce"

  cal_regex <- "County Council At( |-)Large"
  ccl3_regx <- "^C(CD|C0|NC(?=0)|OC(?=000))" # standardize CCD/CC0... three character  to CCL
  ccl4_regx <- "^C(CNL|NCL|OCL|OC(?=00[1-9])|YCL)" # standardize CCNL to CCL, and CC001 to CCL0
  ccl5_regx <- "(^CTYCN|^CCSCH(?=[0-9]+\\sCounty))"  # change to CCL00
  ccl6_regx <- "^CCLIST" # change to CCL000

  ccs_regex <- "County Supervisor"
  ccm_regex <- "County Manager"

  hou_regex <- "^HOU(S|(?=0[0-9][0-9]\\s))" # standardize HOUS to HOU0, standardize HOU078 (6 chars) to HOU0078
  ssn_regex <- "SEN(?=0[0-9][0-9]\\s)" # SEN028 to SEN0028

  # potentially many per county
  sch3_regx <- "(^BOE|^SB(?=000[1-7]\\s)|^SB0(?=0[0-9][0-9][0-9]\\s))" # SB0001 to SCH0001
  sch4_regx <- "^SB0(?=00[0-9][0-9]\\s)" # SCB0001 to SCH0001
  scb_regex <- "(Board of Education Chair|School Board Chairman)"
  sca_regex <- "(School Board Trustee|School Board At Large)" # McCormick is at-large, elects four

  # multiple votes per person
  wat_regex <- "(^WAT00|^WSD00|^WSH00|^WTR00|^WS000|^WATWR)"
  sow_regex <- "Soil (and|&) Water.*"

  # Federal
  ushou_ptrn1 <- "CON(G|0|)00"
  ushou_ptrn2 <- "U\\.?S\\.?\\sHouse of Rep(\\.|resentatives)\\s+Dist(|rict)\\s+" # one way a small minority show it
  h01_regex <- glue("({ushou_ptrn1}1.*|{ushou_ptrn2}1)")
  h02_regex <- glue("({ushou_ptrn1}2.*|{ushou_ptrn2}2)")
  h03_regex <- glue("({ushou_ptrn1}3.*|{ushou_ptrn2}3)")
  h04_regex <- glue("({ushou_ptrn1}4.*|{ushou_ptrn2}4)")
  h05_regex <- glue("({ushou_ptrn1}5.*|{ushou_ptrn2}5)")
  h06_regex <- glue("({ushou_ptrn1}6.*|{ushou_ptrn2}6)")
  h07_regex <- glue("({ushou_ptrn1}7.*|{ushou_ptrn2}7)")
  sen_regex <- "^U\\.?\\s?S\\.? Senat(e|or)$"
  sn2_regex <- "^U\\.?\\s?S\\.? Senat(e|or) \\(Unexpired? Term\\)"


  inner <- function(input, regex_str, replacement) {
    str_replace(input, regex(regex_str, ignore_case = TRUE), replacement)
  }

  vec %>%
    inner(prs_regex, "PRS0000 President")  %>%
    inner(pty_regex, "PTY0000 Straight Party") %>%
    inner(none_regx, "A000000 Absentee for all Offices") %>%
    inner(gov_regex, "GOV0000 Governor")  %>%
    inner(ltg_regex, "LGV0000 Lieutenant Governor") %>%
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
    inner(ccl3_regx, "CCL") %>%
    inner(ccl4_regx, "CCL0") %>%
    inner(ccl5_regx, "CCL00") %>%
    inner(ccl6_regx, "CCL000") %>%
    inner(ccc_regex, "CCL0000 County Coucil Chair") %>%
    inner(cal_regex, "CCL0000 County Coucil at Large") %>%
    inner(ccs_regex, "CCS0000 County Supervisor") %>%
    inner(ccm_regex, "CCM0000 County Manager") %>%
    inner(sch3_regx, "SCH") %>%
    inner(sch4_regx, "SCH0") %>%
    inner(scb_regex, "SCH0000 School Board Chair") %>%
    inner(sca_regex, "SCH0000 School Board At-Large") %>%
    inner(hou_regex, "HOU0") %>%
    inner(ssn_regex, "SEN0") %>%
    inner(rgd_regex, "RGD0000 Register of Deeds") %>%
    inner(rmc_regex, "RMC0000 Register of Mesne Conveyance") %>%
    inner(wat_regex, "WAT00") %>%
    inner(sow_regex, "SOW0000 Soil and Water Commissioner") %>%
    inner(h01_regex, "USHOU01 US House SC-01") %>%
    inner(h02_regex, "USHOU02 US House SC-02") %>%
    inner(h03_regex, "USHOU03 US House SC-03") %>%
    inner(h04_regex, "USHOU04 US House SC-04") %>%
    inner(h05_regex, "USHOU05 US House SC-05") %>%
    inner(h06_regex, "USHOU06 US House SC-06") %>%
    inner(h07_regex, "USHOU07 US House SC-07") %>%
    inner(sen_regex, "USSEN01 US Senator") %>%
    inner(sn2_regex, "USSEN02 US Senator (Special)")
}



#' Standardize solicitor race with district number, based on county
#'
#' @param vec a character vector that contains some solicitor district values
#' @param is_solicitor a logical vector of the same length as \code{vec} that is
#' \code{TRUE} when the index is a solicitor, and \code{FALSE} otherwise
#' @param county A vector of strings, south carolina counties.
#' @param data A data that matches counties to solicitor district keys. Provided in package.
#'
#'@examples
#' raw <- c("Solicitor Circuit District 14", "Solicitor 10th Circuit", "County Council")
#' is_solicit <- c(TRUE, TRUE, FALSE)
#' county <- c("Anderson", "Jasper", "Charleston")
#' std_sc_solicit(raw, is_solicit, county)
#'
#' @export

std_sc_solicit <- function(vec, is_solicit, county, data = sc_counties) {
  stopifnot(length(vec) == length(is_solicit) & length(vec) == length(county))

  county_tbl <- tibble(county = county, race = vec)

  coded <- left_join(county_tbl,  select(sc_counties, county, sol_race_code), by = "county") %>%
    mutate(race_code = str_c(sol_race_code, vec, sep = " ")) %>%
    pull(race_code)

  coded[!is_solicit] <- NA

  coded
}


#' Standardize race with first letter after code
#'
#' @param vec a character vector that contains some values to be used in code
#' @param use a logical vector of the same length as \code{vec} that is
#' \code{TRUE} when the index is to be transformed, and \code{FALSE} otherwise
#'
#' raw <- c("CSB West Ashley", "CSB East Cooper", "County Council")
#' use <- c(TRUE, TRUE, FALSE)
#' std_sc_first(raw, use, "CSB ", "SCH")
#'
#' @export

std_sc_first <- function(vec, use, code_regex, code_replace) {
  stopifnot(length(vec) == length(use))
  values <- str_replace(vec, code_regex, "")
  key <- str_pad(str_sub(values, 1, 1), width = "4", pad = "0", side = "left")

  coded <- str_c(code_replace, key, " ", vec)
  coded[!use] <- NA

  coded
}


#' Standardize options in referendum race
#'
#'
#' @param vec Vector of candidate / vote options
#'
#' @export
#'
#' @examples
#' std_ref_option(c("No", "Opposed to Question", "Opposed", "W/I ANY OPPOSED"))
#'
std_ref_option <- function(vec) {
  vec %>%
    str_replace(regex("(?<!^W/I).*In Favor.*", ignore_case = TRUE), "Yes") %>%
    str_replace(regex("(?<!^W/I).*Opposed.*", ignore_case = TRUE), "No")
}


#' Basic standardizations
#'
#' Removes a special character, change to ascii for standardization, remove periods at the end of names
#'
#' @param tbl A table with the column "cand_name"
#'
#' @export
#'
cand_to_ascii <- function(tbl) {
  tbl %>%
    mutate(cand_name = str_replace(cand_name, "�", ""),
           cand_name = iconv(cand_name, to = "ASCII", sub = ""),
           cand_name = str_replace(cand_name, "\\.$", ""))
}



