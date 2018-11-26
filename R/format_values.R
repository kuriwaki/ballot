#' Standardize contest variables
#'
#' Change a vector of contest names to o proper names with leading 7-character codes
#'
#' @param vec Vector of unstandardized contest names
#'
#' @export
#'
#' @importFrom glue glue
#'
#' @examples
#' vec <- c(
#'   "CON0001 House 1", "CONG007 House 7", "CONGR02 U.S. House of Rep Dist 2",
#'   "CNG0003 U S House of Representatives Dis",
#'   "CONO006 U S House of Rep Dist 6	",
#'   "U S House of Rep Dist 6",
#'   "CON02 U S House of Representatives Distr",
#'   "CON0001 House 1", "CONG007 House 7",
#'   "U. S. Senator",
#'   "President", "PREsident",
#'   "Straight Party",
#'   "U. S. Senator", "CCNL001 Council 1",
#'   "CCNL001 Council 1", "CCD0001 Council 1"
#' )
#' std_contest(vec)
std_contest <- function(vec, .type = NULL) {


  # special codes
  regex_spcl <- tribble(
    ~pattern, ~replace,
    ".*Straight Party.*", "PTY0000 Straight Party",
    ".*NO VOTES CAST.*", "A000000 Absentee for all Offices",
  )

  # Congress and President
  ush_p1 <- "CO?N(G|G0|G00|GR|O0|00|0)0"
  ush_p2 <- "U\\s?\\.?S\\s?\\.?\\sHouse of Rep(|s|\\.|resentatives)\\s+Dist(|r|rict)\\s+" # one way a small minority show it
  ush_p3 <- "C[A-Z0]+"

  regex_natnl <- tribble(
    ~pattern, ~replace,
    "President.*", "PRS0000 President",
    suppressWarnings(glue("({ush_p1}1.*|{ush_p2}1|{ush_p3}1\\s+{ush_p2}.*)")), "USHOU01 US House SC-01",
    suppressWarnings(glue("({ush_p1}2.*|{ush_p2}2|{ush_p3}2\\s+{ush_p2}.*)")), "USHOU02 US House SC-02",
    suppressWarnings(glue("({ush_p1}3.*|{ush_p2}3|{ush_p3}3\\s+{ush_p2}.*)")), "USHOU03 US House SC-03",
    suppressWarnings(glue("({ush_p1}4.*|{ush_p2}4|{ush_p3}4\\s+{ush_p2}.*)")), "USHOU04 US House SC-04",
    suppressWarnings(glue("({ush_p1}5.*|{ush_p2}5|{ush_p3}5\\s+{ush_p2}.*)")), "USHOU05 US House SC-05",
    suppressWarnings(glue("({ush_p1}6.*|{ush_p2}6|{ush_p3}6\\s+{ush_p2}.*)")), "USHOU06 US House SC-06",
    suppressWarnings(glue("({ush_p1}7.*|{ush_p2}7|{ush_p3}7\\s+{ush_p2}.*)")), "USHOU07 US House SC-07",
    "^(U\\.?\\s?S\\.?||UNITED STATES) Senat(e|or)$", "USSEN01 US Senator",
    "^U\\.?\\s?S\\.? Senat(e|or) \\(Unexpired? Term\\)", "USSEN02 US Senator (Special)",
  )


  # One per state
  regex_stwid <- tribble(
    ~pattern, ~replace,
    "^Governor", "GOV0000 Governor",
    "^(Lieutenant|Lt)\\s+Governor", "LGV0000 Lieutenant Governor",
    "^Attorney General", "ATG0000 Attorney General",
    "Secretary of State", "SOS0000 Secretary of State",
    "^Auditor", "AUD0000 Auditor",
    "^Adjutant General", "ADJ0000 Adjutant General",
    "^State Treasurer", "STRES00 State Treasurer",
    "^Comp(tr|rt)oller General", "CMP0000 Comptroller General",
    "^State Superintendent.*", "SSI0000 State Superintendent of Education",
    "^Commissioner of Agri.*", "AGR0000 State Commissioner of Agriculture"
  )

  # state leg
  regex_stleg <- tribble(
    ~pattern, ~replace,
    "^HOU(S|(?=0[0-9][0-9]\\s))", "HOU0", # standardize HOUS to HOU0, standardize HOU078 (6 chars) to HOU0078
    "SEN(?=0[0-9][0-9]\\s)", "SEN0", # SEN028 to SEN0028
    "State Senate Dist(|rict)\\s(?=[0-9][0-9])", "SEN00"
  )


  # county wide
  regex_ctwid <- tribble(
    ~pattern, ~replace,
    "County Auditor", "CAUD000 County Auditor",
    "^County Treasurer", "CTRES00 County Treasurer",
    "Coroner", "COR0000 Coroner",
    "^Sheriff", "SHF0000 Sheriff",
    "^Probate Judge", "JPRB000 Probate Judge",
    "(County )?Clerk of Cour(t|)", "CLR0000 Clerk of Court",
    "County Council Chair", "CCL0000 County Coucil Chair",
    "Register of Deeds", "RGD0000 Register of Deeds",
    "Register of Mesne Convey(a|e)nce", "RMC0000 Register of Mesne Conveyance",
  )


  regex_ctcnl <- tribble(
    ~pattern, ~replace,
    "County Council At( |-)Large", "CCL0000 County Coucil at Large",
    "^C(CD|C0|NC(?=0)|OC(?=000))", "CCL", # standardize CCD/CC0... three character  to CCL
    "^C(CNL|NCL|OCL|OC(?=00[1-9])|YCL)", "CCL0", # standardize CCNL to CCL, and CC001 to CCL0
    "(^CTYCN|^CCSCH(?=[0-9]+\\sCounty))", "CCL00", # change to CCL00
    "^CCLIST", "CCL000", # change to CCL000
    "County Supervisor", "CCS0000 County Supervisor",
    "County Manager", "CCM0000 County Manager"
  )

  regex_schbd <- tribble(
    ~pattern, ~replace,
    "(^BOE|^SB(?=000[1-7]\\s)|^SB0(?=0[0-9][0-9][0-9]\\s))", "SCH", # SB0001 to SCH0001
    "^SB0(?=00[0-9][0-9]\\s)", "SCH0", # SCB0001 to SCH0001
    "^SCH(|O)(?=0[0-9]\\s)", "SCH00", # SCH02 to SCH0002; SCHO02 to SCH0002
    "(Board of Education Chair|School Board Chairman)", "SCH0000 School Board Chair",
    "(School Board Trustee|School Board At Large)", "SCH0000 School Board At-Large", # McCormick is at-large, elects four
  )


  # multiple votes per person
  regex_spd <- tribble(
    ~pattern, ~replace,
    "(^WAT00|^WSD00|^WSH00|^WTR00|^WS000|^WATWR)", "WAT00",
    "Soil (and|&) Water.*", "SOW0000 Soil and Water",
  )

  regex_recode <- bind_rows(
    regex_natnl,
    regex_spcl,
    regex_stwid,
    regex_stleg,
    regex_ctwid,
    regex_ctcnl,
    regex_schbd,
    regex_spd,
  )

  # use only certain keys
  if (!is.null(.type)) {
    regex_recode <- regex_recode
  }

  # named vector
  recode_vec <- regex_recode$replace
  names(recode_vec) <- regex_recode$pattern

  # replace
  str_replace_all(vec, regex(recode_vec, ignore_case = TRUE))
}

# 54 secs for 1 million rows




#' Standardize solicitor contest with district number, based on county
#'
#' @param vec a character vector that contains some solicitor district values
#' @param is_solicitor a logical vector of the same length as \code{vec} that is
#' \code{TRUE} when the index is a solicitor, and \code{FALSE} otherwise
#' @param county A vector of strings, south carolina counties.
#' @param data A data that matches counties to solicitor district keys. Provided in package.
#'
#' @examples
#' raw <- c("Solicitor Circuit District 14", "Solicitor 10th Circuit", "County Council")
#' is_solicit <- c(TRUE, TRUE, FALSE)
#' county <- c("Anderson", "Jasper", "Charleston")
#' std_sc_solicit(raw, is_solicit, county)
#' @export

std_sc_solicit <- function(vec, is_solicit, county, data = sc_counties) {
  stopifnot(length(vec) == length(is_solicit) & length(vec) == length(county))

  county_tbl <- tibble(county = county, contest = vec)

  coded <- left_join(county_tbl, select(sc_counties, county, sol_contest_code), by = "county") %>%
    mutate(contest_code = str_c(sol_contest_code, vec, sep = " ")) %>%
    pull(contest_code)

  coded[!is_solicit] <- NA

  coded
}


#' Standardize contest with first letter after code
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


#' Standardize options in referendum contest
#'
#'
#' @param vec Vector of candidate / vote options
#'
#' @export
#'
#' @examples
#' std_yes_no(c("No", "Opposed to Question", "Opposed", "W/I ANY OPPOSED"))
std_yes_no <- function(vec) {
  vec %>%
    str_replace(regex("(?<!^W/I).*Favor.*", ignore_case = TRUE), "Yes") %>%
    str_replace(regex("(?<!^W/I).*Opposed.*", ignore_case = TRUE), "No")
}


#' Basic standardizations
#'
#' Removes a special character, change to ascii for standardization, remove periods at the end of names
#'
#' @param tbl A table with the column "choice_name"
#'
#' @export
#'
choice_to_ascii <- function(tbl) {
  stopifnot(any(colnames(tbl) == "choice_name"))
  tbl %>%
    mutate(
      choice_name = str_replace(choice_name, "�", ""),
      choice_name = iconv(choice_name, to = "ASCII", sub = ""),
      choice_name = str_replace(choice_name, "\\.$", "")
    )
}
