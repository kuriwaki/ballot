#' Standardize contest variables
#'
#' Change a vector of contest names to o proper names with leading 7-character codes
#'
#' @param vec Vector of unstandardized contest names
#'
#' @export
#'
#' @importFrom glue glue
#' @importFrom stringr str_replace_all
#'
#' @examples
#' vec <- c(
#'   "CON0001 House 1", "CONG007 House 7", "CONGR02 U.S. House of Rep Dist 2",
#'   "CNG0003 U S House of Representatives Dis",
#'   "CONO006 U S House of Rep Dist 6	",
#'   "US House of Representatives Dist 2",
#'   "U S House of Rep Dist 6",
#'   "U.S. House of Rep. Dist. 5",
#'   "CON02 U S House of Representatives Distr",
#'   "CON0001 House 1", "CONG007 House 7",
#'   "Auditor", "State Treasurer", "Adjutant General", "Straight Party",
#'   "HSE0118 State House of Representatives D",
#'   "HSE0124 State House of Representatives D",
#'   "CCNL001 Council 1",
#'   "CCD0001 Council 1",
#'   "CCNL001 Council 1",
#'   "CCD4 County Council District 4",
#'   "CCD10 County Council District 10",
#'   "CCD11 County Council District 11",
#'   "PSD4 HH#1 Public Service District Subdis",
#'   "MAY3 Town of Hilton Head Island Mayor", "MUN7 Beaufort City Council City of Beauf",
#'   "SBD5 County School Board District 5",
#'   "Governor", "Lt Governor",
#'   "Attorney General", "Secretary of State", "SOIL & WATER COMMISSION",
#'   "State Superintendent of Education", "US Senate",
#'   "Comptroller General",
#'   "Commissioner of Agriculture",
#'   "Treasurer", "Sheriff", "Probate Judge",
#'   "U. S. Senator",
#'   "President", "PREsident",
#'   "Straight Party",
#'   "U. S. Senator"
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
  ush_p2 <- "U\\s?\\.?S\\s?\\.?\\sHouse of Rep(|s|\\.|resentatives)\\s+Dis(|t|tr|trict)\\.?\\s?" # one way a small minority show it
  ush_p3 <- "(C[A-Z0]+|UDIST)"

  regex_natnl <- tribble(
    ~pattern, ~replace,
    "President.*", "PRS0000 President",
    suppressWarnings(glue("({ush_p1}1.*|{ush_p2}1|{ush_p3}1\\s+{ush_p2}.*)")), "USH0001 US House SC-01",
    suppressWarnings(glue("({ush_p1}2.*|{ush_p2}2|{ush_p3}2\\s+{ush_p2}.*)")), "USH0002 US House SC-02",
    suppressWarnings(glue("({ush_p1}3.*|{ush_p2}3|{ush_p3}3\\s+{ush_p2}.*)")), "USH0003 US House SC-03",
    suppressWarnings(glue("({ush_p1}4.*|{ush_p2}4|{ush_p3}4\\s+{ush_p2}.*)")), "USH0004 US House SC-04",
    suppressWarnings(glue("({ush_p1}5.*|{ush_p2}5|{ush_p3}5\\s+{ush_p2}.*)")), "USH0005 US House SC-05",
    suppressWarnings(glue("({ush_p1}6.*|{ush_p2}6|{ush_p3}6\\s+{ush_p2}.*)")), "USH0006 US House SC-06",
    suppressWarnings(glue("({ush_p1}7.*|{ush_p2}7|{ush_p3}7\\s+{ush_p2}.*)")), "USH0007 US House SC-07",
    "^(U\\.?\\s?S\\.?||UNITED STATES) Senat(e|or)$", "USS0001 US Senator",
    "^U\\.?\\s?S\\.? Senat(e|or) \\(Unexpired? Term\\)", "USS0002 US Senator (Special)",
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
    "^State Treasurer", "STR0000 State Treasurer",
    "^Comp(tr|rt)oller General", "CMP0000 Comptroller General",
    "^State Superintendent.*", "SSI0000 State Superintendent of Education",
    "^Commissioner of Agri.*", "AGR0000 State Commissioner of Agriculture"
  )

  # state leg
  regex_stleg <- tribble(
    ~pattern, ~replace,
    "^HOU(S|(?=0[0-9][0-9]\\s))", "HOU0", # standardize HOUS to HOU0, standardize HOU078 (6 chars) to HOU0078
    "^(STH|HSE)0(?=[0-9][0-9][0-9]\\s)", "HOU0", # STH or HSE notation, e.g. in Greenville 2010
    "SEN(?=0[0-9][0-9]\\s)", "SEN0", # SEN028 to SEN0028
    "State Senate Dist(|rict)\\s(?=[0-9][0-9])", "SEN00"
  )


  # county wide
  regex_ctwid <- tribble(
    ~pattern, ~replace,
    "County Auditor", "CAUD000 County Auditor",
    "^County Treasurer", "CTR0000 County Treasurer",
    "Coroner", "COR0000 Coroner",
    "^Sheriff", "SHF0000 Sheriff",
    "^Probate Judge", "JPR0000 Probate Judge",
    "(County )?Clerk of Cour(t|)", "CLR0000 Clerk of Court",
    "County Council Chair", "CCC0000 County Coucil Chair",
    "Register of Deeds", "RGD0000 Register of Deeds",
    "Register of Mesne Convey(a|e)nce", "RMC0000 Register of Mesne Conveyance",
  )


  regex_ctcnl <- tribble(
    ~pattern, ~replace,
    "County Council At( |-)Large", "CCA0000 County Coucil at Large",
    "^CCL(?=00[0-9][0-9])", "CCD", # general change CCL, CCD
    "^C(CD|C0|NC(?=0)|OC(?=000))", "CCD", # standardize CCD/CC0... three character  to CCD
    "^C(CNL|NCL|OCL|OC(?=00[1-9]\\s)|YCL)", "CCD0", # standardize CCNL to CCD, and CC001 to CCD0
    "^COC(?=00[0-9][0-9])", "CCD",
    "^CCL(?=[1-9]\\s)" ,"CCD000",
    "^CCL(?=[1-9][0-9]\\s)", "CCD00",
    "(^CTYCN|^CCSCH(?=[0-9]+\\sCounty))", "CCD00", # change to CCD00
    "^CCLIST", "CCD000", # change to CCL000
    "County Supervisor", "CCS0000 County Supervisor",
    "County Manager", "CCM0000 County Manager"
  )

  regex_schbd <- tribble(
    ~pattern, ~replace,
    "(^BOE|^SB(?=000[1-7]\\s)|^SB0(?=0[0-9][0-9][0-9]\\s))", "SCH", # SB0001 to SCH0001
    "^SB0(?=00[0-9][0-9]\\s)", "SCH0", # SCB0001 to SCH0001
    "^SBD(?=[0-9]\\s)", "SCH000", # SBD4 to SBD0004
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

  # REPLACE
  str_replace_all(vec, regex(recode_vec, ignore_case = TRUE))
}

# 54 secs for 1 million rows


#' Separate contest and number
#'
#' create a "code" that should be a seven-character code if properly formatted
#' reassemble contest by code and name
#'
#' @param tbl long data
#' @param ref_county dataframe with manual contest_codes for county-wide referenda to join
#' @param ref_state same, but for statewide referenda
#'
#' @export
#'
code_c_name <- function(tbl, ref_county, ref_state) {
  tbl <- lazy_dt(tbl)
  ref_county <- lazy_dt(ref_county)
  ref_state <- lazy_dt(ref_state)

  ref_state <- select(ref_state, elec, contest_orig, contest_code)
  ref_county <- select(ref_county, elec, county, contest_orig, contest_code)

  tbl %>%
    left_join(ref_state, by = c("contest_orig", "elec")) %>% # with state
    mutate(contest = coalesce(contest_code, contest)) %>%
    select(-contest_code) %>%
    left_join(ref_county, by = c("elec", "county", "contest_orig")) %>% # now with c ount
    mutate(contest_code = str_c(contest_code, " ")) %>%
    mutate(contest_code = replace(contest_code, is.na(contest_code), "")) %>%
    mutate(contest = str_c(contest_code, contest)) %>%
    select(-contest_code) %>%
    as_tibble() %>%
    separate(contest, into =  c("contest_code", "contest_name"), sep = " ", extra = "merge") %>%
    mutate(contest_type = str_extract(contest_code, "[A-Z]+"))
}


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

#' Format solicitor and school district
#'
#' @param tbl long datafrae
#'
#' @export
#'
sol_fmt <- function(tbl) {
  tbl %>%
    as.data.table() %>%
    mutate(is_solicitor = str_detect(contest, regex("Solicitor", ignore_case = TRUE))) %>%
    mutate(is_csb = str_detect(contest, regex("^C(|C)SB", ignore_case = TRUE))) %>%
    mutate(solicitor_fmt = std_sc_solicit(contest, is_solicitor, county)) %>%
    mutate(csb_fmt = std_sc_first(contest, is_csb, "CSB ", "SCH")) %>%
    mutate(contest = coalesce(solicitor_fmt, csb_fmt, contest)) %>%
    select(-is_solicitor, -solicitor_fmt, -is_csb, -csb_fmt) %>%
    select(elec:ballot_style, contest_orig, contest, everything()) %>%
    as_tibble()
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
    str_replace(regex("No, .*", ignore_case = TRUE), "No") %>%
    str_replace(regex("(?<!^W/I).*Opposed.*", ignore_case = TRUE), "No")
}

#' Standardize Yes / No / NA to -1, 1, 0.
#'
#' @param vec long values of Yes/Nos Must have been passed through filter_existing,
#' otherwise all NAs are coded as 0s
#'
#' @export
stdnum_yesno <- function(vec) {
  numvec <- recode(vec, `Yes` = -1, `No` = 1, .missing = 0)
  stopifnot(n_distinct(numvec) == 3)
  numvec
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


#' Add "other" and write in votes
#'
#' For a given office, looks at office_vote (the candidate in characters) and
#' the office_party (the party, -1, 1, or otherwise 0), and edits the 0 to 0.5
#' if office_party is 0 but candiddate name is not NA (a third party or write-in)
#'
#' @param tbl wide table
#' @param var the unquoted office variable
#'
#' @export
add_wi <- function(tbl, var) {
  var <- enquo(var)
  var_name <- quo_name(var)
  votevar_name <- str_replace(var_name, "_party", "_vote")
  votevar <- enquo(votevar_name)

  tbl %>%
    mutate(!!var := replace(!!var, !!var == 0 & !is.na(.data[[votevar_name]]), 0.5))
}



#' Custom fixes for contest names that are hard to recode by regex
#'
#' @param tbl long tibble  of votes
#' @param elec election year
#'
#' @export
#'
fix_custom <- function(tbl, elec) {
  year <- as.integer(elec)
  tbl <- as.data.table(tbl)

  # unambiguously all-year changes
  tbl_common <- tbl %>%
    mutate(contest = replace(contest, contest_orig == "SEN45 State Senate District 45", "SEN0045 State Senate 45")) %>%
    mutate(contest = replace(contest, contest_orig == "HOUS55 State House of Rep Dist 55", "HOU0055 State House of Rep Dist 55")) %>%
    mutate(contest = replace(contest, contest_orig == "HOUS57 State House of Representatives Di", "HOU0057 State House of Rep Dist 57")) %>%
    mutate(contest = replace(contest, (contest_orig %in% c("CON0001 US House of Representatives Dist", "CON0001 U.S. House of Representatives")) & county == "Kershaw", "USH0005 US House SC-05"))

  if (year == 2018) {
    out <- tbl_common %>%
      mutate(contest = replace(contest, contest_orig %in% c("Lieutenant Governor", "Governor", "Lt Governor"), "GOV0000 Governor and Lieutenant Governor")) %>%
      mutate(contest = replace(contest, contest_orig == "US House of Representatives", "USH0003 US House SC-03"))
  }

  if (year == 2016) {
    out <- tbl_common %>%
      mutate(contest = replace(contest, contest_orig == "US House of Representatives" & county == "Greenwood" & elec == "2016-11-08", "USH0003 US House SC-03")) %>%
      mutate(contest = replace(contest, contest_orig == "State House of Rep Dist 122", "HOU0122 State House 122")) %>%
      mutate(contest = replace(contest, contest_orig == "Treasurer" & county %in% c("Dorchester", "Oconee") & elec == "2016-11-08", "CTRES00 County Treasurer")) %>%
      mutate(contest = replace(contest, contest_orig == "MUNCA00 Carlisle Mayor" & county %in% "Union", "MUN0001 Carlisle Mayor")) %>%
      mutate(contest = replace(contest, contest_orig == "MUN001 Mayor" & county %in% "Sumter",       "MUN0001 Sumter Mayor")) %>%
      mutate(contest = replace(contest, contest_orig == "MUN0003 Mayor" & county %in% "Kershaw",     "MUN0001 Camden Mayor")) %>%
      mutate(contest = replace(contest, contest_orig == "MUNABBE Mayor" & county %in% "Abbevile",    "MUN0001 Abbeville Mayor")) %>%
      mutate(contest = replace(contest, contest_orig == "MUNFLOR Mayor" & county %in% "Florence",    "MUN0001 Florence Mayor")) %>%
      mutate(contest = replace(contest, contest_orig == "MUNUN00 Union Mayor" & county %in% "Union", "MUN0001 Union Mayor")) %>%
      mutate(contest = replace(contest, contest_orig == "MUNWHIT Mayor" & county %in% "Newberry",    "MUN0001 Whitmire Mayor")) %>%
      mutate(contest = replace(contest, contest_orig == "MUNWOOD Mayor" & county %in% "Spartanburg", "MUN0001 Woodruff Mayor")) %>%
      mutate(contest = replace(contest, contest_orig == "TWNALLN Mayor" & county %in% "Allendale",   "MUN0001 Allendale Mayor")) %>%
      mutate(contest = replace(contest, contest_orig == "WSH0014 Mayor" & county %in% "Greenwood",   "MUN0001 Ware Shoals Mayor"))
  }

  if (year == 2014) {
    out <- tbl_common %>%
      mutate(contest = replace(contest, contest_orig == "US House of Representatives" & county == "Greenwood", "USH0003 US House SC-03")) %>%
      mutate(contest = replace(contest, str_detect(contest_orig, "(U.S.|US) House of Representatives District 7"), "USH0007 US House SC-07")) %>%
      mutate(contest = replace(contest, contest_orig == "CCNL05 County Council District 5", "CCD0005 County Council District 5")) %>%
      mutate(contest = replace(contest, contest_orig == "Treasurer", "CTRES00 County Treasurer"))
  }

  if (year == 2012) {
    out <- tbl_common %>%
      mutate(contest = replace(contest, contest_orig == "UDIST1 US House of Representatives Dist", "USH0001 US House SC-01")) %>%
      mutate(contest = replace(contest, contest_orig == "U.S. House of Representatives District 5" & county == "Chesterfield", "USH0007 US House SC-07")) %>%
      mutate(contest = replace(contest, contest_orig == "US House of Representatives" & county == "Greenwood", "USH0003 US House SC-03")) %>%
      mutate(contest = replace(contest, contest_orig == "CTY ABB Mayor-Abbeville", "MUN0001 Abbeville Mayor"))
  }

  if (year == 2010) {
    out <- tbl_common %>%
      mutate(contest = replace(contest, contest_orig == "U.S. House of Representatives" & county == "Greenville", "USH0004 US House SC-04")) %>%
      mutate(contest = replace(contest, contest_orig == "US House of Representatives" & county == "Greenwood", "USH0003 US House SC-03")) %>%
      mutate(contest = replace(contest, contest_orig == "US House of Representatives" & county == "Newberry", "USH0005 US House SC-05")) %>%
      mutate(contest = replace(contest, contest_orig == "NG0006 U.S. House of Rep" & county == "Florence", "USH0006 US House SC-06")) %>%
      mutate(contest = replace(contest, contest_orig == " CCD10 County Council District 10" & county == "Beaufort", "CCD0010 County Council District 10"))
  }

  as_tibble(out)
}
