library(data.table)
library(tidyverse)
library(microbenchmark)
library(stringi)
library(glue)

l2016 <- readRDS('~/Dropbox/EL155s/output/02_long/2016_formatted.Rds')
samp <- sample_n(l2016, 1e5)


vec <- c("President", "PREsident",
         "Straight Party", "Straight Party President",
         "CON0001 House 1", "CONG007 House 7",
         "U. S. Senator", "CCNL001 Council 1",
         "CCD0001 Council 1")


ushou_ptrn1 <- "CON(G|G0|00|0)0"
ushou_ptrn2 <- "U\\.?S\\.?\\sHouse of Rep(|s|\\.|resentatives)\\s+Dist(|rict)\\s+" # one way a small minority show it

coder_pres <-  tribble(
  ~type, ~code, ~pattern,
  "PRS0000 President", "President.*")

coder_cong <-  tribble(
  ~code, ~pattern,
  "USHOU01 US House SC-01", suppressWarnings(glue("({ushou_ptrn1}1.*|{ushou_ptrn2}1)")),
  "USHOU02 US House SC-02", suppressWarnings(glue("({ushou_ptrn1}2.*|{ushou_ptrn2}2)")),
  "USHOU03 US House SC-03", suppressWarnings(glue("({ushou_ptrn1}3.*|{ushou_ptrn2}3)")),
  "USHOU04 US House SC-04", suppressWarnings(glue("({ushou_ptrn1}4.*|{ushou_ptrn2}4)")),
  "USHOU05 US House SC-05", suppressWarnings(glue("({ushou_ptrn1}5.*|{ushou_ptrn2}5)")),
  "USHOU06 US House SC-06", suppressWarnings(glue("({ushou_ptrn1}6.*|{ushou_ptrn2}6)")),
  "USHOU07 US House SC-07", suppressWarnings(glue("({ushou_ptrn1}7.*|{ushou_ptrn2}7)")),
  "USSEN01 US Senator", "^(U\\.?\\s?S\\.?||UNITED STATES) Senat(e|or)$",
  "USSEN02 US Senator (Special)", "^U\\.?\\s?S\\.? Senat(e|or) \\(Unexpired? Term\\)"
)


coder_stwide <-  tribble(
  ~code, ~pattern,
  "PTY0000 Straight Party", "Straight Party",
  "A000000 Absentee for all Offices", ".*NO VOTES CAST.*",
  "GOV0000 Governor", "^Governor",
  "LGV0000 Lieutenant Governor", "^(Lieutenant|Lt)\\s+Governor",
  "SOS0000 Secretary of State", "Secretary of State",
  "AUD0000 Auditor", "^Auditor",
  "ATG0000 Attorney General", "^Attorney General",
  "ADJ0000 Adjutant General", "^Adjutant General",
  "STRES00 State Treasurer", "^State Treasurer",
  "CMP0000 Comptroller General", "^Comp(tr|rt)oller General",
  "SSI0000 State Superintendent of Education", "^State Superintendent.*",
  "AGR0000 State Commissioner of Agriculture", "^Commissioner of Agri.*")

coder_other <-  tribble(
  ~code, ~pattern,
  "COR0000 Coroner", "Coroner",
  "SHF0000 Sheriff", "^Sheriff",
  "CLR0000 Clerk of Court", "(County )?Clerk of Cour(t|)",
  "JPRB000 Probate Judge", "^Probate Judge",
  "RGD0000 Register of Deeds", "Register of Deeds",
  "RMC0000 Register of Mesne Conveyance", "Register of Mesne Convey(a|e)nce",
  "CCL0000 County Coucil Chair", "County Council Chair",
  "CCL0000 County Coucil at Large", "County Council At( |-)Large",
  "CCL", "^C(CD|C0|NC(?=0)|OC(?=000))", # standardize CCD/CC0... three character  to CCL
  "CCL0", "^C(CNL|NCL|OCL|OC(?=00[1-9])|YCL)", # standardize CCNL to CCL, and CC001 to CCL0
  "CCL00", "(^CTYCN|^CCSCH(?=[0-9]+\\sCounty))",  # change to CCL00
  "CCL000", "^CCLIST", # change to CCL000
  "CLR0000 Clerk of Court", "(County )?Clerk of Cour(t|)")



coder <- bind_rows(
  coder_pres,
  coder_cong,
  coder_stwide,
  coder_other,
)



std_contest2 <- function(vec, key = coder, .type = NULL) {

  if (!is.null(.type)) {
    key <- filter(key, type == .type)
  }

  named_coder <- key$code
  names(named_coder) <- key$pattern

  str_replace_all(vec, regex(named_coder, ignore_case = TRUE))

}

std_contest2i <- function(vec, key = coder, .type = NULL) {

  if (!is.null(.type)) {
    key <- filter(key, type == .type)
  }

  named_coder <- key$code
  names(named_coder) <- key$pattern

  stri_replace_all_regex(vec,  key$pattern, key$code,
                         vectorize_all = FALSE,
                         opts_regex = stri_opts_regex(case_insensitive = TRUE))

}

microbenchmark(s2 <- mutate(samp, contest = std_contest2(race)),
               s2i <- mutate(samp, contest = std_contest2i(race)),
               times = 10)



out <- mutate(samp, contest = std_contest2(race))
count(out, contest, sort = TRUE)

std_contest_df <- function(df) {
  df_col <- df %>% mutate(
    type = case_when(
      stri_detect_regex(race, c("President"), case_insensitive = TRUE) ~ "President",
      stri_detect_regex(race,  coder$, case_insensitive = TRUE) ~ "Congress",
      stri_detect_regex(race, coder_stwide$pattern, case_insensitive = TRUE) ~ "Statewide",
      TRUE ~ "Other"
    )
  ) %>%
    as.data.table()

  setDT(df_col)
  setkey(df_col, race)



  df_col[type == "President", contest := std_contest2(race, .type = "President")]
  df_col[type == "Congress",  contest := std_contest2(race, .type = "Congress")]
  df_col[type == "Other",     contest := std_contest2(race, .type = NULL)]

}


out.split <- std_contest_df(samp)


# tatus quo
std_contest0 <- function(vec) {
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
  cor_regex <- "Coroner"
  shf_regex <- "^Sheriff"
  clr_regex <- "(County )?Clerk of Cour(t|)"

  # county
  cal_regex <- "County Council At( |-)Large"
  ccl3_regx <- "^C(CD|C0|NC(?=0)|OC(?=000))" # standardize CCD/CC0... three character  to CCL
  ccl4_regx <- "^C(CNL|NCL|OCL|OC(?=00[1-9])|YCL)" # standardize CCNL to CCL, and CC001 to CCL0
  ccl5_regx <- "(^CTYCN|^CCSCH(?=[0-9]+\\sCounty))"  # change to CCL00
  ccl6_regx <- "^CCLIST" # change to CCL000
  ccc_regex <- "County Council Chair"



  # Congress
  # treat CNG for abbeville as well
  ushou_ptrn1 <- "CON(G|G0|00|0)0"
  ushou_ptrn2 <- "U\\.?S\\.?\\sHouse of Rep(|s|\\.|resentatives)\\s+Dist(|rict)\\s+" # one way a small minority show it
  h01_regex <- suppressWarnings(glue("({ushou_ptrn1}1.*|{ushou_ptrn2}1)"))
  h02_regex <- suppressWarnings(glue("({ushou_ptrn1}2.*|{ushou_ptrn2}2)"))
  h03_regex <- suppressWarnings(glue("({ushou_ptrn1}3.*|{ushou_ptrn2}3)"))
  h04_regex <- suppressWarnings(glue("({ushou_ptrn1}4.*|{ushou_ptrn2}4)"))
  h05_regex <- suppressWarnings(glue("({ushou_ptrn1}5.*|{ushou_ptrn2}5)"))
  h06_regex <- suppressWarnings(glue("({ushou_ptrn1}6.*|{ushou_ptrn2}6)"))
  h07_regex <- suppressWarnings(glue("({ushou_ptrn1}7.*|{ushou_ptrn2}7)"))

  # US Senate
  sen_regex <- "^(U\\.?\\s?S\\.?||UNITED STATES) Senat(e|or)$"
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
    inner(cor_regex, "COR0000 Coroner") %>%
    inner(shf_regex, "SHF0000 Sheriff") %>%
    inner(ccl3_regx, "CCL") %>%
    inner(ccl4_regx, "CCL0") %>%
    inner(ccl5_regx, "CCL00") %>%
    inner(ccl6_regx, "CCL000") %>%
    inner(ccc_regex, "CCL0000 County Coucil Chair") %>%
    inner(cal_regex, "CCL0000 County Coucil at Large") %>%
    inner(clr_regex, "CLR0000 Clerk of Court") %>%
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



