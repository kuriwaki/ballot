#' Standardize race code
#'
#'
#' @export
std_racecode <- function(vec) {
  prs_regex <- "President( And |/)Vice President"

  gov_regex <- "^Governor"
  atg_regex <- "^Attorney General"
  sos_regex <- "Secretary of State"
  sad_regex <- "^Auditor"
  cad_regex <- "County Auditor"
  adj_regex <- "^Adjutant General"
  str_regex <- "^State Treasurer"
  ctr_regex <- "^County Treasurer"
  cmp_regex <- "^Comptroller General"
  ltg_regex <- "^(Lieutenant|Lt) Governor"
  pty_regex <- "Straight Party"
  cor_regex <- "Coroner"
  ssi_regex <- "^State Superintendent"
  wat_regex <- "Soil and Water.* Commission"

  sen_regex <- "U(\\.|\\s|)S\\.? Senat(e|or)$"
  sn2_regex <- "U(\\.|\\s|)S\\.? Senat(e|or) \\(Unexpired Term\\)"


  inner <- function(input, regex_str, replacement, use_num = FALSE) {
    if (use_num) {
    }

    str_replace(input, regex(regex_str, ignore_case = TRUE), replacement)
  }

  vec %>%
    inner(prs_regex, "PRS0000 President")  %>%
    inner(gov_regex, "GOV0000 Governor")  %>%
    inner(sos_regex, "SOS0000 Secretary of State") %>%
    inner(sad_regex, "AUD0000 Auditor") %>%
    inner(cad_regex, "CAUD000 County Auditor") %>%
    inner(atg_regex, "ATG0000 Attorney General") %>%
    inner(adj_regex, "ADJ0000 Adjutant General") %>%
    inner(str_regex, "STRES00 State Treasurer") %>%
    inner(ctr_regex, "CTRES00 County Treasurer") %>%
    inner(cmp_regex, "CMP0000 Comptroller General") %>%
    inner(ltg_regex, "LGV0000 Lieutenant Governor") %>%
    inner(pty_regex, "PTY0000 Straight Party") %>%
    inner(cor_regex, "COR0000 Coroner") %>%
    inner(ssi_regex, "SSI0000 State Superintendent of Education") %>%
    inner(wat_regex, "WAT0000 Soil and Water District Commission") %>%
    inner(sen_regex, "USSEN01 US Senator") %>%
    inner(sn2_regex, "USSEN02 US Senator (Special)")
}

std_racecode(samp) %>% sample() %>% head(n = 20) %>% sort()

df2 <- readRDS("~/Dropbox/local-representation/build/output/fmt_long/2012-11-06_formatted.Rds")
samp <- sample_n(df2, 5000) %>% pull(race)


#' Standardize race name
#'
#' Make all sentence case, with exception of "U.S."
#'
#' @param df
#'
#' @export
#'
std_race <- function(df) {
  df %>%
    mutate(race = str_to_title(race),
           race = gsub("U\\.s\\.", "U.S.", race),  # capitalize
           race = gsub("U\\.\\sS\\.", "U.S.", race),  # remove space
           race = gsub("U\\.S\\. Senator", "U.S. Senate", race), # Senate
           race = gsub("Us Senate", "U.S. Senate", race),
           race = gsub("Us Senator", "U.S. Senate", race),
           race = gsub("President And Vice President", "President", race), # Unify
           race = gsub("President/Vice President", "President", race),
           race = gsub("Lt Governor", "Lieutenant Governor", race))

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
