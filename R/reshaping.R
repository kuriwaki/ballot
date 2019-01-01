
#' long voter-vote to voter-based data
#'
#' This drops offices where multiple votes are cast for the same contest_code. This
#' is necessary to make a voter uniquely identify a vote for a contest (column).
#' Some offices like congress and state leg are separated out into district-name pairs,
#'  with a specific column for a district and the vote in that corresponding district.
#' Absentees are also dropped because they do not have precint information.
#'
#' @param df A standardized long dataset with contest_code and contest_name. Should
#' have the variables \code{elec}, \code{county}, \code{contest_code}, \code{voter_id},,
#' \code{precinct}, \code{precinct_id}.
#' @param contests A vector of contest_codes to select and put into the wide.
#'
#' @return  A dataset at the individual voter level. For some offices with a district,
#' these are collapsed to a pair-district pairing. If a voter abstained the
#' _vote  variable is \code{NA} (indeed it was not in the long dataset), but
#' we try to impute the district by the voter's precinct and ballot style.
#'
#' @importFrom data.table as.data.table dcast melt
#'
#' @export
cast_to_wide <- function(df = raw,
                         contests = c("PTY", "PRS", "USHOU", "USSEN", "GOV", "LGV", "SOS", "ATG", "SSI",
                                      "HOU", "SEN", "JPRB", "SHF", "COR", "CLR", "AUD",
                                      "CTRES", "CCL","CCD", "CCA", "CCC", "SCH", "JPRB", "WAT", "Q", "LRCA")) {


  # slow
  office_votes <- count(df, elec, county, contest_code, voter_id) %>%
    group_by(elec, county, contest_code) %>%
    summarize(max_votes = max(n)) %>%
    ungroup() %>%
    arrange(-max_votes)

  # votes for code, by precinct. since every record in raw is a vote, length captures the number of votes

  # office with one vote
  df_one <- semi_join(df, filter(office_votes, max_votes == 1),
                      by = c("elec", "county", "contest_code"))

  # Non-absentee precinct
  p_absentee <- df %>%
    filter(str_detect(precinct, regex("(Absentee|Failsafe)", ignore_case = TRUE))) %>%
    distinct(elec, precinct_id)

  # select offices and non-absentee
  select_offices <- filter(df_one, contest_type %in% contests) %>%
    anti_join(p_absentee, by = c("elec", "precinct_id"))

  # separate columns for district
  dist_offices <- c("HOU", "SEN", "USHOU", "SOL", "CCD")

  # dist num in one office, candidates in another. For SMD
  dt_fmt <- select_offices %>%
    filter(contest_type %in% dist_offices) %>%
    mutate(dist = as.numeric(parse_number(contest_code))) %>% # DISTRICT
    rename(vote = choice_name) %>%
    as.data.table()

  dist_wide <- dt_fmt %>%
    dcast(elec + county + precinct_id + precinct + ballot_style + voter_id ~ contest_type, value.var = c("vote", "dist")) %>%
    rename_at(vars(matches("vote_")), function(x) str_c(str_remove(x, "vote_"), "_vote")) %>%
    rename_at(vars(matches("dist_")), function(x) str_c(str_remove(x, "dist_"), "_dist")) %>%
    tbl_df()

  # impute dist when the vote (therefore the indicator of district) is missing
  dists_long <- dist_wide %>%
    as.data.table() %>%
    melt(id.vars = c("elec", "precinct_id", "ballot_style"),
         measure.vars = patterns("_dist$"),
         na.rm = TRUE,
         value.name = "dist",
         variable.name = "office") %>% # ignore abstentions
    distinct()

  # check if each precinct - district number is unique
  stopifnot(nrow(distinct(dists_long)) == nrow(dists_long))
  district_table <- dists_long %>%
    dcast(elec + precinct_id + ballot_style ~ office, value.var = "dist")

  # join and coalesce
  dist_wide_add <- left_join(dist_wide, district_table,
            by = c("elec", "precinct_id", "ballot_style"),
            suffix = c(".a", ".b"))

  # a lot of code just to coalesce any districts in any office that had a missing
  # due to abstension
  newdists <- dist_wide_add %>%
    as.data.table() %>%
    melt(id.vars = c("elec", "voter_id"),
         measure = patterns("\\.(a|b)"),
         na.rm = TRUE) %>%
    mutate(type = str_extract(variable, ".*(?=\\.(a|b))"),
           dist = str_extract(variable, "(a|b)$")) %>%
    dcast(elec + voter_id + type ~ dist, value.var = "value") %>%
    mutate(dist = coalesce(a, b)) %>%
    dcast(elec + voter_id ~ type, value.var = "dist") %>%
    tbl_df()

  # merge in new districts (and drop old ones)
  dist_wide_imp <- dist_wide_add %>%
    select(-matches("\\.(a|b)")) %>%
    left_join(newdists, by = c("elec", "voter_id"))

  # other offices (use the 7-digit code on its own)
  df_wide_oth <- select_offices %>%
    filter(!contest_type %in% dist_offices) %>%
    as.data.table() %>%
    dcast(elec + voter_id ~ contest_code, value.var = "choice_name") %>%
    tbl_df()

  left_join(dist_wide_imp, df_wide_oth, by = c("elec", "voter_id"))
}


# https://stackoverflow.com/questions/49075824/using-tidy-eval-for-multiple-dplyr-filter-conditions
pair_coalesce <- function(df, cols){

  fp <- map(cols,
            function(x) quo(!!(as.name(x)) := coalesce(.data[[str_c(x, ".a")]], .data[[str_c(x, ".b")]]))
  )
  mutate(df, !!!fp)
}
