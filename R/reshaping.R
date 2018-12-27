
#' long voter-vote to voter-based data
#'
#' This drops offices where multiple votes are cast for the same contest_code. Absentees are also dropped.
#'
#' @param raw A standardized long dataset with contest_code and contest_name
#' @param contests A vector of contest_codes to select and put into the wide.
#'
#' @export

cast_to_wide <- function(df = raw,
                         contests = c("PTY", "PRS", "USHOU", "USSEN", "GOV", "LGV", "SOS", "ATG", "SSI",
                                      "HOU", "SEN", "JPRB", "SHF", "COR", "CLR", "AUD",
                                      "CTRES", "CCL", "SCH", "JPRB", "WAT", "Q", "LRCA")) {

  # slow
  office_votes <- count(df, elec, county, contest_code, voter_id) %>%
    group_by(elec, county, contest_code) %>%
    summarize(max_votes = max(n)) %>%
    ungroup() %>%
    arrange(-max_votes)
  cat("Number of votes a person can vote for in a county\n")

  # votes for code, by precinct. since every record in raw is a vote, length captures the number of votes

  # office with one vote
  df_one <- semi_join(df, filter(office_votes, max_votes == 1),
                      by = c("elec", "county", "contest_code"))

  # Non-absentee precinct
  p_absentee <- df %>%
    filter(str_detect(p_name, regex("(Absentee|Failsafe)", ignore_case = TRUE))) %>%
    distinct(precinct_id)


  # select offices and non-absentee
  select_offices <- filter(df_one, contest_type %in% contests) %>%
    anti_join(p_absentee)

  # separate columns for district
  dist_offices <- c("HOU", "SEN", "USHOU", "SOL")

  dist_wide <- select_offices %>%
    filter(contest_type %in% dist_offices) %>%
    mutate(dist = as.integer(parse_number(contest_code))) %>%
    rename(vote = choice_name) %>%
    as.data.table() %>%
    dcast(elec + voter_id ~ contest_type, value.var = c("vote", "dist")) %>%
    rename_at(vars(matches("vote_")), function(x) str_c(str_remove(x, "vote_"), "_vote")) %>%
    rename_at(vars(matches("dist_")), function(x) str_c(str_remove(x, "dist_"), "_dist")) %>%
    tbl_df()


  df_wide_oth <- select_offices %>%
    filter(!contest_type %in% dist_offices) %>%
    as.data.table() %>%
    dcast(elec + county + precinct_id + p_name + ballot_style + voter_id ~ contest_code, value.var = "choice_name") %>%
    tbl_df()

  left_join(df_wide_oth, dist_wide, by = c("elec", "voter_id"))
}
