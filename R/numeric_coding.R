
#' Filter down a long dataset to precinct-ballot style combinations for which
#' that contest race exists. Otherwise, observation is dropped
#' @param tbl dataset
#' @param pattern regex to identify the race. Must resolve to one variable
#' @param race tabulation of votes
filter_existing <- function(tbl, pattern, race, na_thresh = 0.8) {
  race_existence <- race %>%
    as.data.table() %>%
    melt.data.table(
      id.vars = c("elec", "precinct_id", "ballot_style"),
      measure.vars = patterns(pattern),
      variable.name = "contest_code"
    ) %>%
    filter(value > 0)

  # should delete most irrelevant races
  where_elec <- semi_join(tbl, race_existence, by = c("elec", "precinct_id", "ballot_style")) %>%
    select(elec, precinct_id, ballot_style, voter_id, matches(pattern))

  contest_name <- colnames(where_elec)[length(where_elec)]

  # sometimes the abstentions are too much.. if so treat these as the race didn't happen
  missings <- where_elec %>%
    group_by(elec, precinct_id, ballot_style) %>%
    summarize(prop_na = mean(is.na(.data[[contest_name]]))) %>%
    ungroup()

  cat(glue("missings: {inline_hist(missings$prop_na)}, deleting where prop_na >= 0.8"), "\n")

  exist_2 <- filter(missings, prop_na < na_thresh)

  semi_join(where_elec, exist_2, by = c("elec", "precinct_id", "ballot_style"))
}




#' Join with candidate data with single district identifier
#'
#' For example, used for referenda where there is only one "seat"
#' @param tbl A wide dataset with the contest
#' @param pattern regex to mark that race
#' @param type NSE code for the contest. Will also be used to filter \env{cand} if not null.
#' @param p_race A separate dataset that indicates the number of votes cast for
#' a contest code in a particular geography
#' @param cand A key that matches candidate names to parties
#' @param ... geographic variables to segment on
#'
#'
#' @export
#'
join_1col <- function(tbl,
                      pattern,
                      cand,
                      new_name,
                      ...) {
  choice_col <- quos(...)
  new_var <- enquo(new_name)
  new_name <- quo_name(new_var)
  nchoice_name <- glue("{new_name}_nchoice")

  # if this is a candidate race, like sherrif
    cand <- filter(cand, contest_type == new_name) # only relevant type
    cand_counts <- count(cand, elec, !!!choice_col) %>%
      rename(!!nchoice_name := n)

    # join relevant precinct voters and candidate
    joined <- tbl %>%
      left_join(cand) %>%
      left_join(cand_counts, by = setdiff(colnames(cand_counts), nchoice_name)) %>%
      mutate(party_num = replace(party_num, !is.na(.data[[nchoice_name]]) & is.na(party_num), 0)) %>%
      select(elec, voter_id,  !!!choice_col, !!new_name := party_num, !!nchoice_name)

  joined
}


#' Melt when one county has more than one race
#'
#' @param tbl data frame with votes for an office like US House
#' @param pattern regex of vote columns to deal with
#' @param distname character vector that indicates the district number indicator
#' @param idvars a series of variables that is the unit for \env{na_thresh}
#' @param na_thresh if the proportion of NAs in a combination of \env{idvars} is more than \env{na_thresh}, then
#' we say that this precinct did not have that member running
#'
#'
#' @export
melt_office <- function(tbl = df_wide,
                        pattern,
                        race = p_race,
                        distname,
                        idvars = c("elec", "voter_id", "precinct_id", "ballot_style"),
                        na_thresh = 0.80) {

  # should delete most irrelevant races
  where_elec <- semi_join(long, race_existence)

  cast_form <- glue("elec + precinct_id + ballot_style + {distname} ~ ballot_name")

  # sometimes the abstentions are too much.. if so treat these as the race didn't happen
  v_by_p <- dcast(where_elec,
    as.formula(cast_form),
    value.var = "ballot_name",
    fun.aggregate = length
  ) %>%
    mutate(prop_NA = `NA` / rowSums(select(., -elec, -precinct_id, -ballot_style, -matches(distname))))

  where_elec <- semi_join(where_elec, filter(v_by_p, prop_NA < na_thresh))


  # find unique if precinct info is indetermine
  voter_unq <- where_elec %>%
    arrange(voter_id, ballot_name) %>% # take the name -- should be only one name per voter if any
    distinct(voter_id, .keep_all = TRUE)

  voter_unq %>%
    mutate(!!distname := as.numeric(str_extract(.data[[distname]], "[0-9]+"))) %>%
    mutate(ballot_name = str_replace(ballot_name, "\\s+(Jr|Sr)$", "")) %>%
    tbl_df()
}

#' Join and add number of candidates
#'
#' @param long a long dataset
#' @param cand dataframe of candidates
#' @param new_name a symbol to use for the new dataset
#'
#'
#' @export
#'
join_office <- function(long, cand, new_name, ...) {
  cand_col <- quos(...)
  new_name <- enquo(new_name)
  new_name <- quo_name(new_name)
  ncands_name <- glue("{new_name}_ncands")
  cname_name <- glue("{new_name}_name")

  cand_party <- select(cand, elec, !!!cand_col, ballot_name, party_num) # given names

  dist_count <- count(cand, elec, !!!cand_col) %>%
    rename(num_in_dist = n)

  joined <- long %>%
    left_join(cand_party) %>%
    left_join(dist_count) %>%
    mutate(party_num = replace(party_num, is.na(party_num), 0)) %>%
    select(
      elec, voter_id,
      !!!cand_col,
      !!new_name := party_num,
      !!ncands_name := num_in_dist,
      !!cname_name := ballot_name
    )

  joined
}
