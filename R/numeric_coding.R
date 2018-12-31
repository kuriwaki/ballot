
#' Filter down a long dataset to precinct-ballot style combinations for which
#' that contest race exists. Otherwise, observation is dropped
#' @param tbl dataset
#' @param pattern regex to identify the race. Must resolve to one variable
#' @param race tabulation of votes
#' @param na_thresh how much NAs can is still a abstention?
#' @param gtbl join with a table with more district information like county.
#'
#' @importFrom skimr inline_hist
#'
#' @export
#'
filter_existing <- function(tbl, pattern, race, na_thresh = 0.8, gtbl = NULL) {
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

  # record variable which has votes
  contest_name <- colnames(where_elec)[length(where_elec)]
  stopifnot(contest_name != "voter_id")

  # sometimes the abstentions are too much.. if so treat these as the race didn't happen
  missings <- where_elec %>%
    group_by(elec, precinct_id, ballot_style) %>%
    summarize(prop_na = mean(is.na(.data[[contest_name]]))) %>%
    ungroup()

  cat(glue("missings: {skimr::inline_hist(missings$prop_na)}, deleting where NA proportion >= {na_thresh}"), "\n")

  exist_2 <- filter(missings, prop_na < na_thresh)

  out <- semi_join(where_elec, exist_2, by = c("elec", "precinct_id", "ballot_style"))

  if (is.null(gtbl))
    return(out)

  if (!is.null(gtbl))
    inner_join(geo_wide, out)
}




#' Join with candidate data with single district identifier
#'
#' For example, used for referenda where there is only one "seat"
#' @param tbl A wide dataset with the contest
#' @param join_col NSE code to indicate variable that is being joined (to ballot_name in cands)
#' @param type NSE code for the contest. Will also be used to filter \env{cand} if not null.
#' @param ... geographic variables to segment on
#'
#'
#' @export
#'
join_1col <- function(tbl,
                      cand,
                      vote_col,
                      join_cols_tbl,
                      join_cols_cand
                      ) {
  vote_col <- enquo(vote_col)
  vote_name <- quo_name(vote_col)

  join_cols_tbl <- enquo(join_cols_tbl)
  join_name_tbl <- quo_name(join_cols_tbl)

  join_cols_cand <- enquo(join_cols_cand)
  join_name_cand <- quo_name(join_cols_cand)


  # add counts
    cand <- cand %>%
      left_join(
        count(cand, elec, !!join_cols_cand)
      ) %>%
      rename(!!join_name_tbl := !!join_name_cand,
             !!vote_name := ballot_name)

    # join relevant precinct voters and candidate
    joined <- tbl %>%
      left_join(cand, by = c("elec", join_name_tbl, vote_name)) %>% # join to candidate party
      mutate(party_num = replace(party_num, !is.na(n) & is.na(party_num), 0)) %>%
      select(elec, voter_id, !!vote_col,  !!join_cols_tbl, party_num, n)

  joined
}

#' Join by a single district identifier (congerss, state hosue) or county
#'
#' Calls \link{join_1col} internally
#'
#' @param tbl variables must be of the type '{office}_{vote}', '{office}_{dist}' (if dist) and
#' county
#' for vote name and district, respectively.
#' @param cands A table of candidates. Must have variables \code{elec}, \code{dist},
#' \code{county}, \code{ballot_name}, \code{party_num}, \code{contest_type}
#' @param office NSE short form of the office in question
#' @export

join_dist <- function(tbl, cands, office) {
  office_var <- enquo(office)
  office_name <- quo_name(office_var)

  cands_office <- filter(cands, contest_type == office_name)

  join_name <- glue("{office_name}_dist")

  vote_name <- glue("{office_name}_vote")
  vote_party_name <- glue("{office_name}_party")
  vote_ncand_name <- glue("{office_name}_ncand")


  join_1col(tbl, cands_office,
            vote_col = !!vote_name,
            join_cols_tbl = !!join_name,
            join_cols_cand = dist) %>%
    rename(!!vote_party_name := party_num,
           !!vote_ncand_name := n)
}

#' @rdname join_dist
#' @export
join_county <- function(tbl, cands, office) {
  office_var <- enquo(office)
  office_name <- quo_name(office_var)

  cands_office <- filter(cands, contest_type == office_name)

  join_name <- "county"

  vote_name <- glue("{office_name}0000")
  vote_party_name <- glue("{office_name}_party")
  vote_ncand_name <- glue("{office_name}_ncand")
  vote_name2_name <- glue("{office_name}_vote")

  join_1col(tbl, cands_office,
            vote_col = !!vote_name,
            join_cols_tbl = !!join_name,
            join_cols_cand = county) %>%
    rename(!!vote_name2_name := !!vote_name,
           !!vote_party_name := party_num,
           !!vote_ncand_name := n)
}


#' @rdname join_dist
#' @export
join_countydist <- function(tbl, cands, office) {
  office_var <- enquo(office)
  office_name <- quo_name(office_var)

  # make a new column, "ctydist", for county district combination to join on
  cands_office <- filter(cands, contest_type == office_name)
  cands_ctydist <- cands_office %>% mutate(ctydist = str_c(county, "-", dist))

  join_name <- glue("{office_name}_dist")

  vote_name <- glue("{office_name}_vote")
  vote_party_name <- glue("{office_name}_party")
  vote_ncand_name <- glue("{office_name}_ncand")

  tbl %>%
    mutate(ctydist = str_c(county, "-", .data[[join_name]])) %>%
    join_1col(cands_ctydist,
              vote_col = !!vote_name,
              join_cols_tbl = ctydist,
              join_cols_cand = ctydist) %>%
    rename(!!vote_party_name := party_num,
           !!vote_ncand_name := n)
}
