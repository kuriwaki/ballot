
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
    inner_join(geo_wide, out, by = c("elec", "precinct_id", "ballot_style", "voter_id"))
}

