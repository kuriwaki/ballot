
#' Filter down a long dataset to precinct-ballot style combinations for which
#' that contest race exists. Otherwise, observation is dropped
#' @param tbl dataset
#' @param pattern regex to identify the race. Must resolve to one variable
#' @param race tabulation of votes
#' @param na_thresh how much NAs can is still a abstention?
#' @param gtbl join with a table with more district information like county.
#'
#' @importFrom skimr inline_hist
#' @importFrom tidyr pivot_longer
#'
#' @export
#'
filter_existing <- function(tbl, pattern, race, na_thresh = 0.8, gtbl = NULL) {
  race_existence <- race %>%
    select(elec, precinct_id, ballot_style, matches(pattern)) %>%
    pivot_longer(
      -c(elec, precinct_id, ballot_style),
      names_to = "contest_code"
    ) %>%
    filter(value > 0)

  # should delete most irrelevant races
  mrg_on <- c("elec", "precinct_id", "ballot_style")
  where_elec <- semi_join(tbl, race_existence, by = mrg_on) %>%
    select(!!!mrg_on, voter_id, matches(pattern))

  # record variable which has votes
  contest_name <- colnames(where_elec)[length(where_elec)]
  stopifnot(length(where_elec) == length(mrg_on) + 1 + 1) # input data should have it as one column
  stopifnot(contest_name != "voter_id")

  # sometimes the abstentions are too much.. if so treat these as the race didn't happen
  missings <- where_elec %>%
    group_by(elec, precinct_id, ballot_style) %>%
    summarize(prop_na = mean(is.na(.data[[contest_name]]))) %>%
    ungroup()

  nmiss <- nrow(filter(missings, prop_na >= na_thresh))

  cat(glue("missings: {skimr::inline_hist(missings$prop_na)}, deleting {nmiss} ballot-styles where NA proportion >= {na_thresh}"), "\n")

  exist_2 <- filter(missings, prop_na < na_thresh)

  out <- semi_join(where_elec, exist_2, by = c("elec", "precinct_id", "ballot_style"))

  if (is.null(gtbl))
    return(out)

  if (!is.null(gtbl))
    inner_join(gtbl, out, by = c("elec", "precinct_id", "ballot_style", "voter_id"))
}

