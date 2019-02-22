#' Take a small sample of voters or precincts
#'
#' Custom wrapper around \code{dplyr::sample_n} and \code{dplyr::sample_frac}.
#' Instead of taking samples of rows, it takes a sample of voters defined by
#' voters (\code{elec, voter_id}) or precincts(\code{elec, precinct_id}).
#' This is Useful for long-form datasets where each row is an
#' vote for a particular office by a person.
#'
#' @param tbl the dataset that has the columns \code{elec} and \code{voter_id}
#'  (for \code{sample_nvoters} or \code{sample_fvoters}) and \code{elec} and \code{precinct_id}
#'  (for \code{sample_nprecincts} or \code{sample_fprecincts}).
#' @param n Number of voters or precincts to sample
#' @param frac Fraction of voters or precicts to sample. Alternative to \code{n}
#'
#' @export
sample_nvoters <- function(tbl, n) {
  by_voter <- distinct(tbl, elec, voter_id)
  sampled <- sample_n(by_voter, n)

  semi_join(tbl, sampled, by = c("elec", "voter_id"))
}

#' @rdname sample_nvoters
#'
#' @export
sample_fvoters <- function(tbl, frac) {
  by_voter <- distinct(tbl, elec, voter_id)
  sampled <- sample_frac(by_voter, frac)

  semi_join(tbl, sampled, by = c("elec", "voter_id"))
}

#' @rdname sample_nvoters
#'
#' @export
sample_nprecincts <- function(tbl, n) {
  by_voter <- distinct(tbl, elec, precinct_id)
  sampled <- sample_n(by_voter, n)

  semi_join(tbl, sampled, by = c("elec", "precinct_id"))
}


#' @rdname sample_nvoters
#'
#' @export
sample_fprecincts <- function(tbl, frac) {
  by_voter <- distinct(tbl, elec, precinct_id)
  sampled <- sample_frac(by_voter, frac)

  semi_join(tbl, sampled, by = c("elec", "precinct_id"))
}




#' Bind a list of dataframes, but with only a subset of variables
#'
#' Wrapper to map_dfr but specifies variables to use. This is useful when the underlying data
#' is large and has lots of variables that are not necessary.
#'
#' @param df_list A list of dataframes to bind.
#' @param regex A regular expression to
#' @param default character vector of variables (like identifiers to always keep)
#'
#' @importFrom purrr map_dfr
#' @export

slim_bind = function(df_list, regex, default = c("elec", "county", "precinct_id", "ballot_style", "voter_id")) {
  if (inherits(df_list, "tbl_df")) df_list <- list(df_list)

  if (!inherits(df_list, "list")) stop("Input must be a list of dataframes")

  map_dfr(df_list,
          function(v) select(v, !!!default, matches(regex)))
}

