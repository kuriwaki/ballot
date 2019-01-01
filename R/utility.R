#' Take a small sample of voters
#'
#' Custom wrapper around \code{dplyr::sample_n} and \code{dplyr::sample_frac}.
#' Instead of taking samples of rows, it takes a sample of voters defined by
#' \code{elec, voter_id}. Useful for long-form datasets where each row is an
#' vote for a particular office by a person.
#'
#' @param tbl the dataset that has the columns \code{elec} and \code{voter_id}
#'  (useful if a long version)
#' @param n Number of voters to sample
#' @param frac Fraction of voters to sample. Alternative to \code{n}
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
