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
