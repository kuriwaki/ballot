#' Wrapper from path to formatted data
#'
#' Use the step-by-step parsing  functions to generate all votes in a contest
#'
#' @param paths a vector of paths of the county-directory
#'
#' @importFrom foreach foreach
#'
read_format_EL155 <- function(paths) {

  # countynames
  cnames <- gsub(".*/([A-z]+)$", "\\1", paths)

  all_contest <-
    foreach(
      i = 1:length(paths),
      .inorder = TRUE,
      .combine = "bind_rows",
      .packages = c("ballot", "dplyr")
    ) %do% {
      el155name <- grep("EL155", list.files(paths[i]), value = TRUE, ignore.case = TRUE)

      # rarely there are more than one EL155 file
      rows <- foreach(elpath = glue("{paths[i]}/{el155name}"), .combine = "bind_rows") %do% {
        read_EL155(path = elpath, cname = cnames[i])
      }

      # if two, re-index
      if (rows$id[nrow(rows)] != nrow(rows)) rows <- mutate(rows, id = 1:n()) # re-index if composite


      pkey <- get_precinct(rows)
      wprecinct <- add_precinct(rows, pkey)

      parsed_df <- parse_EL155(votes = wprecinct)
      wvoter <- identify_voter(parsed_df)


      wIDs <- add_id(wvoter)
      rm(wvoter, parsed_df, rows)
      gc()

      wIDs
    }

  all_contest
}

#' Read a EL155 file
#'
#' @param path path to raw file
#' @param cname county name
#' @param precinct_regex regex that identifies precinct (a data row)
#'
#' @export
#'
#' @examples
#' allendale <- read_EL155("build/input/SC_2010Gen/Allendale/EL155", "Allendale")
read_EL155 <- function(path = "build/input/SC_2010Gen/Allendale/EL155",
                       cname,
                       precinct_regex = "(RUN DATE|ELECTION ID:)") {
  raw <- tibble(text = suppressWarnings(read_lines(path))) %>%
    mutate(
      id = 1:n(),
      county = cname
    ) %>%
    select(id, county, text)

  # weird encoding
  valid <- raw %>%
    filter(!grepl("^\033", text)) %>% # beginnign
    mutate(text = str_replace(text, pattern = "<a0> ", " ")) # after Reeves
  nrow(valid)

  nonempty <- valid %>% filter(str_length(text) > 0)

  # extract info before dropping metadata
  eID <- valid %>%
    filter(str_detect(text, "(RUN DATE|ELECTION ID:)")) %>%
    distinct(text) %>%
    mutate(election_ID = gsub(".*ID: ([0-9A-Z]+)$", "\\1", text)) %>%
    distinct(election_ID) %>%
    unlist()


  n_precincts <- nonempty %>% filter(str_detect(text, "CAND VOTES")) %>% nrow()
  cat(glue::glue("{cname} (code {eID}) with {n_precincts} precincts, "), "\n")

  nonempty %>%
    filter(str_detect(text, "^[0-9]{7}\\s+") | str_detect(text, precinct_regex))
}

#' Extract precinct-identifying information
#'
#' Get a precinct-level dataset by using dplyr::distinct(.keep_all = TRUE).
#' Thus the rows need to be ordered correctly.
#'
#' @param df a EL155 dataset, read through read_EL155
#' @param precinct_regex regex to identify rows with precinct names. For
#'  2018-11-06, use \env{"PRECINCT (?!TOTAL)"}. Default should work in ohter cases.
#'
#' @return A dataset keyed by precinct. start_id and end_id refer to the range of
#' the precinct in terms of the row IDs in read_EL155 output. column `precinct` is
#' is the name portion of the precinct header. `precinct_id` is the ID of
#' precinct within the county
#'
#' @export
#'
#' @examples
#' data(G2016_Dillon_EL155)
#' dillon_p <- get_precinct(G2016_Dillon_EL155)
#' wprecinct <- add_precinct(G2016_Dillon_EL155, dillon_p)
#'
get_precinct <- function(df, precinct_regex = "(RUN DATE|ELECTION ID:)") {
  pfirst <- df %>%
    filter(grepl(precinct_regex, text, perl = TRUE)) %>%
    distinct(text, .keep_all = TRUE)

  pfirst_range <- pfirst %>%
    mutate(
      precinct = gsub(".*(?=PRECINCT)", "", text, perl = TRUE),
      precinct = gsub("\\s+ELECTION ID: [0-9A-Z]+", "", precinct, perl = TRUE),
      precinct = str_squish(precinct)
    ) %>%
    mutate(
      precinct_id = 1:n(),
      p_start_id = id + 1,
      p_end_id = lead(id, 1) - 1,
      text = NULL,
      id = NULL
    )

  pfirst_range$p_end_id[nrow(pfirst_range)] <- max(df$id)

  pfirst_range
}



#' Add precinct to each row of vote dataset, and remove other headers
#'
#' Use the non-equi join in data.table to identify precinct. row i is in
#' precinct j if i's ID is in between p_start_id and p_end_id of precint j.
#'
#' @param votes EL155 dataset of votes, product of read_EL155()
#' @param pkey precinct key, product of get_precinct_range()
#'
#' @importFrom fuzzyjoin interval_inner_join
#'
#' @export
#'
#' @examples
#' data(G2016_Dillon_EL155)
#' dillon_p <- get_precinct(G2016_Dillon_EL155)
#' wprecinct <- add_precinct(G2016_Dillon_EL155, dillon_p)
#'
add_precinct <- function(votes, pkey) {
  stopifnot(n_distinct(votes$county) == 1)

  pkey_append <- pkey %>%
    select(precinct, precinct_id, p_start_id, p_end_id) %>%
    tbl_df() %>%
    mutate_if(is.double, as.integer)

  # trivial interval that precinct should match
  votes <- votes %>%
    mutate(
      p_start_id = id,
      p_end_id = id
    )

  wprecinct <- interval_inner_join(votes, pkey_append, by = c("p_start_id", "p_end_id")) %>%
    select(-matches("p_(start|end)_id")) # no longer needed

  # now remove the headers -- since we now have precinct.
  # once we have precinct and asterisk, rows are identifiable
  wprecinct <- wprecinct %>%
    filter(!grepl(".*ELECTION ID: [0-9+]", text)) %>%
    filter(!grepl("\\s+PRECINCT", text)) %>%
    filter(!grepl("PRECINCT TOTALS", text)) %>%
    filter(!grepl("CANDIDATES RECEIVING A VOTE", text))

  wprecinct
}


#' separate vote rows in to columns
#'
#' Use fwf to separate out colums in vote rows. Entries need to be votes, thus
#' they need to be a product of get_precinct_range()
#' Data looks like
#' \code{5123906    2 *   10 Nikki R Haley                           GOVERNOR}
#'
#' @export
#' @param votes data frame with row
#' @param vote_col column name (in characters) where the votes are
#' @param start_pos numerical vector of start char positions
#' @param end_pos numerical vector of end char positions
#' @param col_names names of variables for each column
#'
#' @import stringr purrr readr
#'
#' @examples
#' data(G2016_Dillon_EL155)
#' dillon_p <- get_precinct(G2016_Dillon_EL155)
#' wprecinct <- add_precinct(G2016_Dillon_EL155, dillon_p)
#' df <- parse_EL155(wprecinct)
parse_EL155 <-
  function(votes, vote_col = "text",
             start_pos = c(1, 9, 14, 16, 21, 61),
             end_pos = c(7, 12, 14, 19, 59, 131),
             col_names = c("machine", "ballot_style", "marker", "choice_id", "choice_name", "contest_name")) {
    covariates <- select(votes, -one_of(vote_col))

    concat <- str_c(votes[[vote_col]], collapse = "\n")
    fwf <- suppressWarnings(read_fwf(concat, col_positions = fwf_positions(start_pos, end_pos, col_names)))

    bind_cols(covariates, fwf) %>%
      mutate(machine = as.character(machine)) %>%
      mutate(voter_top = coalesce(as.integer(marker == "*"), 0L)) %>%
      select(-marker)
  }



#' assign individual voter ID by reading the asterisk mark
#'
#' Use cumsum to find jumps in the asterisk
#'
#' @import dplyr
#' @export
#'
#' @param df a data frame where each row is a birth. The column `voter_top`
#' is 1 if the entry is the first vote on the ballot and 0 otherwise.
identify_voter <- function(df) {
  df %>%
    mutate(voter_id = cumsum(voter_top)) %>%
    select(-voter_top) %>%
    select(county, voter_id, everything())
}


#' Add unique ID for each voter in a contest.
#'
#' This implies adding county and voter_id within county
#'
#' @param df A dataset with county and voter ID
#' @param st state
#'
#' @import noncensus purrr
#' @export
#'
add_id <- function(df, st = "SC") {
  data("counties")


  max_d_v <- str_length(as.character(max(df$voter_id, na.rm = TRUE)))
  max_d_p <- str_length(as.character(max(df$precinct_id, na.rm = TRUE)))

  state_fips <- counties %>%
    mutate(state = as.character(state)) %>% # not factor
    filter(state == st) %>%
    mutate(county = str_to_title(gsub(" County", "", county_name))) %>%
    mutate(fips = paste0(state_fips, county_fips)) %>%
    select(st = state, fips, county)

  left_join(df, state_fips, by = c("county")) %>%
    rename(id_within_county = voter_id) %>%
    mutate(voter_id = paste0(fips, "-", str_pad(as.character(id_within_county), max_d_v, pad = "0"))) %>%
    mutate(precinct_id = paste0(fips, "-", str_pad(as.character(precinct_id), max_d_p, pad = "0"))) %>%
    select(-id_within_county) %>%
    select(st, county, fips, voter_id, precinct_id, everything())
}
