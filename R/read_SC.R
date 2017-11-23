#' Wrapper from path to formatted data
#'
#' Use the step-by-step parsing  functions to generate all votes in a race
#'
#' @param paths a vector of paths of the county-directory
#'
read_format_EL155 <- function(paths) {

  # countynames
  cnames <- gsub("[0-9A-z/_]+/([A-z]+)", "\\1", paths)

  all_race <- foreach(i = 1:length(paths), .combine = "bind_rows") %do% {

    rows <- read_EL155(path = glue("{paths[i]}/EL155"), cname = cnames[i])

    pkey <- get_precinct_range(rows)
    wprecinct <- add_precinct(rows, pkey)
    rm(rows)

    parsed_list <- parse_EL155(votes = wprecinct)
    parsed_df <- list_to_df(parsed_list, wprecinct)
    wvoter <- identify_voter(parsed_df)

    wvoter
  }

  all_race
}

#' Read a EL155 file
#'
#' @param path path to raw file
#' @param cname county name
#'
#'
#' @examples
#' allendale <- read_EL155("build/input/SC_2010Gen/Allendale/EL155", "Allendale")
#'
read_EL155 <- function(path = "build/input/SC_2010Gen/Allendale/EL155",
                       cname = "Allendale") {

  raw <-  tibble(text = read_lines(path)) %>%
    mutate(id = 1:n(),
           county = cname) %>%
    select(id, county, text)



  # weird encoding
  valid <- raw %>% filter(!grepl("^\033", text)) %>% # beginnign
    mutate(text = str_replace(text, pattern = "<a0> ", " ")) # after Reeves
  nrow(valid)

  nonempty <- valid %>% filter(str_length(text) > 0)
  nrow(nonempty)


  # extract info before dropping metadata
  eID <- valid %>% filter(grepl("RUN DATE", text)) %>% distinct(text) %>%
    mutate(election_ID = gsub(".*ID: ([0-9A-Z]+)$", "\\1", text)) %>%
    distinct(election_ID) %>%
    unlist()


  n_precincts <- nonempty %>% filter(grepl("CAND VOTES", text)) %>% nrow()
  cat(glue("{cname} code {eID} - with {n_precincts} precincts; "))

  df <- nonempty %>%
    filter(!grepl("^[\\s\\d]+$", text, perl = TRUE)) %>%
    filter(!grepl("REPORT-EL155\\s+PAGE\\s+[0-9+]", text, perl = TRUE)) %>% # Dorchester
    filter(!grepl("^\\s+General Election\\s+[0-9+]", text)) %>% # in Marlbolo, precinct footer
    filter(!grepl("^\\s+[A-z]+\\sCounty\\s*$", text)) %>% # Fairfield and Jasper County
    filter(!grepl("^\\s+test\\s*$", text)) %>% # Beaufort footer
    filter(!grepl("CAND VOTES", text)) %>%
    filter(!grepl("PRECINCT TOTALS", text))

  df
}

#' Extract precinct-identifying information
#'
#' Get a precinct-level dataset by using dplyr::distinct(.keep_all = TRUE).
#' Thus the rows need to be ordered correctly.
#'
#' @param df a EL155 dataset, read through read_EL155
#'
#' @return A dataset keyed by precinct. start_id and end_id refer to the range of
#' the precinct in terms of the row IDs in read_EL155 output. column `p_name` is
#' is the name portion of the precinct header. `precinct_id` is the ID of
#' precinct within the county
#'
#'
#' @examples
#' allendale <- read_EL155("build/input/SC_2010Gen/Allendale/EL155", "Allendale")
#' ald_p <- get_precinct_range(allendale)
#' wprecinct <- add_precinct(allendale, ald_p)
get_precinct_range <- function(df) {
  pfirst <- df %>%
    filter(grepl("RUN DATE", text, perl = TRUE)) %>%
    distinct(text, .keep_all = TRUE)

  pfirst_range <- pfirst %>%
    mutate(p_name = gsub(".*(?=PRECINCT)", "", text, perl = TRUE),
           p_name = gsub("\\s+ELECTION ID: [0-9A-Z]+", "", p_name, perl = TRUE),
           p_name = gsub("\\s+", " ", p_name, perl = TRUE)) %>%
    mutate(precinct_id = 1:n(),
           p_start_id = id + 1,
           p_end_id = lead(id, 1) - 1,
           text = NULL,
           id = NULL)

  pfirst_range$p_end_id[nrow(pfirst_range)] <- max(df$id)

  pfirst_range
}



#' Add precinct to each row of vote dataset, and remove other headers
#'
#' Use the non-equi join in data.table to identify precinct. row i is in
#' precinct j if i's ID is in between p_Start_id and p_end_id of precint j.
#'
#' @param votes EL155 dataset of votes, product of read_EL155()
#' @param pkey precinct key, product of get_precinct_range()
#'
#' @import data.table
#'
#' @examples
#' allendale <- read_EL155("build/input/SC_2010Gen/Allendale/EL155", "Allendale")
#' ald_p <- get_precinct_range(allendale)
#' wprecinct <- add_precinct(allendale, ald_p)
add_precinct <- function(votes, pkey) {

  # do a join-by-range with data.table
  require(data.table)
  setDT(votes)
  setDT(pkey)

  wprecinct <- pkey[votes,
                    on = .(p_start_id <= id,
                           p_end_id >= id,
                           county = county),
                    nomatch = NA]  # all rows in votes
  wprecinct <- tbl_df(wprecinct) %>%
    select(-p_start_id, -p_end_id)
  stopifnot(nrow(votes) == nrow(wprecinct))

  # now remove the headers -- since we now have precinct.
  # once we have precinct and asterisk, rows are identifiable
  wprecinct <-  wprecinct %>%
    filter(!grepl("RUN DATE.*ELECTION ID: [0-9+]", text)) %>%
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
#' @param votes data frame with row
#' @param vote_col column name (in characters) where the votes are
#' @param start_pos numerical vector of start char positions
#' @param end_pos numerical vector of end char positions
#'
#' @import stringr purrr
#'
#' allendale <- read_EL155("build/input/SC_2010Gen/Allendale/EL155", "Allendale")
#' ald_p <- get_precinct_range(allendale)
#' wprecinct <- add_precinct(allendale, ald_p)
#' lst <- parse_EL155(wprecinct)
#'
parse_EL155 <- function(votes, vote_col = "text",
                        start_pos = c(1,  9, 14, 16, 21, 61),
                        end_pos =   c(7, 12, 14, 19, 59, 131)) {
  map(votes[[vote_col]], ~ str_sub(., start_pos, end_pos))
}


#' Convert parsed list to data frame
#'
#' @param lst list which is a product of parse_EL155
#' @param votes dataset of votes to be combined to parsed list. Must be in right ordersame order
#'
#' @import tibble purrr
#'
#' allendale <- read_EL155("build/input/SC_2010Gen/Allendale/EL155", "Allendale")
#' ald_p <- get_precinct_range(allendale)
#' wprecinct <- add_precinct(allendale, ald_p)
#' lst <- parse_EL155(wprecinct)
#' df <- list_to_df(lst, wprecinct)

list_to_df <- function(lst, votes) {

  tbl <- tibble(marker = map_chr(lst, 3),
                cand_id = map_chr(lst, 4),
                cand_name = map_chr(lst, 5),
                race = map_chr(lst, 6))

  # trim
  tbl <- mutate_all(tbl, str_trim)

  # change marker to numeric
  tbl <- tbl %>%
    mutate(voter_top = as.numeric(marker == "*")) %>%
    select(-marker)

  bind_cols(select(votes, -text),
            tbl)
}


#' assign individual voter ID by reading the asterisk mark
#'
#' Use cumsum to find jumps in the asterisk
#'
#' @import dplyr
#'
#' @param df a data frame where each row is a birth. The column `voter_top`
#' is 1 if the entry is the first vote on the ballot and 0 otherwise.
identify_voter <- function(df) {
  df %>%
    mutate(voter_id = cumsum(voter_top)) %>%
    select(-voter_top) %>%
    select(county, voter_id, everything())
}


#' Add unique ID for each voter in a race.
#'
#' This implies adding county and voter_id within county
#'
#' @param df A dataset with county and voter ID
#' @param state state
#'
#' @import maps purrr
#'
add_unique_id <- function(df, state = "SC", year = "2010") {
  require(maps)
  data(county.fips)

  max_d_v <- str_length(as.character(max(df$voter_id)))
  max_d_p <- str_length(as.character(max(df$precinct_id)))

  state_fips <- county.fips %>%
    filter(grepl("south carolina", polyname)) %>%
    mutate(county = str_to_title(gsub("south carolina,", "", polyname))) %>%
    select(fips, county)

  left_join(df, state_fips, by = c("county")) %>%
    rename(id_within_county = voter_id) %>%
    mutate(voter_id = paste0(fips, "-", str_pad(as.character(id_within_county), max_d_v, pad = "0"))) %>%
    mutate(precinct_id = paste0(fips, "-", str_pad(as.character(precinct_id), max_d_p, pad = "0"))) %>%
    select(-id_within_county) %>%
    select(voter_id, county, voter_id, precinct_id, everything())
}
