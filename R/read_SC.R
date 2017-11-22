

#' Read a EL155 file
#'
#' @param path path to raw file
#' @param cname county name
#'
read_EL155 <- function(path = "build/input/SC_2010/Allendale/EL155",
                       cname = "Allendale") {

  raw <-  tibble(text = read_lines(path)) %>%
    mutate(id = 1:n(),
           county = cname) %>%
    select(id, county, text)

  cat(glue("read county {cname}; "))


  # weird encoding
  valid <- raw %>% filter(!grepl("^\033", text)) %>% # beginnign
    mutate(text = str_replace(text, pattern = "<a0> ", " ")) # after Reeves
  nrow(valid)

  nonempty <- valid %>% filter(str_length(text) > 0)
  nrow(nonempty)


  # extract info before dropping metadata
  eID <- valid %>% filter(grepl("RUN DATE", text)) %>% distinct(text) %>%
    mutate(election_ID = gsub(".*ID: ([0-9]+)$", "\\1", text)) %>%
    distinct(election_ID) %>%
    unlist()

  cat(glue("county and election date: {eID};   "))

  n_precincts <- nonempty %>% filter(grepl("CAND VOTES", text)) %>% nrow()
  cat(glue("Identified {n_precincts} precincts;  "))

  nototals <- nonempty %>%
    filter(!grepl("^[\\s\\d]+$", text, perl = TRUE)) %>%
    filter(!grepl("CAND VOTES", text)) %>%
    filter(!grepl("PRECINCT TOTALS", text))
  nrow(nototals)

  nototals
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
get_precinct_range <- function(df) {
  pfirst <- df %>%
    filter(grepl("RUN DATE", text, perl = TRUE)) %>%
    distinct(text, .keep_all = TRUE)

  pfirst_range <- pfirst %>%
    mutate(p_name = gsub(".*(?=PRECINCT)", "", text, perl = TRUE),
           p_name = gsub("\\s+ELECTION ID: [0-9]+", "", p_name, perl = TRUE),
           p_name = gsub("\\s+", " ", p_name, perl = TRUE)) %>%
    mutate(precinct_id = 1:n(),
           p_start_id = id + 1,
           p_end_id = lead(id, 1) - 1,
           text = NULL,
           id = NULL)

  pfirst_range$p_end_id[nrow(pfirst_range)] <- max(df$id)

  pfirst_range
}

pkey <- get_precinct_range(nototals)


#' Add precinct to each row of vote dataset, and remove other headers
#'
#' @param votes EL155 dataset of votes, product of read_EL155()
#' @param pkey precinct key, product of get_precinct_range()
#'
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

wprecinct <- add_precinct(nototals, pkey)

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
parse_EL155 <- function(votes, vote_col = "text", start_pos, end_pos, names_pos) {
  start_pos = c(1,  9, 14, 16, 21, 61)
  end_pos =   c(7, 12, 14, 19, 59, 131)
  names_pos = c("machine_id", "ballot_style", "marker", "cand_id", "cand_name", "race")

  parse_name <- function(vec, start_pos, end_pos, names_pos) {
    parsed <- str_sub(vec, start_pos, end_pos)
    tbl <- as_tibble(t(parsed)) %>% mutate_all(str_trim)
    colnames(tbl) <- names_pos
    tbl
  }

  foo %>%
    map_dfr(~ parse_name(.1$text,
                         start_pos = start_pos,
                         end_pos = end_pos,
                         names_pos = names_pos))

  foo %>%
    mutate_all()
  parse_name(.1, start_pos = start_pos, end_pos = end_pos, names_pos = names_pos))



}
