

#' Join with candidate data with single district identifier
#'
#' For example, used for referenda where there is only one "seat"
#' @param tbl A wide dataset with the contest
#' @param join_col NSE code to indicate variable that is being joined (to ballot_name in cands)
#' @param type NSE code for the contest. Will also be used to filter \env{cand} if not null.
#' @param ... geographic variables to segment on
#'
#' #' This assumes that all rows inlcuded in \code{tbl} did have that election to choose and
#' that the candidate table is complete; therefore NAs are coded to 0 (abstain).
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
    filter(!is.na(!!join_cols_cand)) %>%
    rename(!!join_name_tbl := !!join_name_cand,
           !!vote_name := ballot_name)

  # add counts and rename
  cand_counts <- group_by(cand, elec, .data[[join_name_tbl]]) %>%
    summarize(n = sum(party_num %in% c(-1, 1)))

  # join relevant precinct voters and candidate
  # also delete junior senior as this does not seem to be in tbl
  # CHANGE ALL TO 0s
  joined <- tbl %>%
    mutate(!!vote_name := str_remove(.data[[vote_name]], "\\s+(Jr|Sr)\\.?$")) %>%
    left_join(cand, by = c("elec", join_name_tbl, vote_name)) %>% # join to candidate party
    left_join(cand_counts, by = c("elec", join_name_tbl)) %>% # ncand is at the district level
    mutate(party_num = replace(party_num, is.na(party_num), 0)) %>% #
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
#'
#' @details Use \code{join_dist} for an office based on a single district number (like USHOU
#' or state house, use \code{join_county} for an office countywide (like Sheriff),
#' \code{join_countydist} for an office based on both (like county council). All are
#' wrappers to \code{join_1col}. We simplify the problem into just a one-variable
#' merge.
#'
#' It creates new columns for each  \code{contest}: \code{contest_party}, a numeric
#' -1 (D), 0 (Abstain / 3rd Party / Write-in), 1 (R), and \code{contest_dist}, a
#' number for the district of the voter of the contest in question if applicable, and
#' \code{contest_ncand}, the number of D-Rs in the district (does not count 3rd party)
#'
#'
#' @export
#'
#'
#'
join_dist <- function(tbl, cands, office) {
  office_var <- enquo(office)
  office_name <- quo_name(office_var)

  cands_office <- filter(cands, contest_type == office_name)

  join_name <-       as.character(glue("{office_name}_dist"))
  vote_name <-       as.character(glue("{office_name}_vote"))
  vote_party_name <- as.character(glue("{office_name}_party"))
  vote_ncand_name <- as.character(glue("{office_name}_ncand"))


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

  vote_name <-       str_pad(office_name, width = 7, side = "right", pad = "0")
  vote_party_name <- as.character(glue("{office_name}_party"))
  vote_ncand_name <- as.character(glue("{office_name}_ncand"))
  vote_name2_name <- as.character(glue("{office_name}_vote"))

  join_1col(tbl, cands_office,
            vote_col = !!vote_name,
            join_cols_tbl = county,
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

  join_name <-   as.character(glue("{office_name}_dist"))

  vote_name <-       as.character(glue("{office_name}_vote"))
  vote_party_name <- as.character(glue("{office_name}_party"))
  vote_ncand_name <- as.character(glue("{office_name}_ncand"))

  tbl %>%
    mutate(ctydist = str_c(county, "-", .data[[join_name]])) %>%
    join_1col(cands_ctydist,
              vote_col = !!vote_name,
              join_cols_tbl =  ctydist,
              join_cols_cand = ctydist) %>%
    rename(!!vote_party_name := party_num,
           !!vote_ncand_name := n)
}


