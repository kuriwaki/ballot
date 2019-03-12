#' Encode party of statewide offices in South Carolina
#'
#' Recode each year's statewide elections to numeric by hand. Because we know these were on the ballot for everyone, numeric codes can be determined as a 0
#'
#' @param tbl wide table with columns like \env{GOV0000}.
#' @param year year of general election
#'
#' @export
stwide_to_numeric <- function(tbl, year) {

  # 2010
  if (year == "2010") {
    out <- tbl %>%
      mutate(
        GOV_party = recode(GOV0000, `Nikki R Haley` = 1L, `Vincent A Sheheen` = -1L, .default = 0L, .missing = 0L),
        USSEN_party = recode(USSEN01, `Jim DeMint` = 1L, `Alvin M Greene` = -1L, .default = 0L, .missing = 0L),
        LGV_party = recode(LGV0000, `Ken Ard` = 1L, `Ashley Cooper` = -1L, .default = 0L, .missing = 0L),
        ATG_party = recode(ATG0000, `Alan Wilson` = 1L, `Matthew Richardson` = -1L, .default = 0L, .missing = 0L),
        SOS_party = recode(SOS0000, `Mark Hammond` = 1L, `Marjorie L Johnson` = -1L, .default = 0L, .missing = 0L),
        SSI_party = recode(SSI0000, `Mick Zais` = 1L, `Frank Holleman` = -1L, .default = 0L, .missing = 0L)
      ) %>%
      rename(
        GOV_vote = GOV0000,
        USSEN_vote = USSEN01,
        LGV_vote = LGV0000,
        ATG_vote = ATG0000,
        SOS_vote = SOS0000,
        SSI_vote = SSI0000)
  }

  # 2012
  if (year == "2012") {
    out <- tbl %>%
      mutate(
        PRS_party = recode(PRS0000, `Mitt Romney` = 1L, `Barack Obama` = -1L, .default = 0L, .missing = 0L)
      ) %>%
      rename(
        PRS_vote = PRS0000
        )
  }

  # 2014
  if (year == "2014") {
    out <- tbl %>%
      mutate(
        GOV_party = recode(GOV0000, `Nikki R Haley` = 1L, `Vincent Sheheen` = -1L, .default = 0L, .missing = 0L),
        USSEN1_party = recode(USSEN01, `Lindsey Graham` = 1L, `Brad Hutto` = -1L, .default = 0L, .missing = 0L),
        USSEN2_party = recode(USSEN02, `Tim Scott` = 1L, `Joyce Dickerson` = -1L, .default = 0L, .missing = 0L),
        LGV_party = recode(LGV0000, `Henry McMaster` = 1L, `Bakari Sellers` = -1L, .default = 0L, .missing = 0L),
        ATG_party = recode(ATG0000, `Alan Wilson` = 1L, `Parnell Diggs` = -1L, .default = 0L, .missing = 0L),
        SOS_party = recode(SOS0000, `Mark Hammond` = 1L, `Ginny Deerin` = -1L, .default = 0L, .missing = 0L),
        SSI_party = recode(SSI0000, `Molly Mitchell Spearman` = 1L, `Tom Thompson` = -1L, .default = 0L, .missing = 0L)
      ) %>%
      rename(
        GOV_vote = GOV0000,
        USSEN1_vote = USSEN01,
        USSEN2_vote = USSEN02,
        LGV_vote = LGV0000,
        ATG_vote = ATG0000,
        SOS_vote = SOS0000,
        SSI_vote = SSI0000
      )
  }

  # 2016
  if (year == "2016") {
    out <-  tbl %>%
      mutate(
        PRS_party = recode(PRS0000, `Donald J Trump` = 1L, `Hillary Rodham Clinton` = -1L, .default = 0L, .missing = 0L),
        USSEN_party = recode(USSEN01, `Tim Scott` = 1L,  `Thomas Dixon` = -1L, .default = 0L, .missing = 0L)
      ) %>%
      rename(
        PRS_vote = PRS0000,
        USSEN_vote = USSEN01
        )
  }

  if (year == "2018") {
    out <- tbl %>%
      mutate(
        GOV_party = recode(GOV0000, `Henry McMaster` = 1L, `James Smith` = -1L, .default = 0L, .missing = 0L),
        ATG_party = recode(ATG0000, `Alan Wilson` = 1L, `Constance Anastopoulo` = -1L, .default = 0L, .missing = 0L),
        SOS_party = recode(SOS0000, `Mark Hammond` = 1L, `Melvin T Whittenburg` = -1L, .default = 0L, .missing = 0L),
        SSI_party = recode(SSI0000, `Molly Mitchell Spearman` = 1L, `Israel Romero` = -1L, .default = 0L, .missing = 0L)
      ) %>%
      rename(
        GOV_vote = GOV0000,
        ATG_vote = ATG0000,
        SOS_vote = SOS0000,
        SSI_vote = SSI0000
      )
  }


  ## all years have party
  out <- out %>%
    mutate(
      PTY_party = recode(PTY0000, `Republican` = 1L, `Democratic` = -1L, .default = 0L, .missing = 0L)
    ) %>%
    rename(PTY_vote = PTY0000)

  out
}
