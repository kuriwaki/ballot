
#' Waffle Plot of votes, colored
#'
#'
#' @param tbl_indiv A wide dataset of individual votes
#' @param office_nam Title of the office. If NULL, use recode_abbrv of variable name
#' @param var The variable in the dataset to use as the colors
#' @param nrows number of rows and columns
#'
#'
#'
#' @import ggplot2
#' @examples
#'  \dontrun{
#'    w_prez <- filter(wide, elec %in% c("2012-11-06", "2016-11-08"))
#'    gg_wfl(filter(w_prez, PRS_party == 1), USH_party, nrows = 10)
#'   }
#'
#' @export
gg_wfl <- function(tbl_indiv, var, nrows = 31, rev = FALSE,
                   office_nam = NULL,
                   blank = FALSE,
                   legend = FALSE, title_size = 0.8, check_ncand = TRUE) {
  var <- enquo(var)
  var_name <- quo_name(var)

  cells <- crossing(y = 1:nrows, x = 1:nrows)
  n_cells <- nrows^2

  if (check_ncand) {
    cand_name <- str_replace(var_name, "party", "ncand")
    tbl_indiv <- filter(tbl_indiv, .data[[cand_name]] >= 2)
  }

  tbl_indiv <- tbl_indiv |>
    filter(!is.na(!!var))

  categ_table <-  tbl_indiv %>%
    count(!!var) %>%
    mutate(!!var := factor(!!var))
  vec_n <- length(tbl_indiv[[var_name]])

  categ_table$n  <- round(categ_table$n * (n_cells)/(vec_n))

  # adjust for rounding
  if (sum(categ_table$n) != n_cells) {
    diff <- n_cells - sum(categ_table$n)
    categ_table <- categ_table %>%
      mutate(n = n + (n == max(n))*diff)
  }

  if (rev) {
    categ_table <- categ_table %>%
      mutate(!!var := fct_rev(as.character(!!var))) %>%
      arrange(!!var)
  }
  if (!rev) {
    categ_table <- categ_table %>%
      mutate(!!var := factor(!!var, c(-1, 0.5, 0, 1))) %>%
      arrange(!!var)
  }
  # enter everyone
  cells <- cells %>%
    mutate(category = rep(pull(categ_table, !!var),
                          times = categ_table$n))

  # text
  print_pct <- percent(max(categ_table$n)/(nrows^2))

  # plot
  if (is.null(office_nam))
    office_nam <- recode_abbrv(var_name)

  g0 <- ggplot(cells, aes(x = x, y = y, fill = category)) +
    scale_fill_manual(name = "",
                      values = c("1" = "#b2182b", "-1" = "#2166ac", "0.5" = "#ffffbf", "0" = "#999999"),
                      labels = c("1" = "Republican", "-1" = "Democrat", "0.5" = "Other", "0" = "Drop-off")) +
    # scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0),
                       trans = 'reverse') +
    coord_equal() +
    theme_void() +
    labs(title = glue("{office_nam} (n = {str_c(round(vec_n/1000), 'k')})" )) +
    theme(plot.title = element_text(hjust = 0.5, size = rel(title_size)))

  if (!legend)
    g0 <- g0 +  guides(fill = "none")

  #
  if (isTRUE(blank)) {
    gg <- g0 + geom_blank() +
      theme(plot.caption = element_text(color = "white", hjust = 0.5, size = rel(0.8)),
            plot.title = element_text(color = "white"))
  }

  if (!blank) {
    gg <- g0 + geom_tile(color = NA) +
      annotate("text",
               x = nrows/2,
               y =  nrows/2,
               label = glue("{print_pct}"),
               color = "white",
               size = 4.5)
  }
  gg
}
