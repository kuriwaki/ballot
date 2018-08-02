ballot
================

Install by

``` r
devtools::install_github("kuriwaki/ballot")
```

Wrapper
=======

A wrapper across all counties in a given election is

``` r
subdirs <- dir_ls(path(dir, "input/SC_2017-11-28"), recursive = FALSE)
df <- suppressWarnings(read_format_EL155(subdirs))
```

    ## Berkeley (code 08112817) with 13 precincts,  
    ## Charleston (code 10112817) with 8 precincts,

To do this county by county, see the following.

Each Function Separately
========================

`read_EL155`
------------

Read the EL155 file (raw file output for ballot images) and format.

``` r
p2016 <- read_EL155(path(dir, "input/SC_2016-06-14/Spartanburg/EL155"), "Spartanburg")
```

    ## Spartanburg (code 42061416) with 95 precincts,

``` r
p2016
```

    ## # A tibble: 38,570 x 3
    ##       id county     text                                                  
    ##    <int> <chr>      <chr>                                                 
    ##  1     2 Spartanbu… RUN DATE:06/16/16 10:38 AM                         PR…
    ##  2     4 Spartanbu… VOTR.    B/I     CANDIDATES RECEIVING A VOTE          
    ##  3     6 Spartanbu… 5131172    2 *    2 Scott Ramsey                     …
    ##  4     7 Spartanbu… 5131172    2      5 Jane Hall                        …
    ##  5     8 Spartanbu… 5131172    2 *    1 Rusty Clevenger                  …
    ##  6     9 Spartanbu… 5131172    2      4 Whitney Farr                     …
    ##  7    10 Spartanbu… 5131172    2 *    1 Rusty Clevenger                  …
    ##  8    11 Spartanbu… 5131172    2      5 Jane Hall                        …
    ##  9    12 Spartanbu… 5131172    2 *    1 Rusty Clevenger                  …
    ## 10    13 Spartanbu… 5131172    2      4 Whitney Farr                     …
    ## # ... with 38,560 more rows

`get_precinct`
--------------

Add precinct identifier to each row

``` r
pkey <- get_precinct(p2016)
head(pkey)
```

    ## # A tibble: 6 x 5
    ##   county     precinct                      precinct_id p_start_id p_end_id
    ##   <chr>      <chr>                               <int>      <dbl>    <dbl>
    ## 1 Spartanbu… PRECINCT 1 - Woodruff Leisur…           1          3      114
    ## 2 Spartanbu… PRECINCT 3 - Arcadia Element…           2        116      194
    ## 3 Spartanbu… PRECINCT 4 - Rebirth Mission…           3        196      911
    ## 4 Spartanbu… PRECINCT 6 - Friendship Bapt…           4        913     1675
    ## 5 Spartanbu… PRECINCT 7 - Morningside                5       1677     2009
    ## 6 Spartanbu… PRECINCT 8 - Boiling Springs…           6       2011     2564

``` r
p2016_precinct <- add_precinct(p2016, pkey)
p2016_precinct
```

    ## # A tibble: 37,284 x 5
    ##       id county   text                        precinct         precinct_id
    ##    <int> <chr>    <chr>                       <chr>                  <int>
    ##  1     6 Spartan… 5131172    2 *    2 Scott … PRECINCT 1 - Wo…           1
    ##  2     7 Spartan… 5131172    2      5 Jane H… PRECINCT 1 - Wo…           1
    ##  3     8 Spartan… 5131172    2 *    1 Rusty … PRECINCT 1 - Wo…           1
    ##  4     9 Spartan… 5131172    2      4 Whitne… PRECINCT 1 - Wo…           1
    ##  5    10 Spartan… 5131172    2 *    1 Rusty … PRECINCT 1 - Wo…           1
    ##  6    11 Spartan… 5131172    2      5 Jane H… PRECINCT 1 - Wo…           1
    ##  7    12 Spartan… 5131172    2 *    1 Rusty … PRECINCT 1 - Wo…           1
    ##  8    13 Spartan… 5131172    2      4 Whitne… PRECINCT 1 - Wo…           1
    ##  9    14 Spartan… 5131172    2 *    1 Rusty … PRECINCT 1 - Wo…           1
    ## 10    15 Spartan… 5131172    2      4 Whitne… PRECINCT 1 - Wo…           1
    ## # ... with 37,274 more rows

`parse_EL155`
-------------

Parse the text with pre-specified fixed width.

``` r
p2016_parsed <- parse_EL155(p2016_precinct)
p2016_parsed
```

    ## # A tibble: 37,284 x 10
    ##       id county precinct precinct_id machine ballot_style choice_id
    ##    <int> <chr>  <chr>          <int> <chr>          <int>     <int>
    ##  1     6 Spart… PRECINC…           1 5131172            2         2
    ##  2     7 Spart… PRECINC…           1 5131172            2         5
    ##  3     8 Spart… PRECINC…           1 5131172            2         1
    ##  4     9 Spart… PRECINC…           1 5131172            2         4
    ##  5    10 Spart… PRECINC…           1 5131172            2         1
    ##  6    11 Spart… PRECINC…           1 5131172            2         5
    ##  7    12 Spart… PRECINC…           1 5131172            2         1
    ##  8    13 Spart… PRECINC…           1 5131172            2         4
    ##  9    14 Spart… PRECINC…           1 5131172            2         1
    ## 10    15 Spart… PRECINC…           1 5131172            2         4
    ## # ... with 37,274 more rows, and 3 more variables: choice_name <chr>,
    ## #   contest_name <chr>, voter_top <int>

`identify_voter`
----------------

Add a unique ID for each voter

``` r
with_voter <- identify_voter(p2016_parsed)
```

`add_id`
--------

Add voter identifier that will be unique with many counties

``` r
df <- add_id(with_voter)
```
