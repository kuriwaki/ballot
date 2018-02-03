`ballot`
================

``` r
devtools::install_github("kuriwaki/ballot")
```

Procedure
=========

`read_EL155`
------------

Read the EL155 file (raw file output for ballot images) and format.

``` r
p2016 <- read_EL155(file.path(dir, "build/input/SC_2016-06-14/Spartanburg/EL155"), "Spartanburg")
```

    ## Spartanburg code 42061416 - with 95 precincts;

`get_precinct`
--------------

Add precinct identifier to each row

``` r
pkey <- get_precinct_range(p2016)
head(pkey)
```

    ## # A tibble: 6 x 5
    ##   county      p_name                       precinct_id p_start_id p_end_id
    ##   <chr>       <chr>                              <int>      <dbl>    <dbl>
    ## 1 Spartanburg PRECINCT 1 - Woodruff Leisu…           1       3.00      114
    ## 2 Spartanburg PRECINCT 3 - Arcadia Elemen…           2     116         194
    ## 3 Spartanburg PRECINCT 4 - Rebirth Missio…           3     196         911
    ## 4 Spartanburg PRECINCT 6 - Friendship Bap…           4     913        1675
    ## 5 Spartanburg PRECINCT 7 - Morningside               5    1677        2009
    ## 6 Spartanburg PRECINCT 8 - Boiling Spring…           6    2011        2564

``` r
p2016_precinct <- add_precinct(p2016, pkey)
p2016_precinct
```

    ## # A tibble: 37,284 x 4
    ##    county      p_name                               precinct_id text      
    ##    <chr>       <chr>                                      <int> <chr>     
    ##  1 Spartanburg PRECINCT 1 - Woodruff Leisure Center           1 5131172  …
    ##  2 Spartanburg PRECINCT 1 - Woodruff Leisure Center           1 5131172  …
    ##  3 Spartanburg PRECINCT 1 - Woodruff Leisure Center           1 5131172  …
    ##  4 Spartanburg PRECINCT 1 - Woodruff Leisure Center           1 5131172  …
    ##  5 Spartanburg PRECINCT 1 - Woodruff Leisure Center           1 5131172  …
    ##  6 Spartanburg PRECINCT 1 - Woodruff Leisure Center           1 5131172  …
    ##  7 Spartanburg PRECINCT 1 - Woodruff Leisure Center           1 5131172  …
    ##  8 Spartanburg PRECINCT 1 - Woodruff Leisure Center           1 5131172  …
    ##  9 Spartanburg PRECINCT 1 - Woodruff Leisure Center           1 5131172  …
    ## 10 Spartanburg PRECINCT 1 - Woodruff Leisure Center           1 5131172  …
    ## # ... with 37,274 more rows

`parse_EL155`
-------------

Parse the text with pre-specified fixed width.

``` r
p2016_parsed <- parse_EL155(p2016_precinct)
```

    ## Warning in rbind(names(probs), probs_f): number of columns of result is not
    ## a multiple of vector length (arg 1)

    ## Warning: 37284 parsing failures.
    ## row # A tibble: 5 x 5 col     row col   expected actual file         expected   <int> <chr> <chr>    <chr>  <chr>        actual 1     1 race  71 chars 7      literal data file 2     2 race  71 chars 22     literal data row 3     3 race  71 chars 7      literal data col 4     4 race  71 chars 22     literal data expected 5     5 race  71 chars 7      literal data
    ## ... ................. ... .......................................... ........ .......................................... ...... .......................................... .... .......................................... ... .......................................... ... .......................................... ........ ..........................................
    ## See problems(...) for more details.

``` r
p2016_parsed
```

    ## # A tibble: 37,284 x 9
    ##    county  p_name precinct_id machine ballot_style cand_id cand_name race 
    ##    <chr>   <chr>        <int>   <int>        <int>   <int> <chr>     <chr>
    ##  1 Sparta… PRECI…           1 5131172            2       2 Scott Ra… Coro…
    ##  2 Sparta… PRECI…           1 5131172            2       5 Jane Hall CCL0…
    ##  3 Sparta… PRECI…           1 5131172            2       1 Rusty Cl… Coro…
    ##  4 Sparta… PRECI…           1 5131172            2       4 Whitney … CCL0…
    ##  5 Sparta… PRECI…           1 5131172            2       1 Rusty Cl… Coro…
    ##  6 Sparta… PRECI…           1 5131172            2       5 Jane Hall CCL0…
    ##  7 Sparta… PRECI…           1 5131172            2       1 Rusty Cl… Coro…
    ##  8 Sparta… PRECI…           1 5131172            2       4 Whitney … CCL0…
    ##  9 Sparta… PRECI…           1 5131172            2       1 Rusty Cl… Coro…
    ## 10 Sparta… PRECI…           1 5131172            2       4 Whitney … CCL0…
    ## # ... with 37,274 more rows, and 1 more variable: voter_top <int>

`identify_voter`
----------------

Add a unique ID for each voter

``` r
with_voter <- identify_voter(p2016_parsed)
```

`add_unique_id`
---------------

Add voter identifier that will be unique with many counties

``` r
df <- add_unique_id(with_voter)
```

Finally, cast the long dataset to wide, to voter level

``` r
tbl <- dcast(as.data.table(df),  voter_id ~ race, value.var = "cand_name") %>% 
  tbl_df()

tbl
```

    ## # A tibble: 17,068 x 12
    ##    voter_id   `(NO VOTES CAST)` `CCL0003 County Coun… `CCL0004 County Cou…
    ##    <chr>      <chr>             <chr>                 <chr>               
    ##  1 45083-000… <NA>              <NA>                  Jane Hall           
    ##  2 45083-000… <NA>              <NA>                  Whitney Farr        
    ##  3 45083-000… <NA>              <NA>                  Jane Hall           
    ##  4 45083-000… <NA>              <NA>                  Whitney Farr        
    ##  5 45083-000… <NA>              <NA>                  Whitney Farr        
    ##  6 45083-000… <NA>              <NA>                  Jane Hall           
    ##  7 45083-000… <NA>              <NA>                  Jane Hall           
    ##  8 45083-000… <NA>              <NA>                  Whitney Farr        
    ##  9 45083-000… <NA>              <NA>                  Whitney Farr        
    ## 10 45083-000… <NA>              <NA>                  Whitney Farr        
    ## # ... with 17,058 more rows, and 8 more variables: `CON0005 U.S. House of
    ## #   Representatives` <chr>, Coroner <chr>, `HOU0031 State House of
    ## #   Representatives` <chr>, `HOU0037 State House of
    ## #   Representatives` <chr>, `HOU0038 State House of
    ## #   Representatives` <chr>, `SEN0005 State Senate` <chr>, `SEN0012 State
    ## #   Senate` <chr>, `SEN0014 State Senate` <chr>
