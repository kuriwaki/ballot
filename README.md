`ballot`
================

``` r
devtools::install_github("kuriwaki/ballot")
```

Wrapper
=======

A wrapper across all counties in a given election is

``` r
subdirs <- list.dirs(file.path(dir, "build/input/SC_2016-06-14"), recursive = FALSE)
df <- read_format_EL155(subdirs)
```

    ## Warning: executing %dopar% sequentially: no parallel backend registered

    ## Abbeville (code 01061416) with 15 precincts,  
    ## Aiken (code 02061416) with 86 precincts,  
    ## Allendale (code 03061416) with 9 precincts,  
    ## Anderson (code 04061416) with 82 precincts,  
    ## Bamberg (code 05061416) with 15 precincts,  
    ## Barnwell (code 06061416) with 16 precincts,  
    ## Beaufort (code 07061416) with 94 precincts,  
    ## Berkeley (code 08061416) with 69 precincts,  
    ## Calhoun (code 09061416) with 14 precincts,  
    ## Charleston (code 10061416) with 184 precincts,  
    ## Cherokee (code 11PRIM16) with 31 precincts,  
    ## Chester (code 12061416) with 23 precincts,  
    ## Chesterfield (code 13061416) with 18 precincts,  
    ## Clarendon (code 14061416) with 27 precincts,  
    ## Colleton (code 15061416) with 34 precincts,  
    ## Darlington (code 16061416) with 34 precincts,  
    ## Dillon (code 17061416) with 22 precincts,  
    ## Dorchester (code 18061416) with 83 precincts,  
    ## Edgefield (code 19061416) with 14 precincts,  
    ## Fairfield (code 20061416) with 24 precincts,  
    ## Florence (code 21061416) with 65 precincts,  
    ## Georgetown (code 22061416) with 36 precincts,  
    ## Greenville (code 23061416) with 153 precincts,  
    ## Greenwood (code 24061416) with 50 precincts,  
    ## Hampton (code 25061416) with 20 precincts,  
    ## Horry (code 26061416) with 124 precincts,  
    ## Jasper (code 27061416) with 17 precincts,  
    ## Kershaw (code 28061416) with 34 precincts,  
    ## Lancaster (code 29061416) with 37 precincts,  
    ## Laurens (code 30061416) with 36 precincts,  
    ## Lee (code 31061416) with 24 precincts,  
    ## Lexington (code 32061416) with 101 precincts,  
    ## Marion (code 34061416) with 19 precincts,  
    ## Marlboro (code 35061416) with 17 precincts,  
    ## McCormick (code 33061416) with 13 precincts,  
    ## Newberry (code 36061416) with 32 precincts,  
    ## Oconee (code 37061416) with 33 precincts,  
    ## Orangeburg (code 38061416) with 55 precincts,  
    ## Pickens (code 39061416) with 63 precincts,  
    ## Richland (code 40061416) with 151 precincts,  
    ## Saluda (code 41061416) with 20 precincts,  
    ## Spartanburg (code 42061416) with 95 precincts,  
    ## Sumter (code 43061416) with 59 precincts,  
    ## Union (code 44061416) with 25 precincts,  
    ## Williamsburg (code 45061416) with 29 precincts,  
    ## York (code 46061416) with 97 precincts,

To do this county by county, see the following.

Each Function Separately
========================

`read_EL155`
------------

Read the EL155 file (raw file output for ballot images) and format.

``` r
p2016 <- read_EL155(file.path(dir, "build/input/SC_2016-06-14/Spartanburg/EL155"), "Spartanburg")
```

    ## Spartanburg (code 42061416) with 95 precincts,

``` r
p2016
```

    ## # A tibble: 38,570 x 3
    ##       id county      text                                                 
    ##    <int> <chr>       <chr>                                                
    ##  1     2 Spartanburg RUN DATE:06/16/16 10:38 AM                         P…
    ##  2     4 Spartanburg VOTR.    B/I     CANDIDATES RECEIVING A VOTE         
    ##  3     6 Spartanburg 5131172    2 *    2 Scott Ramsey                    …
    ##  4     7 Spartanburg 5131172    2      5 Jane Hall                       …
    ##  5     8 Spartanburg 5131172    2 *    1 Rusty Clevenger                 …
    ##  6     9 Spartanburg 5131172    2      4 Whitney Farr                    …
    ##  7    10 Spartanburg 5131172    2 *    1 Rusty Clevenger                 …
    ##  8    11 Spartanburg 5131172    2      5 Jane Hall                       …
    ##  9    12 Spartanburg 5131172    2 *    1 Rusty Clevenger                 …
    ## 10    13 Spartanburg 5131172    2      4 Whitney Farr                    …
    ## # ... with 38,560 more rows

`get_precinct`
--------------

Add precinct identifier to each row

``` r
pkey <- get_precinct(p2016)
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
p2016_parsed
```

    ## # A tibble: 37,284 x 9
    ##    county  p_name precinct_id machine ballot_style cand_id cand_name race 
    ##    <chr>   <chr>        <int> <chr>          <int>   <int> <chr>     <chr>
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

`add_id`
--------

Add voter identifier that will be unique with many counties

``` r
df <- add_id(with_voter)
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
