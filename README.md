Analysis of NYC Sales Data
================

A model to predict NYC sales amounts and frequencies.

Objective
=========

Create a modeling data set with NYC sales data. To enrich features, we want to map in PLUTO and PAD information. In particular, mapping in lat/lon coordinates from PLUTO will allow us to locate our observations in space, which, in turn, will enable geo-spatial analysis of the data set. PAD is primarily for adding geo-location data to condos and other BBLs not readily found in the PLUTO data set.

TO RUN THE MODEL
================

The full script can be run from the command line (from inside the project directory) with the following command:

`Rscript R/00-script.R`

Additional arguments can be passed to the Rscript via the `optparse` package (use -h to see help menu):

``` bash
Rscript R/00-script.R -h

Usage: R/00-script.R [options]

Options:
    -d, --skip-dl
        Skip the download script to save time

    -p, --skip-pp
        Skip the pre-processing steps to same time

    -r, --run-radii
        Should the radii indexing be run? Default is not to run (very time intensive)

    -s, --run-sample
        Run the model on sample data

    -h, --help
        Show this help message and exit
```

Most of the options are used to speed up the development process. skip-dl, skip-pp and run-sample can only be run successfully once they have been run once and the data has been cached. run-radii by default does not run, because it is so time-intensive, however if has yet to be run successfully (or a sample of the data is not available) the script will throw an error.

Steps
=====

#### A) data

1.  Download & process all years of PLUTO data
    -   <https://www1.nyc.gov/site/planning/data-maps/open-data/bytes-archive.page?sorts%5Byear%5D=0>
    -   at time of writing, 2002-2017 files available
2.  Download & process all Property Address Directory data
    -   <https://data.cityofnewyork.us/City-Government/Property-Address-Directory/bc8t-ecyu/data>
    -   at time of writing, only 2017 file available (not perfect, but sufficient)
3.  Download & process all NYC rolling Sales data
    -   <http://www1.nyc.gov/site/finance/taxes/property-annualized-sales-update.page>
    -   at time of writing, 2003-2016 files available
4.  Combine PAD and Sales data (allows every transaction to be mapped to a BBL)
5.  Create base modeling data by Left-joining Sales-PAD data to PLUTO

``` ruby
OUTPUT: all years of PLUTO data with sales information (frequency, amt, etc)
```

#### B) feature engineering & data partitioning

1.  Create radii-index & radii features
2.  Create zip code features
3.  Create 3 modeling data sets:
    1.  Base
    2.  Zip code features
    3.  Radii features

``` ruby
OUTPUT: 3 modeling data sets with differing features
```

#### C) probability of sale model

1.  Create data/modeling combos (use `purrr:::invoke()` technique)
2.  Run models
3.  Review and compare results

``` ruby
OUTPUT: binary prediction modeling results
```

#### D) sale price model

1.  Create data/modeling combos (use `purrr:::invoke()` technique)
2.  Run models
3.  Review and compare results

``` ruby
OUTPUT: regression prediction modeling results
```
