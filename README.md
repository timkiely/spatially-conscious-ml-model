Analysis of NYC Sales Data
================

A model to predict NYC sales amounts and frequencies.

Objective
=========

Create a modeling data set with NYC sales data. To enrich features, we want to map in PLUTO and PAD information. In particular, mapping in lat/lon coordinates from PLUTO will allow us to locate our observations in space, which, in turn, will enable geo-spatial analysis of the data set. PAD is primarily for adding geo-location data to condos and other BBLs not readily found in the PLUTO dataset.

### Done:

-   Raw data downloaded and processed
-   Modeling data EDA done
-   Three scripts all functioning: Processing, model objects and model training
-   (10/13/2017) Added all models of interest and run first successful full trial. Run time took 1.6 days with Random Forrest models eating up considerable computation time.
-   (10/18/2017) Optimized run time by swapping in modern algorithms for KNN and especially Random Forrest (using highly parallel h2o package which also boosts accuracy). Run time now completes in around 5 hours (down from 1.6 days)
-   (11/18/2017) Added additonal evaluation metrics. Modeling: RMSE, MAPE, Spearman, Pearson, Rsq. AVM: Sales Ratio w/ CI's, COD
-   (01/06/2018) New game plan. Re-drew the steps of the procedure and am targeting 2 models: probability of sale & amount. Will have to step through the data processing steps once more and re-run the radii metrics calculations. Overhauled the directory structure. Project now has a clearer process and goal.
-   (01/23/2018) Progress update. scripts 00 through 06 all or nearly complete. Next tasks will be additional feature engineering and then modeling. Expecting the radii features to take significant time to sort out. I have some existing scripts which can be re-purposed for the modeling portion, so that should go quickly.
-   (01/24/2018) Had to manually correct the 2006 PLUTO data. Was missing some newline characters and variable names. Uploaded the corrected version so S3 and gave it public access. Updated download script to account for new destination.
-   (01/30/2018) Probability model now stable on base data. Working on evaluation script, then moving on to sales model, then zipcode + radii features.
-   (01/30/2018) UPDATE: SALES model now also stable on base data. The sales eval metrics are suspiciously too good; need to backtrack through the preocessing steps to see if I included the y variable in any of the training data ("Last Sale Amt" seems too good to be true). Evaluation scripts working for now, although could use some improvement. Last things to do is to create zip and radii features the wrap up

### TODO:

-   Create zip level and radii level features
-   Run all models on all data sets
-   Wrap up

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
3.  Create 3 modleing data sets:
    1.  Base
    2.  Zipcode features
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

TO RUN THE MODEL
================

The full script can be run from the command line (from inside the project directory) with the following command:

`Rscript R/00-script.R`

Additional arguments can be passed to the Rscript:

`Rscript R/00-script.R skip-dl skip-pp run-dev`

-   `skip-dl` skip the download script. Saves about 30 minutes
-   `skip-pp` skip the processing script. Saves about 50 minutes
-   `run-dev` run the probability model on base data with a 10% sample. Full model run takes ~2 minutes

MODEL RUN LOG:
==============

### Model Run Oct 16th, 2017

First complete model training run complete. Total run time was 1.6 days, with considerable time dedicated to Random Forrest (approx. 1.3 days for just RF). We can significantly improve the model training time by optimizing the RF routine, possibly by experimenting with alternative implementations of RF. In addition, the Multi layer perceptron models performed exceedingly poorly and also required some of the longest training times. It's possible that they require significant tuning in order to be competitive. Literature suggests that MLP's won't compete vs GBM or RF without significant pre-processing and computation time. I will consider tuning MLP further.

### Model Run Oct 19th, 2017

After optimizing several of the slowest models, the entire runtime has been reduced by a factor of 8 (from ~40 hours down to 5). This despite adding several models. The largest speedup was due to incorporating H2O for Random Forrest rather than the caret version.

Interestingly, the best performing models are trained on the numeric\_only and numeric\_processed datasets. The vtreat packages appears to help with parametric models but the non-linear models don't seem to benefit from such treatment. It's possible and worth investigating alternative methods for dealing with categorial variables, such as one-hot encoding. This seems like the logical next step as the overall process will no doubt benefit from having the best possible baseline models prior to introducing proximity features.
