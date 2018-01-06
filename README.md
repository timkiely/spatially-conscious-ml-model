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
-   (01/06/2018) New game plan. Re-drew the steps of the procedure and am targeting 2 models: probability of sale & amount. Will have to step through the data processing steps once more and re-run the radii metrics calculations.

### TODO:

-   Run the data processing steps from scratch (inlude all years)
-   Create features & data partitions
-   Re-run models

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

<span style="color:darkred"> **OUTPUT: All years of PLUTO data with sales information (frequency, amt, etc)** </span>

#### B) feature engineering & data partitioning

1.  Create radii-index & radii features
2.  Create zip code features
3.  Create 3 modleing data sets:
    1.  Base
    2.  Zipcode features
    3.  Radii features

<span style="color:darkred"> **OUTPUT: 3 modeling data sets with differing features** </span>

#### C) probability of sale model

1.  Create data/modeling combos (use `purrr:::invoke()` technique)
2.  Run models
3.  Review and compare results

<span style="color:darkred"> **OUTPUT: binary prediction modeling results** </span>

#### D) sale price model

1.  Create data/modeling combos (use `purrr:::invoke()` technique)
2.  Run models
3.  Review and compare results

``` html
<span style="color:darkred">
**OUTPUT: regression prediction modeling results**
</span>
```

MODEL RUN LOG:
==============

### Model Run Oct 16th, 2017

First complete model training run complete. Total run time was 1.6 days, with considerable time dedicated to Random Forrest (approx. 1.3 days for just RF). We can significantly improve the model training time by optimizing the RF routine, possibly by experimenting with alternative implementations of RF. In addition, the Multi layer perceptron models performed exceedingly poorly and also required some of the longest training times. It's possible that they require significant tuning in order to be competitive. Literature suggests that MLP's won't compete vs GBM or RF without significant pre-processing and computation time. I will consider tuning MLP further.

### Model Run Oct 19th, 2017

After optimizing several of the slowest models, the entire runtime has been reduced by a factor of 8 (from ~40 hours down to 5). This despite adding several models. The largest speedup was due to incorporating H2O for Random Forrest rather than the caret version.

Interestingly, the best performing models are trained on the numeric\_only and numeric\_processed datasets. The vtreat packages appears to help with parametric models but the non-linear models don't seem to benefit from such treatment. It's possible and worth investigating alternative methods for dealing with categorial variables, such as one-hot encoding. This seems like the logical next step as the overall process will no doubt benefit from having the best possible baseline models prior to introducing proximity features.
