Analysis of NYC Sales Data
================

A model to predict NYC sales amounts and frequencies.

### Objective

Create a modeling data set with NYC sales data. To enrich features, we want to map in PLUTO and ACRIS information. In particular, mapping in lat/lon coordinates from PLUTO will allow us to locate our observations in space, which, in turn, will enable geo-spatial analysis of the data set. ACRIS is primarily for filtering purposes, e.g., remove "Time Share Deed" transfers that bloat the data set with small frequent non-sale transactions.

### Done:

-   Raw data downloaded and processed
-   Modeling data EDA done
-   Three scripts all functioning: Processing, model objects and model training
-   Added all models of interest and run first successful full trial. Run time took 1.6 days with Random Forrest models eating up considerable computation time.

### To do:

-   Baseline R Squared is relatively low (0.4 on test runs with 1,000 rows of data)
-   Feature processing and tuning. Model selection
-   Radial metrics
-   Can cut down on the model training process considerably by optimizing the Random Forrest routines, possibly by subbing in a different package

### Model Run Oct 16th, 2017

First complete model training run complete. Total run time was 1.6 days, with considerable time dedicated to Random Forrest (approx. 1.3 days for just RF). We can significantly improve the model training time by optimizing the RF routine, possibly by experimenting with alternative implementations of RF. In addition, the Multi layer perceptron models performed exceedingly poorly and also required some of the longest training times. It's possible that they require significant tuning in order to be competitive. Literature suggests that MLP's won't compete vs GBM or RF without significant pre-processing and computation time. I will consider tuning MLP further.
