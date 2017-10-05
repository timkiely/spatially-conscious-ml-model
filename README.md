Analysis of NYC Sales Data
================

A model to predict NYC sales amounts and frequencies.

### Objective

Create a modeling data set with NYC sales data. To enrich features, we want to map in PLUTO and ACRIS information. In particular, mapping in lat/lon coodinates from PLUTO will allow us to locate our observations in space, which, in turn, will enable geo-spatial analysis of the data set. ACRIS is primarily for filtering purposes, e.g., remove "Time Share Deed" transfers that bloat the dataset with small frequent non-sale transactions.

### Done:

-   Raw data downloaded and processed
-   Modeling data EDA done
-   Three scripts all functioning: Processing, model objects and model training

### To do:

-   Baseline R Squared is relatively low (0.4 on test runs with 1,000 rows of data)
-   Feature processing and tuning. Model selection
-   Radial metrics
