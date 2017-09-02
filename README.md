Analysis of NYC Sales Data
================

A model to predict NYC sales amounts and frequencies.

### Objective

Create a modeling data set with NYC sales data. To enrich features, we want to map in PLUTO and ACRIS information. In particular, mapping in lat/lon coodinates from PLUTO will allow us to locate our observations in space, which, in turn, will enable geo-spatial analysis of the data set. ACRIS is primarily for filtering purposes, e.g., remove "Time Share Deed" transfers that bloat the dataset with small frequent non-sale transactions.

### Done:

-   Mapped PLUTO to the sales data. This involved first mapping PAD (expanded) to PLUTO in order to get an ehaustive list of bbls (including condos), then mapping the resultant data back to SALES via BBL.

### To do:

-   Map in ACRIS data in order to filter out useless transactions data (e,g. Timeshare Deeds)
-   Create the baseline predictive model. This will involve the usual steps of EDA, transformation, pre-processing, and modeling.
