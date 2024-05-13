# Guide
This repo contains the relevant code for my undergraduate thesis [Optimizing Agricultural Water Usage of Jordanian Economy](https://digitalcollections.drew.edu/UniversityArchives/ThesesAndDissertations/CLA/BA/2024/Mir/) which I worked on during Fall 2023 - Spring 2024 at Drew University. Everything is coded in R.

One of the things I strived to achieve while writing this code was to make it as abstract and general as possible. While I could have certainly done more, most of the code is representative of this. For example, the model used in the thesis can be very easily applied to other countries and agricultural commodities (assuming data is available for period of analysis). You could even add more objectives and constraints relatively easily. Though I acknowledge there are certain instances where the codebase appears to be statically defined for the specific analysis in the thesis, these limitations were not addressed due to time and my coding experience with R.

Below, I walk through each folder and main R files and their functionality.

## Install Packages
A handful of packages need to be installed in order to run the R files. These packages are loaded in whichever R files they are needed. The user should install all of the packages below on their system:
```r
install.packages(
    c("tidyverse", "riem", "weathermetrics", "SPEI", "FAOSTAT", "mco", "treemapify", "webr", "networkD3")
)
```

## [Data](./Data/)
This folder is where the imported climate data (which was purchased from the Jordan Meteorological Department) and FAOSTAT data are located. The FAOSTAT data is filtered for Jordan in 2019 for a set of agricultural commodities. Important code regarding downloading and filtering the FAO data can be found in [FAO Helpers.R](./Helpers/FAO%20Helpers.R).

Note: While price data could have been programmatically defined like production and trade data, we hardcode these values, as can be seen in line 368 of [Run.R](./Run.R).

## [Helpers](./Helpers/)
Many helper functions were made to assist with calculations. I will briefly walk through the functionality of each:

1. [FAO Helpers.R](./Helpers/FAO%20Helpers.R)
    - Loading, cleaning, and saving FAOSTAT Crop and Livestock Products (QCL) and Detailed Trade Matrix (TM) databases.
2. [Kc Functions.R](./Helpers/Kc%20Functions.R)
    - Functionality for computing monthly $K_c$ value to construct expanded $K_c$ and $ET_c$ curves without assuming every month has 30 days.
3. [METAR Helpers.R](./Helpers/METAR%20Helpers.R)
    - Collecting, cleaning, and aggregating METAR data from the Iowa Environmental Mesonet (IEM) for monthly $ET_o$ curves
4. [Optimization Helpers.R](./Helpers/Optimization%20Helpers.R)
    - Functionality associated with pre- and post- optimization
5. [Visualization Helpers.R](./Helpers//Visualization%20Helpers.R)
    - Functionality for creating some of the visuals

## [Images](./Images/)
This folder contains the generated plot images.

## [Solutions](./Solutions/)
This folder contains all the solutions from the NSGA-II algorithm ran under different conditions. Solutions can be then loaded and assessed in a different R file.

Note: As of 5/13/24, I have made minor performance improvements and, as a result, am re-running Scenarios A and B, as detailed in the thesis. When both optimizations are completed, they will be added to the folder.

## Main R Files

### [Example.R](./Example.R)
In Example.R, one can find the code representation of the example problem as shown in the thesis in Section 2.4. All water footprint and economic data are hard-coded. We run NSGA-II for 1000 generations and save the results in the Solutions folder. Afterwards, we create some visuals and assess the results.

### [Run.R](./Run.R)
in Run.R, we calculate (almost) all of the water footprint and economic data dynamically for Jordan in 2019. We compute the baseline values and then run NSGA-II and save the results in the Solutions folder.

Note: If the user would like to perform their own analysis or make changes to the model, it is recommended they make them here and run this file in a separate terminal.

### [Analysis.R](./Analysis.R)
In Analysis.R, we assess the results of the optimization from Run.R.

Note: Since Run.R was designed to run the optimization algorithm and Analysis.R was designed to assess the results, Analysis.R  depends on some variables that were computed in Run.R (like `countries`, `items`, `max_water_use`, `min_revenue`). The user should run all of Analysis.R up until "Running Optimization" section to assure all necessary variables are computed. Then, in the same session, the user can run Analysis.R without errors.

### [Visualizations.R](./Visualizations.R)
This file was used to generate some extra visuals that were used in the thesis.