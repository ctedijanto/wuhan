# Data and code for examining coronavirus transmission

## Scripts

__nrevss.R:__ script for reading and producing figures from the coronavirus incidene data

## Data

__NationalCoV.csv:__ Dataset containing percent positive coronavirus by virus type from the CDC, 2 July 2016 -- 18 Jan 2020. These are stitched together from web archive snapshots of CDC data files accessed on 30 June 2018, 19 June 2019, and 26 June 2020. 

__ntests.csv:__ Number of tests performed weekly between 1 July 2014 and 27 June 2017 to detect any of the four coronaviruses reported to NREVSS, scraped from Figure 1A by Killerby _et al._ (2018) using PlotDigitizer. 

__ILINet.csv:__ Weekly ILI data from the CDC. Accessed through FluView Interactive.

__NREVSSClinicalFlu.csv:__ Lab-confirmed influenza dataset from the CDC capturing clinical samples as reported to the NREVSS dataset. Accessed through FluView Interactive. 

__NREVSSPublicHealthFlu.csv:__ Lab-confirmed influenza dataset from the CDC capturing public health samples as reported to the NREVSS dataset. Accessed through FluView Interactive. 

__YearWeekConversion.csv:__ A useful data frame for converting between the CDC's Year/Week numbering system to a date. Note that the dates fall on Saturdays, following the CoV data, whereas flu data are normally tagged on Sundays. Add 1 to each date to get to the corresponding Sunday. 

## Figures
__natcov.pdf:__ Depicts weekly percent positive by coronavirus from the NationalCoV.csv dataset.

__testsperweek.pdf__ Reproduces Figure 1A from Killerby _et al._ (2018), giving weekly number of coronavirus tests in the US between 1 July 2014 and 27 June 2017. 

__natcov_ili.pdf:__ Same as natcov.pdf with percent ILI overlaid.

__natcov_nrevssclinflu.pdf:__ Same as natcov.pdf with percent positive clinical influenza overlaid.

__natcov_nrevssphflu.pdf:__ Same as natcov.pdf with percent positive public health influenza overlaid.