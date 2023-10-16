# LEMMA-Forecasts2
This reposistory containts forecasts from the LEMMA model starting October 2023. 

Older LEMMA forecasts are in the LEMMA-Forecasts repo. https://github.com/LocalEpi/LEMMA-Forecasts

The main LEMMA sites are

https://localepi.github.io/LEMMA/  
https://github.com/LocalEpi/LEMMA

# Omicron

Columns in `All_CA_county_Forecasts.csv`  

- `date`  
- `county`
- `hosp_census_with_covid` hospital census of all COVID positive patients (includes incidental admissions; "with covid")
- `hosp_census_for_covid` hospital census of COVID positive patients admitted primarily due to COVID (includes incidental admissions; "for covid")
- `quantile` quantile of credibility interval (5, 50, 95)

Hospital census includes COVID positive patients with either Delta or Omicron. 
The current model assumption is that 40% of COVID positive patients are incidental admissions (not primarily due to COVID).

Scenarios prior to Jan 10 have been removed but are available in old commits:  
/Scenarios/All_CA_county_Omicron_Scenarios.csv  
/Scenarios/All_CA_county_Omicron_Scenarios_V2.csv

There are still files specific to San Francisco in the `\Scenarios` folder to maintain links with external code.  


## Contributors
LEMMA is a collaborative effort between experts in Medicine, Public Health, and Data Science, including but not limited to

- Joshua Schwab - UC Berkeley
- Laura B. Balzer - UMass Amherst
- Elvin Geng - Washington University
- James Peng - UC San Francisco
- Maya L. Petersen - UC Berkeley
- Sean L. Wu - UC Berkeley

We have moved our model fitting from R to Stan. Our Stan implementation is based on the "Santa Cruz County COVID-19 Model" (https://github.com/jpmattern/seir-covid19) by Jann Paul Mattern (UC Santa Cruz) and Mikala Caton (Santa Cruz County Health Services Agency). We are very grateful to Paul and Mikala for generously sharing their code and helping us.
