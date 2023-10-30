#if LEMMA is not already installed:
#devtools::install_github("LocalEpi/LEMMA")

library(data.table)
library(ggplot2)
library(rstan)
library(readxl)
library(LEMMA)
library(matrixStats)
source("Code/MarmVE.R")

county_set <- c("Alameda", "Amador", "Butte", "Contra Costa", "Fresno", "Humboldt",
                "Imperial", "Kern", "Kings", "Lake", "Los Angeles", "Madera",
                "Marin", "Mendocino", "Merced", "Monterey", "Napa", "Nevada",
                "Orange", "Placer", "Riverside", "San Francisco",
                "Sacramento", "San Bernardino", "San Diego", "San Joaquin", "San Luis Obispo",
                "San Mateo", "Santa Barbara", "Santa Clara", "Santa Cruz", "Shasta",
                "Solano", "Sonoma", "Stanislaus", "Tehama", "Tulare",
                "Tuolumne", "Ventura", "Yolo", "Yuba")

if (F) {
  county.dt <- GetCountyData()
  saveRDS(county.dt, "Inputs/savedCountyData.rds")
} else {
  county.dt <- readRDS("Inputs/savedCountyData.rds")
}

#Use this if some counties don't converge
#If rerun_set is not NULL, run only this set, use "Temp/forecast_list.rds" for the rest
rerun_set <- NULL
# rerun_set <- c("Butte", "Fresno")

if (!is.null(rerun_set)) {
  cat("USING RERUN SET!\n")
  print(rerun_set)
}


omicron_recovered_booster_scale <- 0.8 #those with omicron infection 20% less likely to get boosted
omicron_trans_multiplier <- 1.3 #vs delta - based on LSHTM

frac_incidental_delta <- 0.25 #UK
frac_incidental_omicron <- 0.4 #UK

hosp_frac_omicron_0 <- 0.004
case_frac_omicron_0 <- 0.02
omicron_growth <- 0.27

initial_omicron_fraction <- 0.07
start_date <- as.Date("2021/12/7")
day0 <- start_date - 1
end_date <- county.dt[!is.na(hosp.conf), max(date) + 60]
nt <- end_date - start_date + 1
initial_conditions <- readRDS("Inputs/initial_conditions.rds")

GetBoosters <- function(county1) {
  booster_past1 <- boosters_past[county == county1 & date >= start_date]
  last_booster_date <- booster_past1[, max(date)]
  last_booster_index <- as.numeric(last_booster_date - day0)

  booster_future <- boosters[county == county1, booster_future]
  booster_current <- boosters[county == county1, booster_current]

  booster_future_dt <- data.table(t = 1:(nt - last_booster_index))
  booster_daily_change <- boosters[county == county1, daily_change]
  booster_future_dt[, num_boosters := booster_current * booster_daily_change ^ t]

  booster_future_dt[cumsum(num_boosters) > booster_future, num_boosters := 0]

  num_boosters <- c(booster_past1[, booster_doses], booster_future_dt[, num_boosters])
  stopifnot(length(num_boosters) == nt)
  return(num_boosters)
}

RunCounty <- function(county1) {
  population <- readRDS("Inputs/county_population.rds")[county1]

  # num_boosters <- GetBoosters(county1)
  num_boosters <- rep(0, nt)

  vax <- initial_conditions[[county1]]$vax
  delta_forecast <- initial_conditions[[county1]]$delta_forecast

  initial_total_infected <- delta_forecast[date == start_date, infected]
  initial_infected <- initial_omicron_fraction * initial_total_infected

  infected <- delta_forecast[date == start_date, totalCases] / population
  waned_infected <- delta_forecast[date == (start_date - 60), totalCases] / population
  fresh_infected <- infected - waned_infected

  VE_list <- CalcVE(vax, fresh_infected, waned_infected)

  VE_infection <- VE_list$VE_infection
  VE_infection_delta <- VE_list$VE_infection_delta
  VE_severe_given_infection <- VE_list$VE_severe_given_infection

  frac_hosp_lemma <- 0.03

  booster_VE_infection <- VE_list$booster_VE_infection
  booster_VE_severe_given_infection <- VE_list$booster_VE_severe_given_infection

  frac_hosp_lemma <- frac_hosp_lemma / (1 + frac_incidental_delta)

  delta <- county.dt[county == county1 & date >= as.Date("2021/12/1") & date <= as.Date("2021/12/7"), mean(hosp.conf)]

  holiday_set <- c(seq.Date(as.Date("2021/12/24"), as.Date("2022/1/1"), by = "day"), as.Date(c("2022/4/16", "2022/4/17", "2022/5/28", "2022/5/29", "2022/5/30", "2022/7/2", "2022/7/3", "2022/7/4", "2022/9/3", "2022/9/4", "2022/9/5", "2022/10/28", "2022/10/29", "2022/10/31", "2022/11/24", "2022/11/25", "2022/11/26", "2022/11/27")), seq.Date(as.Date("2022/12/24"), as.Date("2023/1/1"), by = "day"))
  t_holiday <- as.numeric(holiday_set - day0)
  nholiday <- length(t_holiday)
  stopifnot(uniqueN(t_holiday) == nholiday) #if a holiday is repeated it will multiply by beta twice
  mu_beta_holiday <- 1.2
  sigma_beta_holiday <- 0.1

  input_file <- "Inputs/CAcounties.xlsx"
  sheets <- LEMMA:::ReadInputs(input_file)
  inputs <- LEMMA:::ProcessSheets(sheets)

  #in LEMMA.stan, hosp_frac_omicron_0/case_frac_omicron_0 are misnamed hosp_frac_delta_0/case_frac_delta_0
  inputs$omicron <- c(as.list(data.table(frac_hosp_lemma, VE_infection, VE_infection_delta, VE_severe_given_infection_0 = VE_severe_given_infection, omicron_recovered_booster_scale, booster_VE_infection, booster_VE_severe_given_infection, frac_incidental_omicron, hosp_frac_delta_0 = hosp_frac_omicron_0, case_frac_delta_0 = case_frac_omicron_0, omicron_growth, nholiday, mu_beta_holiday, sigma_beta_holiday)), list(num_boosters = num_boosters, t_holiday = t_holiday))

  inputs$obs.data <- county.dt[county == county1 & date > start_date]
  inputs$obs.data[date == min(date), cases.conf := NA]

  inputs$model.inputs$total.population <- population
  inputs$internal.args$simulation.start.date <- day0
  inputs$model.inputs$end.date <- end_date

  inputs$interventions[.N, t_inter := county.dt[!is.na(hosp.conf), max(date) + 1]]

  delta_cases <- county.dt[county == county1 & date >= as.Date("2021/12/1") & date <= as.Date("2021/12/7"), mean(cases.conf, na.rm = T)]
  inputs$params <- rbind(inputs$params,
                         data.table(name = "hosp_delta", mu = delta, sigma = delta * 0.3),
                         data.table(name = "cases_delta", mu = delta_cases, sigma = delta_cases * 0.3))
  inputs$internal.args$intial_infected <- initial_infected

  inputs$internal.args$info <- county1
  r <- LEMMA:::CredibilityInterval(inputs)
  z <- extract(r$fit.extended)
  out <- NULL
  for (q in c(0.05, 0.5, 0.95)) {
    forecast_dt <- data.table(hosp_census_with_covid = colQuantiles(z$sim_data_with_error[, 1, ], probs = q),
                         cases = colQuantiles(z$sim_data_with_error[, 2, ], probs = q),
                         Rt = colQuantiles(z$Rt, probs = q))

    out <- rbind(out, data.table(county = county1, quantile = q, date = day0 + (1:nt), forecast_dt))
  }
  return(out)
}

library(ParallelLogger)
logfile <- "Temp/logger.txt"
unlink(logfile)
clearLoggers()
addDefaultFileLogger(logfile)

if (is.null(rerun_set)) {
  forecast_list <- sapply(county_set, RunCounty, simplify = F)
} else {
  #Try fixing convergence problem by setting 10% of dates at least 60 days ago to NA
  county.dt[date > as.Date("2022/2/1") & date < (max(date) - 60) & runif(.N) < 0.1, c("hosp.conf", "hosp.pui", "cases.conf", "cases.pui") := list(NA, NA, NA, NA)]

  forecast_list <- readRDS("Temp/forecast_list.rds")
  for (i in rerun_set) {
    forecast_list[[i]] <- RunCounty(county1 = i)
  }
}

ParallelLogger::logInfo("done")
unregisterLogger(1)

hosp_dt <- rbindlist(forecast_list)
hosp_dt[, hosp_census_for_covid := hosp_census_with_covid * (1 - frac_incidental_omicron)]

hosp_dt <- rbind(hosp_dt, hosp_dt[, .(county = "Total", hosp_census_with_covid = sum(hosp_census_with_covid), hosp_census_for_covid = sum(hosp_census_for_covid), cases = sum(cases), Rt = NA_real_), by = c("date", "quantile")])

pop <- readRDS("Inputs/county_population.rds")
pop_dt <- data.table(county = names(pop), pop)
hosp_dt <- merge(hosp_dt, pop_dt, by = "county", all.x = T)
setkey(hosp_dt, county, date)
hosp_dt[county !=  "Total", weight := pop / sum(pop), by = c("date", "quantile")]
total_rt <- hosp_dt[county != "Total", sum(Rt * weight), by = c("date", "quantile")]$V1
hosp_dt[county == "Total", Rt := total_rt]
hosp_dt$pop <- hosp_dt$weight <- NULL

fwrite(hosp_dt[, .(date, county, quantile, hosp_census_with_covid, hosp_census_for_covid, cases, Rt)], "Forecasts/All_CA_county_Forecasts.csv")

saveRDS(forecast_list, "Temp/forecast_list.rds")







