setwd("~/Documents/GitHub/LEMMA-Forecasts2/")

county_set <- c("Alameda", "Amador", "Butte", "Contra Costa", "Fresno", "Humboldt",
                "Imperial", "Kern", "Kings", "Lake", "Los Angeles", "Madera",
                "Marin", "Mendocino", "Merced", "Monterey", "Napa", "Nevada",
                "Orange", "Placer", "Riverside", "San Francisco",
                "Sacramento", "San Bernardino", "San Diego", "San Joaquin", "San Luis Obispo",
                "San Mateo", "Santa Barbara", "Santa Clara", "Santa Cruz", "Shasta",
                "Solano", "Sonoma", "Stanislaus", "Tehama", "Tulare",
                "Tuolumne", "Ventura", "Yolo", "Yuba")
county_set <- "Yolo" #temp!
if (F) {
  county.dt <- GetCountyData()
} else {
  county.dt <- readRDS("Inputs/savedCountyData.rds")
  county.dt <- county.dt[date < as.Date("2022/3/1")] #temp!
}

#devtools::install_github("LocalEpi/LEMMA")

rerun_set <- NULL #If this is not empty, run only this set, use "~/Documents/OmicronSim/sirlist.rds" for the rest #use NULL for no

if (!is.null(rerun_set)) {
  cat("USING RERUN SET!\n")
  print(rerun_set)
}

library(data.table)
library(ggplot2)
# library(parallel)
# library(openxlsx)
library(rstan)
library(readxl)
library(LEMMA)
library(matrixStats)
source("Code/MarmVE.R")
source("Code/GetNonconvergeCounties.R")

omicron_recovered_booster_scale <- 0.8 #those with omicron infection 20% less likely to get boosted
omicron_trans_multiplier <- 1.3 #vs delta - based on LSHTM

frac_incidental_delta <- 0.25 #UK
frac_incidental_omicron <- 0.4 #UK

hosp_frac_omicron_0 <- 0.004
case_frac_omicron_0 <- 0.02
omicron_growth <- 0.27



min_infected_fraction <- 1e-5
initial_omicron_fraction <- 0.07
start_date <- as.Date("2021/12/7")
day0 <- start_date - 1
start_fit_date <- start_date
end_date <- county.dt[!is.na(hosp.conf), max(date) + 60]
nt <- end_date - start_date + 1

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

RunCountySIR <- function(county1) {
  population <- readRDS("Inputs/county_population.rds")[county1]

  # num_boosters <- GetBoosters(county1)
  num_boosters <- rep(0, nt)

  vax <- readRDS(paste0("~/Documents/OmicronSim/vax_", county1, ".rds"))
  lemma_forecast <- lemma_forecast_list[[county1]]

  initial_total_infected <- lemma_forecast[date == start_date, infected]
  initial_infected <- initial_omicron_fraction * initial_total_infected

  infected <- lemma_forecast[date == start_date, totalCases] / population
  waned_infected <- lemma_forecast[date == (start_date - 60), totalCases] / population
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

  inputs$obs.data <- county.dt[county == county1 & date > start_fit_date]
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
  browser()
  z <- extract(r$fit.extended)
  out <- NULL
  for (q in c(0.05, 0.5, 0.95)) {
    sir_dt <- data.table(hosp_census_with_covid = colQuantiles(z$sim_data_with_error[, 1, ], probs = q),
                         cases = colQuantiles(z$sim_data_with_error[, 2, ], probs = q))
    out <- rbind(out, data.table(county = county1, quantile = q, date = date_set, sir_dt))
  }

  params <- c("duration_latent", "duration_rec_mild", "duration_pre_hosp", "duration_protection_infection",
              "duration_hosp_mod", "frac_tested", "initial_infected", "severity", "omicron_trans_multiplier", "test_delay", "hosp_delta", "cases_delta", "beta_holiday")
  probs <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
  r_out <- lapply(extract(r$fit.to.data, params), quantile, probs = probs)
  sim_data <- extract(r$fit.extended, c("sim_data"))[[1]]
  r_out$beta_multiplier <- colQuantiles(extract(r$fit.to.data, "beta_multiplier")[[1]], probs = probs)

  beta <- extract(r$fit.extended, "beta")[[1]]
  r_out$beta <- data.table(date = date_set, colQuantiles(beta, probs = probs))
  r_out$Rt <- data.table(date = date_set, colQuantiles(extract(r$fit.extended, "Rt")[[1]], probs = probs))

  r_out$sigma_obs <- colQuantiles(extract(r$fit.to.data, "sigma_obs")[[1]], probs = probs)


  rel_eff <- beta / matrix(beta[, 1], nrow = nrow(beta), ncol = ncol(beta), byrow = F)
  r_out$rel_eff <- data.table(date = date_set, colQuantiles(rel_eff, probs = probs))

  r_out$hosp <- data.table(date = date_set, colQuantiles(sim_data[, 1, ], probs = probs))
  r_out$cases <- data.table(date = date_set, colQuantiles(sim_data[, 2, ], probs = probs))
  r_out$infected <- data.table(date = date_set, colQuantiles(z$x[, 3, ] + z$x[, 4, ], probs = probs))
  r_out$exposed <- data.table(date = date_set, colQuantiles(z$x[, 2, ], probs = probs))
  r_out$recovered <- data.table(date = date_set, colQuantiles(z$x[, 7, ], probs = probs))
  r_out$inputs <- inputs
  return(list(main = out, extra = r_out))
}

ReadForecast <- function(filestr) {
  proj <- as.data.table(read_excel(filestr, "projection"))
  proj[, date := as.Date(date)]
  return(proj)
}

Get1 <- function(zz) {
  stopifnot(uniqueN(zz) == 1)
  zz[1]
}

date_set <- day0 + (1:nt)

lemma_forecast_list <- list()
for (county1 in county_set) {
  lemma_forecast <- ReadForecast(paste0("~/Dropbox/DELTAONLY_Forecasts/DELTAONLY_", county1, ".xlsx"))
  lemma_forecast_list[[length(lemma_forecast_list) + 1]] <- lemma_forecast
}
names(lemma_forecast_list) <- county_set

if (is.null(rerun_set)) {
  sir_list <- lapply(county_set, RunCountySIR)
} else {
  stop("code this")
  #try setting 10% of values more than 2 months ago to NA?
  sir_list <- readRDS("~/Documents/OmicronSim/sirlist.rds")
  names(sir_list) <- county_set #names aren't saved
  for (i in rerun_set) {
    sir_list[[i]] <- RunCountySIR(county1 = i)
  }
}

hosp_dt <- NULL
r_list <- list()
for (i in seq_along(sir_list)) {
  hosp_dt1 <- sir_list[[i]]$main

  #add Rt - this is kind of ugly because Rt was added later
  rt <- melt(sir_list[[i]]$extra$Rt[, .(date, `5%`, `50%`, `95%`)], id.vars = "date", variable.name = "quantile", value.name = "Rt")
  rt[, quantile := as.numeric(sub("%", "", quantile, fixed = T)) / 100]
  hosp_dt1 <- merge(hosp_dt1, rt, by = c("date", "quantile"))

  hosp_dt <- rbind(hosp_dt, hosp_dt1)
  r_list[[i]] <- sir_list[[i]]$extra
}
names(r_list) <- county_set


nonconverge_counties <- GetNonconvergeCounties()
if (length(nonconverge_counties) > 0) {
  cat("The following counties may not have converged:\n")
  print(nonconverge_counties)
  hosp_dt_orig <- copy(hosp_dt)
}



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

saveRDS(hosp_dt, "~/Documents/OmicronSim/hosp_dt.rds")
saveRDS(r_list, "~/Documents/OmicronSim/rlist.rds")
saveRDS(sir_list, "~/Documents/OmicronSim/sirlist.rds")







