Get1 <- function(zz) {
  stopifnot(uniqueN(zz) == 1)
  zz[1]
}

GetCountySheets <- function(county1, county.dt, doses.dt) {

  county.dt1 <- county.dt[county == county1, .(date, hosp.conf, hosp.pui, icu.conf, icu.pui,  deaths.conf, deaths.pui, admits.conf, admits.pui, cases.conf, cases.pui, seroprev.conf, seroprev.pui)]

  input.file <- "Inputs/CAcounties.xlsx"

  sheets <- LEMMA:::ReadInputs(input.file)

  sheets$Data <- county.dt1
  if (county1 %in% c("San Francisco", "SFresidents")) {
    #add UeS cases
    ues <- c(34, 39, 43, 45, 0, 50, 0, 57, 53, 44, 36, 0, 37, 0, 35, 43, 31, 23, 0, 38, 0, 0, 0, 0, 20, 0, 0, 0, 14, 16, 11, 19, 0, 0, 0, 12, 9, 10, 7, 0, 0, 0, 7, 12, 4, 7, 0, 0, 0, 5, 9, 2, 7, 0, 0, 0, 5, 1, 1, 2, 0, 0, 0, 7, 7, 3, 3, 0, 0, 0, 3, 1, 1, 2, 0, 0, 0, 0, 1, 2, 2, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 2, 1, 1, 3, 0, 0, 0, 1, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 2, 0, 2, 1, 0, 0, 0, 2, 0, 1, 1, 0, 0, 0, 0, 0, 1, rep(0, 25), 2, 0, 3)

    ues.dt <- data.table(date = as.Date("2021/1/10") + (1:length(ues)) - 1, ues)
    sheets$Data <- merge(sheets$Data, ues.dt, by = "date", all.x = T)
    sheets$Data[, ues := frollmean(ues, 7)]
    sheets$Data[!is.na(ues), cases.conf := ues + cases.conf]
    sheets$Data$ues <- NULL
  }

  is.state <- nchar(county1) == 2
  if (is.state) {
    pop <- readRDS("Inputs/state population by age.rds")[county1, ]
  } else {
    pop <- readRDS("Inputs/county population by age.rds")[county1, ]
  }
  population <- data.table(pop)
  #convert census age categories to CDC/vaccine eligibility age categories - for simplicity use CDC 12-15 = census 10-14, CDC 16-30 = census 15-30
  #CDC:    0  5 12 16                30    40    50          65       75    85
  #census: 0  5 10 15 18 20 21 22 25 30 35 40 45 50 55 60 62 65 67 70 75 80 85
  population$age <- c(0, 5, 12, 16, 16, 16, 16, 16, 16, 30, 30, 40, 40, 50, 50, 50, 50, 65, 65, 65, 75, 75, 85)
  sheets$`Vaccine Distribution`$pop <- population[, .(pop = sum(pop)), by = "age"]$pop

  #no updated vaccinations by age and county, assume same distribution as SF but adjust for county age distribution
  sf_pop_weights <- c(0.045, 0.036, 0.034, 0.21, 0.202, 0.141, 0.181, 0.082, 0.045, 0.024)
  sf_vax_weights <- sheets$`Vaccine Distribution`$dose_proportion
  county_weights <- sheets$`Vaccine Distribution`$pop / sum(sheets$`Vaccine Distribution`$pop) * sf_vax_weights / sf_pop_weights
  sheets$`Vaccine Distribution`$dose_proportion <- county_weights / sum(county_weights)

  doses_actual <- doses.dt[county == county1]
  sheets$`Vaccine Doses - Observed` <- doses_actual

  over12.pop.county <- sum(sheets$`Vaccine Distribution`$pop[-1:-2])
  over12.pop.sf <- 759567

  doses.scale <- over12.pop.county / over12.pop.sf  #scale doses to SF population
  #rescale doses_per_day_base, doses_per_day_increase, doses_per_day_maximum - this is clunky because value is a list
  for (i in c("doses_per_day_base", "doses_per_day_increase", "doses_per_day_maximum")) {
    index <- sheets$`Vaccine Doses - Future`[, which(internal.name == i)]
    rescaled.mrna <- unlist(sheets$`Vaccine Doses - Future`[index, mrna]) * doses.scale
    rescaled.jj <- unlist(sheets$`Vaccine Doses - Future`[index, jj]) * doses.scale
    sheets$`Vaccine Doses - Future`[index, mrna := rescaled.mrna]
    sheets$`Vaccine Doses - Future`[index, jj := rescaled.jj]
  }

  uptake.sf <- doses.dt[county == "San Francisco", sum(dose1) + sum(doseJ)] / over12.pop.sf
  uptake.county <- doses.dt[county == county1, sum(dose1) + sum(doseJ)] / over12.pop.county
  uptake.scale <- uptake.county / uptake.sf
  sheets$`Vaccine Distribution`[, vax_uptake := pmin(1, vax_uptake * uptake.scale)]

  if (is.state) {
    #no hosp data before 7/15 so use all death data
    sheets$Data <- sheets$Data[date >= as.Date("2020/3/1")]
  } else {
    date1 <- as.Date("2020/10/1")
    sheets$Data[date <= date1, icu.conf := NA]
    sheets$Data[date <= date1, icu.pui := NA]
    sheets$Data[date <= date1, deaths.conf := NA]
    sheets$Data[date <= date1, deaths.pui := NA]
  }

  return(sheets)
}

ModifyCountyInputs <- function(county1, inputs) {
  #need different initial conditions to converge
  if (county1 == "Siskiyou") {
    inputs$internal.args$init_frac_mort_nonhosp <- 0.00001
  }
  if (county1 %in% c("Humboldt", "El Dorado", "Del Norte", "Yuba", "Napa", "Lassen", "Nevada", "Riverside")) {
    inputs$internal.args$init_frac_mort_nonhosp <- 0.001
  }
  if (county1 %in% c("Madera", "Marin")) {
    inputs$params[name == "duration_rec_mild", sigma := 0.1]
  }
  if (county1 == "Imperial") {
    inputs$obs.data <- rbind(data.table(date = as.Date("2020/3/10"), hosp.conf = 0, hosp.pui = 0), inputs$obs.data, fill = TRUE)
    inputs$obs.data[, admits.conf := NA_real_]
    inputs$obs.data[, admits.pui := NA_real_]
  }
  if (county1 %in% c("San Bernardino", "Riverside", "Tulare")) {
    inputs$obs.data[as.numeric(date - as.Date("2020/1/1"), "days") %% 3 > 0, cases.conf := NA]
  }
  if (county1 %in% c("Tulare","Madera", "Modoc", "Lassen", "Plumas", "Inyo", "Siskiyou", "San Bernardino", "Del Norte", "Fresno", "Calaveras", "Tuolumne", "Stanislaus", "Butte", "Shasta", "Sacramento", "Merced", "San Luis Obispo")) {
    inputs$interventions[2:17, sigma_beta_inter := 0.01]
  }
  if (county1 %in% c("Kern")) {
    inputs$interventions[2:19, sigma_beta_inter := 0.01]
  }
  if (county1 %in% c("Riverside")) {
    inputs$interventions[9:17, sigma_beta_inter := 0.005]
  }
  if (county1 %in% c("Siskiyou")) {
    inputs$interventions[1, sigma_beta_inter := 0.01]
  }

  if (county1 %in% c("Inyo")) {
    inputs$obs.data[date <= as.Date("2020/5/1"), hosp.conf := NA]
    inputs$obs.data[date <= as.Date("2020/5/1"), hosp.pui := NA]
  }
  inputs$internal.args$weights <- c(1, 1, 1, 1, 0.5, 1)
  return(inputs)
}
