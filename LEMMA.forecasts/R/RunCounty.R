GetCountyInputs <- function(county1, county.dt, doses.dt, params) {
  sheets <- GetCountySheets(county1, county.dt, doses.dt)
  change_date <- Sys.Date() #date to increase contacts and change VE
  if (!is.null(params)) {
    stopifnot(setequal(names(params), c("boosters", "contacts")))

    mu_beta_inter <- switch(params$contacts, veryhigh = 2, high = 1.5, mid = 1.25, low = 1.1, stop("unexpected contacts string"))

    sheets$Interventions <- rbind(sheets$Interventions, data.table(
      mu_t_inter = change_date,
      sigma_t_inter = 0.01,
      mu_beta_inter = mu_beta_inter,
      sigma_beta_inter = 0.01,
      mu_len_inter = 40,
      sigma_len_inter = 0.01
    ))

    delta2 <- sheets$Variants[name == "delta"]
    delta2[, name := "delta2"]

    if (params$boosters == "BoostersOutweighWaning") {
      waning_inf <- 1.2
      waning_severe <- 1
    } else if (params$boosters == "WaningOutweighsBoosters") {
      waning_inf <- 0.7
      waning_severe <- 0.97
    } else if (params$boosters == "balanced") {
      waning_inf <- 1
      waning_severe <- 1
    } else {
      stop("unexpected boosters string")
    }

    delta2[, vaccine_efficacy_for_susceptibility_1 := pmin(1, vaccine_efficacy_for_susceptibility_1 * waning_inf)]
    delta2[, vaccine_efficacy_for_susceptibility_2 := pmin(1, vaccine_efficacy_for_susceptibility_2 * waning_inf)]
    delta2[, vaccine_efficacy_for_susceptibility_J := pmin(1, vaccine_efficacy_for_susceptibility_J * waning_inf)]

    delta2[, vaccine_efficacy_against_progression_1 := pmin(1, vaccine_efficacy_against_progression_1 * waning_severe)]
    delta2[, vaccine_efficacy_against_progression_2 := pmin(1, vaccine_efficacy_against_progression_2 * waning_severe)]
    delta2[, vaccine_efficacy_against_progression_J := pmin(1, vaccine_efficacy_against_progression_J * waning_severe)]

    delta2[, variant_day0 := change_date]
    delta2[, frac_on_day0 := sheets$Variants[name == "delta", 0.01 * frac_on_day0 * daily_growth_future ^ (as.numeric(change_date - variant_day0))]]
    delta2[, daily_growth_prior := 10]
    delta2[, daily_growth_future := 1.3]

    sheets$Variants <- rbind(sheets$Variants, delta2)
  }

  inputs <- LEMMA:::ProcessSheets(sheets)
  inputs <- ModifyCountyInputs(county1, inputs)

  return(inputs)
}

RunOneCounty <- function(county1, county.dt, doses.dt, include_scenarios = TRUE) {

  lemma_statusquo <- Scenario("statusquo", county1, county.dt, doses.dt, lemma_statusquo = NULL, params = NULL)$lemma #generates forecast
  if (!include_scenarios) return(lemma_statusquo)


  params_set <- as.data.table(expand.grid(contacts = c("low", "mid", "high", "veryhigh"), boosters = c("BoostersOutweighWaning", "balanced", "WaningOutweighsBoosters"), stringsAsFactors = F))

  results.dt <- NULL
  for (i in 1:nrow(params_set)) {
    filestr <- params_set[i, paste0(boosters, "_", contacts)]
    results.dt1 <- Scenario(filestr, county1, county.dt, doses.dt, lemma_statusquo, params_set[i])$results
    results.dt <- rbind(results.dt, results.dt1)
  }


  if (county1 == "SFresidents") {
    prev.width <- getOption("width")
    options(scipen = 3, width = 9999)

    sink("Scenarios/San Francisco_ScenarioSummary.txt")

    cat("SFresidents:\n")
    print(results.dt, digits = 0)
    saveRDS(results.dt, file = "Scenarios/San Francisco_ScenarioSummary.rds")
    options(width = prev.width)

    # cat("note: Given that contacts have already increased, we consider the veryhigh scenario (contacts double from current) to be unlikely.\n")
    cat("links to pdf and xlsx outputs: https://localepi.github.io/LEMMA/ \n")
    sink()
  }
  return(lemma_statusquo)
}


Scenario <- function(filestr1, county1, county.dt, doses.dt, lemma_statusquo, params) {

  inputs <- GetCountyInputs(county1 = county1, county.dt = county.dt, doses.dt = doses.dt, params = params)

  if (is.null(lemma_statusquo)) {
    #refit
    #inputs$internal.args$output.filestr <- paste0("Forecasts/", county1)
    inputs$internal.args$output.filestr <- paste0("DELTAONLY_Forecasts/DELTAONLY_", county1)
    lemma <- LEMMA:::CredibilityInterval(inputs)
  } else {
    inputs$internal.args$output.filestr <- paste0("Scenarios/", county1, "_", filestr1)
    lemma <- LEMMA:::ProjectScenario(lemma_statusquo, inputs)
  }

  projection1 <- lemma$projection[date >= Sys.Date()]
  hosp.peak <- projection1[, max(hosp)]
  hosp.peak.date <- projection1[, date[which.max(hosp)]]
  admits.byMay1 <- projection1[date <= as.Date("2022/5/1"), sum(admits)]
  deaths.byMay1 <- projection1[date <= as.Date("2022/5/1"), max(deaths) - min(deaths)]
  cases.byMay1 <- projection1[date <= as.Date("2022/5/1"), max(totalCases) - min(totalCases)]
  results <- data.table(filestr1, hosp.peak, hosp.peak.date, admits.byMay1, deaths.byMay1, cases.byMay1)
  return(list(results = results, lemma = lemma))
}
