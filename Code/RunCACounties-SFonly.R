setwd("~/Documents/GitHub/LEMMA-Forecasts/")
library(data.table)
devtools::load_all("LEMMA.forecasts")

if (T) {
  chdr_date <- Sys.Date() - 2#1
  d <- GetFracTransfer(format(chdr_date, "%Y%m%d") )
  write.csv(d, "~/Documents/MissionCovid/SF frac transfer.csv", row.names = F)
  aws.s3::put_object("~/Documents/MissionCovid/SF frac transfer.csv", bucket = "js-lemma-bucket1", region = "us-west-1")
}

#in terminal:
# tail -n 100 -f ~/Documents/OmicronSim/logger.txt

prev_county.dt <- readRDS("Inputs/savedCountyData.rds")

last_obs_hosp_date <- prev_county.dt[!is.na(hosp.conf), Get1(max(date))]
last_obs_case_date <- prev_county.dt[!is.na(cases.conf), Get1(max(date))]

county.dt <- readRDS("Inputs/savedCountyData.rds")[county == "SFresidents_and_nonresidents"]
county.dt$frac_hosp_transfer <- county.dt$frac_icu_transfer <- NULL
county.dt <- merge(county.dt, d[date >= county.dt[, min(date)] & date <= chdr_date, .(date, hosp_chdr)], by = "date", all = T)
county.dt[, hosp.conf := as.double(hosp_chdr)]
county.dt$hosp_chdr <- NULL
county.dt[, county := "San Francisco"]
county.dt <- RemoveTransfers(county.dt)


source("~/Dropbox (UC Berkeley Biostat)/LEMMA_shared/JS code branch/booster uptake for omicron SIR.R")

source("~/Dropbox (UC Berkeley Biostat)/LEMMA_shared/JS code branch/omicron SIR3.R")
saveRDS(sir_list[[1]], file = "Misc/SFresidents_detailed_outputs.rds")


effective_contact_rate_multiplier <- sapply(r_list, function (z) {
  b <- z$beta_multiplier
  if (is.matrix(b)) {
    unname(b[, "50%"])
  } else {
    b
  }
})
effective_contact_rate_multiplier <- t(effective_contact_rate_multiplier)
colnames(effective_contact_rate_multiplier) <- as.data.table(read_excel("Inputs/CAcounties.xlsx", sheet = "Interventions", skip = 2))[, new_var]
effective_contact_rate_multiplier <- data.table(county = rownames(effective_contact_rate_multiplier), effective_contact_rate_multiplier)
print(effective_contact_rate_multiplier, digits = 3)
#fwrite(effective_contact_rate_multiplier, file = "Misc/effective_contact_rate_multipliers.csv")


# source("~/Dropbox (UC Berkeley Biostat)/LEMMA_shared/JS code branch/previous SF forecasts2.R")

hosp_dt_sf <- copy(hosp_dt[county == "SFresidents"])
actual_dt <- county.dt[county == "SFresidents_and_nonresidents", .(date, hosp = hosp.conf + pui_frac * hosp.pui)]
dt <- merge(hosp_dt_sf, actual_dt[date >= (max(date) - 14)], by = "date")

f <- function(total_over_res) {
  dt[, sum((hosp_census_with_covid * total_over_res - hosp) ^ 2)]
}
opt <- optimize(f, c(1, 2))
total_over_res <- opt$minimum
cat("total_over_res = ", total_over_res, "\n")
hosp_dt_sf[, scenario := QuantileToScenario(quantile)]
hosp_dt_sf[, county := "SFresidents_and_nonresidents"]
hosp_dt_sf[, hosp_census_with_covid := hosp_census_with_covid * total_over_res]
hosp_dt_sf[, hosp_census_for_covid := hosp_census_for_covid * total_over_res]
hosp_dt <- rbind(hosp_dt[county != "SFresidents_and_nonresidents"], hosp_dt_sf[, -"scenario"])

PlotOmicron(hosp_dt, diagnostic = F, county_set = c("SFresidents", "SFresidents_and_nonresidents"), filestr_in = "Misc/SF_Forecasts.pdf", incidental_set_in = T, zoom = T)
