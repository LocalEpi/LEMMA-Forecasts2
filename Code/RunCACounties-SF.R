setwd("~/Documents/GitHub/LEMMA-Forecasts/")
library(data.table)
devtools::load_all("LEMMA.forecasts")

#booster uptake for omicron SIR.R is not working due to changes in dataset

if (F) {
  chdr_date <- Sys.Date() - 1
  d <- GetFracTransfer(format(chdr_date, "%Y%m%d") )
  write.csv(d, "~/Documents/MissionCovid/SF frac transfer.csv", row.names = F)
  aws.s3::put_object("~/Documents/MissionCovid/SF frac transfer.csv", bucket = "js-lemma-bucket1", region = "us-west-1")
}

#in terminal:
# tail -n 100 -f ~/Documents/OmicronSim/logger.txt

#either get savedCountyData.rds from last commit or use GitHub desktop to discard any changes to savedCountyData.rds
# system2("git", args = c("checkout", "master~1"))
prev_county.dt <- readRDS("Inputs/savedCountyData.rds")
# system2("git", args = c("checkout", "master"))

last_obs_hosp_date <- prev_county.dt[!is.na(hosp.conf), Get1(max(date))]
last_obs_case_date <- prev_county.dt[!is.na(cases.conf), Get1(max(date))]

if (T) {
  county.dt <- GetCountyData()

  # date1 <- as.Date("2021/6/1")
  # date2 <- county.dt[!is.na(deaths.conf), max(date)]
  # county.dt[date < (date2 - 30) & as.numeric(date - as.Date("2020/1/1"), "days") %% 30 > 0, deaths.conf := NA]
  # county.dt[date < date1, icu.conf := NA]
  # county.dt[date < date1, icu.pui := NA]
  # county.dt[date < date1, cases.conf := NA]

  county.dt <- RemoveTransfers(county.dt)


  county.dt[date > as.Date("2021/12/7") & date < as.Date("2022/8/15"), cases.conf := NA_real_]

  # county.dt[date >= as.Date("2021/11/21") & date <= as.Date("2021/12/1"), cases.conf := NA]; cat("Cases disabled 11/21 to 12/1\n")

  max_date_dt <- county.dt[!is.na(hosp.conf), .(max_date = max(date)), by = "county"]
  if (max_date_dt[, uniqueN(max_date) > 1]) {
    cat("non-unique max date with non-NA hosp:\n")
    print(max_date_dt)
  }
  max_date <- max_date_dt[, max(max_date)]
  cat("max_date = ", as.character(max_date), "\n")

  county.dt <- county.dt[date < max_date] #last day may have reporting delay

  saveRDS(county.dt, "Inputs/savedCountyData.rds") #save in case HHS server is down later

  deaths.cases <- data.table::fread("https://data.chhs.ca.gov/dataset/f333528b-4d38-4814-bebb-12db1f10f535/resource/046cdd2b-31e5-4d34-9ed3-b48cdbc4be7a/download/covid19cases_test.csv")
  setnames(deaths.cases, names(deaths.cases), tolower(names(deaths.cases)))
  deaths.cases[, date := as.Date(date, format = "%m/%d/%Y")]
  deaths.cases[, county := area]
  data.table::setkey(deaths.cases, county, date)
  saveRDS(deaths.cases, paste0("~/Documents/OmicronSim/case_data_", Sys.Date(), ".rds"))
  cases_dt <- deaths.cases[, .(county, date, day = weekdays(date), cases, positive_tests, total_tests, positivity = positive_tests / total_tests)]
  cases_dt[, note := ""]
  cases_dt[date == county.dt[!is.na(cases.conf), max(date)], note := "**latest data used"]

  deaths.cases.sf <- deaths.cases[county == "San Francisco", .(date, day = weekdays(date), cases, cases7=frollmean(cases, 7), total_tests, total_tests_7 = frollmean(total_tests, 7), positivity = positive_tests / total_tests, positivity_7 = frollsum(positive_tests, 7) / frollsum(total_tests, 7), positive_tests, positive_tests7 = frollmean(positive_tests, 7))]
  deaths.cases.sf[, note := ""]
  deaths.cases.sf[date == county.dt[!is.na(cases.conf), max(date)], note := "**latest data used"]
  print(tail(deaths.cases.sf, 22), digits = 2)

  max.case.date <- county.dt[!is.na(cases.conf), max(date)]
  print(ggplot(deaths.cases.sf[date >= as.Date("2021/7/1") & date <= max.case.date], aes(x = date, y = positivity_7)) + geom_line() + ggtitle("San Francisco"))
  deaths.cases.ca <- deaths.cases[county == "California", .(date, day = weekdays(date), cases, cases7=frollmean(cases, 7), total_tests, total_tests_7 = frollmean(total_tests, 7), positivity = positive_tests / total_tests, positivity_7 = frollsum(positive_tests, 7) / frollsum(total_tests, 7), positive_tests, positive_tests7 = frollmean(positive_tests, 7))]
  deaths.cases.ca[, note := ""]
  deaths.cases.ca[date == county.dt[!is.na(cases.conf), max(date)], note := "**latest data used"]
  print(tail(deaths.cases.ca, 22), digits = 2)
  print(ggplot(deaths.cases.ca[date >= as.Date("2021/7/1") & date <= max.case.date], aes(x = date, y = positivity_7)) + geom_line() + ggtitle("California"))
} else {
  cat("using saved county data\n")
  county.dt <- readRDS("Inputs/savedCountyData.rds")
}

FitExp <- function(dt_orig, num.days = 7) {
  dt <- copy(dt_orig)
  stopifnot(ncol(dt) == 2)
  names(dt) <- c("d", "y")
  if (num.days == 0) {
    return(ggplot(dt, aes(x = d)) + geom_point(aes(y = y), na.rm = T) + scale_y_log10() + xlab(""))
  }
  m <- lm(log(y) ~ d, data = tail(dt[!is.na(y)], num.days))
  dt <- rbind(dt, data.table(d = max(dt$d) + 1:20, y = NA))
  dt$pred_y <- exp(predict(m, newdata = dt))
  dt[pred_y < min(y, na.rm = T), pred_y := NA_real_]
  dt[d < max(dt_orig$d) & pred_y > max(y, na.rm = T), pred_y := NA_real_]
  ggplot(dt, aes(x = d)) + geom_point(aes(y = y), na.rm = T) + geom_line(aes(y = pred_y), na.rm = T) + scale_y_log10(n.breaks=10) + xlab("")
}

print(FitExp(county.dt[date >= as.Date("2021/6/10") & !is.na(cases.conf), .(cases = sum(cases.conf)), by = "date"]) + ggtitle("State Cases") + ylab("Weekday cases (log scale)"))

print(FitExp(county.dt[date >= as.Date("2021/12/1") & !is.na(cases.conf), .(cases = sum(cases.conf)), by = "date"]) + ggtitle("State Cases") + ylab("Weekday Cases (log scale)"))

print(FitExp(county.dt[date >= as.Date("2021/6/10") & !is.na(hosp.conf), .(hosp = sum(hosp.conf)), by = "date"]) + ggtitle("State Hosp") + ylab("Hospital Census (log scale)"))

print(FitExp(county.dt[date >= as.Date("2021/12/1") & !is.na(hosp.conf), .(hosp = sum(hosp.conf)), by = "date"]) + ggtitle("State Hosp") + ylab("Hospital Census (log scale)"))

print(FitExp(county.dt[date >= as.Date("2021/6/10") & county == "SFresidents", .(date, cases.conf)]) + ggtitle("SF Cases") + ylab("Weekday Cases (log scale)"))

print(FitExp(county.dt[date >= as.Date("2021/12/1") & county == "SFresidents", .(date, cases.conf)]) + ggtitle("SF Cases") + ylab("Weekday Cases  (log scale)"))

print(FitExp(deaths.cases.sf[date >= as.Date("2021/12/1") & date <= (max(date, na.rm=T) -4), .(date, positive_tests7)]) + ggtitle("SF Positive Tests") + ylab("7 day avg positive tests (log scale)"))

print(FitExp(deaths.cases.ca[date >= as.Date("2021/5/1") & date <= (max(date, na.rm=T) -4), .(date, positive_tests7)]) + ggtitle("CA Positive Tests") + ylab("7 day avg positive tests (log scale)"))

print(FitExp(county.dt[date >= as.Date("2021/6/10") & county == "SFresidents", .(date, hosp.conf)]) + ggtitle("SF Hosp (residents)") + ylab("Hospital Census (log scale)"))

print(FitExp(county.dt[date >= as.Date("2021/12/1") & county == "SFresidents", .(date, hosp.conf)]) + ggtitle("SF Hosp (residents)") + ylab("Hospital Census (log scale)"))

print(FitExp(county.dt[date >= as.Date("2021/12/1") & county == "SFresidents_and_nonresidents", .(date, hosp.conf)]) + ggtitle("SF Hosp (residents and nonresidents)") + ylab("Hospital Census (log scale)"))

print(tail(county.dt[county == "SFresidents_and_nonresidents", .(date, county, hosp.conf, hosp.pui, icu.conf, icu.pui, cases.conf)], 20))

print(tail(county.dt[county == "SFresidents", .(date, county, hosp.conf, hosp.pui, icu.conf, icu.pui, cases.conf)], 20))

#needs updating
#source("~/Dropbox (UC Berkeley Biostat)/LEMMA_shared/JS code branch/booster uptake for omicron SIR.R")


county_set <- setdiff(county.dt[, county], c("SFresidents_and_nonresidents", "Lassen", "Plumas", "Glenn", "Mono", "Modoc", "Inyo", "Del Norte", "Trinity", "Mariposa"))
county.pop <- rowSums(readRDS("Inputs/county population by age.rds"))
county_set <- names(sort(county.pop[county_set], decreasing = T))
# hosp_dt <- fread("Forecasts/All_CA_county_Forecasts.csv")
hosp_dt <- fread("https://raw.githubusercontent.com/LocalEpi/LEMMA-Forecasts/master/Forecasts/All_CA_county_Forecasts.csv")

source("~/Dropbox (UC Berkeley Biostat)/LEMMA_shared/JS code branch/omicron plots - all counties3 - with actuals.R")
PlotOmicron(hosp_dt, diagnostic = T, intersect(county_set, hosp_dt[, county]), zoom = T, last_obs_hosp_date =last_obs_hosp_date , last_obs_case_date = last_obs_case_date, dates_in = c(as.Date("2021/12/20"), as.Date(NA)))


source("~/Dropbox (UC Berkeley Biostat)/LEMMA_shared/JS code branch/omicron SIR3.R")
stopifnot(run_all)
saveRDS(sir_list[["SFresidents"]], file = "Misc/SFresidents_detailed_outputs.rds")

PlotOmicron(hosp_dt, diagnostic = F, county_set, filestr_in = "Misc/All_CA_county_Forecasts2.pdf" , incidental_set_in = T)

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
fwrite(effective_contact_rate_multiplier, file = "Misc/effective_contact_rate_multipliers.csv")

#source("~/Dropbox (UC Berkeley Biostat)/LEMMA_shared/JS code branch/fit reported cases.R")
source("~/Dropbox (UC Berkeley Biostat)/LEMMA_shared/JS code branch/omicron regions.R")
# source("~/Dropbox (UC Berkeley Biostat)/LEMMA_shared/JS code branch/counties of concern.R")
concern <- NULL
source("~/Dropbox (UC Berkeley Biostat)/LEMMA_shared/JS code branch/omicron email.R")

PlotOmicron(hosp_dt, county_set = intersect(county_set, hosp_dt[, county]), write_pdf = T, plot_icu_rt = T, diagnostic = T, filestr_in = "~/Documents/OmicronSim/all CA incl ICU.pdf")

push <- readline("Ready to commit and push forecasts? (y/n) ")
if (push == "y") {
  cat("max date = ", as.character(max_date), "\n")
  system2("git", args = "pull")
  system2("git", args = c("add", "-A"))
  commit.name <- paste0('"', "data through ", as.character(max_date), '"')
  commit.output <- system2("git", args = c('commit', '-a', '-m', commit.name), stderr = T, stdout = T)


  system2("git", args = "push")
  cat("Delete MissionCovid files\n")


  source("~/Dropbox (UC Berkeley Biostat)/LEMMA_shared/JS code branch/previous SF forecasts2.R")
}
