library(data.table)
library(readxl)
library(ggplot2)

GetFracTransfer <- function(CHDR_date) {
  cat("CHDR_date = ", CHDR_date, "\n")
  suppressWarnings(x <- as.data.table(read_excel(paste0("~/Documents/MissionCovid/CHDR_Admissions_", CHDR_date, ".xlsx"))))

  x=x[SFHospital == "Yes" & AcuteHospital == "Yes" & SuspectedOrConfirmedAdmissionFlag == "Confirmed"]
  x[, DischargeInstant := as.Date(DischargeInstant)]
  x[, AdmissionInstant := as.Date(AdmissionInstant)]
  x[, LengthOfStayInDays := as.numeric(LengthOfStayInDays)]
  x[, TimeInICUInDays := as.numeric(TimeInICUInDays)]

  x[is.na(DischargeInstant) & DischargeFlag == "Yes", DischargeInstant := AdmissionInstant + LengthOfStayInDays]

  x[, City := toupper(City)]
  x[, transfer := !(COUNTY %in% "SAN FRANCISCO" | City %in% "SAN FRANCISCO")]

  admits <- x[transfer==F, .N, keyby = AdmissionInstant]
  admits <- merge(admits, data.table(AdmissionInstant = seq.Date(admits[, min(AdmissionInstant)], as.Date(CHDR_date, "%Y%m%d") - 1, by = "day")), all = T)
  admits[is.na(N), N := 0]
  admits[, weekday := weekdays(AdmissionInstant)]
  admits[, admits1 := N]
  admits[, admits7 := frollmean(N, 7)]
  print(ggplot(admits[AdmissionInstant >= as.Date("2021/6/1")], aes(x = AdmissionInstant, y = admits1)) + geom_point() + geom_line(aes(y = admits7), na.rm = T) + ylab("SF admits (residents)") + labs(subtitle = "line = 7 day average", caption = "Most recent dates have reporting delay"))
  admits$N <- NULL
  print(tail(admits, 20))

  x[WasInICU == "Yes" & (DischargeFlag == "Yes" | CurrentICU == "No"), ICUDischargeInstant := AdmissionInstant + TimeInICUInDays] #hard to tell when someone leaves ICU if not discharged - if recent AdmissionInstant + TimeInICUInDays can be a few days ago

  print(x[DischargeFlag=="No", .(total = .N, exclTransfers = sum(!transfer)), by = OrganizationName])
  print(x[DischargeFlag=="No", .(total = .N, exclTransfers = sum(!transfer))])
  dt <- NULL
  date_set <- seq.Date(as.Date("2020/3/1"), Sys.Date(), by = "day")
  for (i in seq_along(date_set)) {
    d <- date_set[i]
    hosp <- x[AdmissionInstant < d & (is.na(DischargeInstant) | DischargeInstant > d), .N]
    hosp_transfer <- x[AdmissionInstant < d & (is.na(DischargeInstant) | DischargeInstant > d) & transfer, .N]
    icu <- x[WasInICU == "Yes" & AdmissionInstant < d & (is.na(ICUDischargeInstant) | ICUDischargeInstant > d), .N]
    icu_transfer <- x[WasInICU == "Yes" & AdmissionInstant < d & (is.na(ICUDischargeInstant) | ICUDischargeInstant > d) & transfer, .N]
    dt <- rbind(dt, data.table(date = d, hosp, hosp_transfer, icu, icu_transfer))
  }

  dt[, frac_hosp_transfer := hosp_transfer / hosp]
  dt[, frac_icu_transfer := 0.9 * icu_transfer / icu] #0.9 because it seems like I'm overestimating icu transfers based on "CHDR transfers temp"
  dt[, county := "SFresidents"]

  dt <- dt[, .(date, county, frac_hosp_transfer, frac_icu_transfer, hosp_chdr = hosp)]
  return(dt)
}

RemoveTransfers <- function(county.dt) {
  max_date <- county.dt[!is.na(hosp.conf), max(date)]
  frac_transfer_dt <- ReadCsvAWS("SF frac transfer.csv")
  frac_transfer_dt <- frac_transfer_dt[date <= max_date]
  frac_transfer_dt[, date := as.Date(date)]

  if (frac_transfer_dt[, max(date)] < max_date) {
    nrep <- frac_transfer_dt[, as.numeric(max_date - max(date))] #number of rows to fill in with last available data
    frac_transfer_dt <- rbind(frac_transfer_dt, frac_transfer_dt[rep(.N, nrep)])
    frac_transfer_dt[(.N - nrep + 1):.N, date := seq.Date(max_date - nrep + 1, max_date, by = "day")]
  }

  county.dt.sf <- county.dt[county == "San Francisco"]
  county.dt.sf[, county := "SFresidents"]
  county.dt <- rbind(county.dt, county.dt.sf)

  county.dt <- merge(county.dt, frac_transfer_dt, by = c("date", "county"), all = T)

  county.dt[!is.na(frac_hosp_transfer), hosp.conf := hosp.conf * (1 - frac_hosp_transfer)]
  county.dt[!is.na(frac_hosp_transfer), hosp.pui := hosp.pui * (1 - frac_hosp_transfer)]
  county.dt[!is.na(frac_icu_transfer), icu.conf := icu.conf * (1 - frac_icu_transfer)]
  county.dt[!is.na(frac_icu_transfer), icu.pui := icu.pui * (1 - frac_icu_transfer)]

  county.dt[county == "San Francisco", county := "SFresidents_and_nonresidents"]
  return(county.dt)
}


