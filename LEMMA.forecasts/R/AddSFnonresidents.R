AddNonresidents <- function(nonres_dt, orig_path) {
  workbook <- lapply(excel_sheets(orig_path), read_excel, path = orig_path)
  names(workbook) <- excel_sheets(orig_path)
  workbook$note <- data.table(note = c("San Francico is handled differently from other counties",
                                       "All values are from SFresidents.xlsx except hosp and icu add estimated nonresidents"))
  workbook$projection <- as.data.table(workbook$projection)
  workbook$projection[, date := as.Date(date)]
  workbook$projection <- merge(workbook$projection, nonres_dt[, .(date, hosp_nonres_pred, icu_nonres_pred)], by = "date", all.x = T)
  workbook$projection[!is.na(hosp_nonres_pred), hosp := hosp + hosp_nonres_pred]
  workbook$projection[!is.na(icu_nonres_pred), icu := icu + icu_nonres_pred]

  openxlsx::write.xlsx(workbook, file = sub("SFresidents", "San Francisco", orig_path), overwrite = T)
}

CreateSFNonresidents <- function(county.dt) {
  nonres_dt <- merge(county.dt[county == "SFresidents_and_nonresidents"], county.dt[county == "SFresidents"], by = "date", suffixes = c("_total", "_res"))
  nonres_dt <- nonres_dt[, .(date, hosp_nonres = hosp.conf_total - hosp.conf_res, icu_nonres = icu.conf_total - icu.conf_res)]
  nonres_dt <- merge(nonres_dt, ReadForecast("Forecasts/California.xlsx")[, .(hosp_ca = hosp, icu_ca = icu), by = "date"], all = T)

  m_hosp <- lm(hosp_nonres ~ hosp_ca + log(hosp_ca), data = nonres_dt[date > as.Date("2021/2/1")])
  print(m_hosp)
  nonres_dt[, hosp_nonres_pred := pmax(0, predict(m_hosp, newdata = nonres_dt))]

  m_icu <- lm(icu_nonres ~ icu_ca + log(icu_ca), data = nonres_dt[date > as.Date("2021/2/1")])
  print(m_icu)
  nonres_dt[, icu_nonres_pred := pmax(0, predict(m_icu, newdata = nonres_dt))]

  files <- list.files(pattern = "SFresidents.*xlsx", recursive = T, full.names = T)
  files <- grep("nonresidents", files, invert = T, value = T)

  for (f in files) {
    x <- ReadForecast(f)
    AddNonresidents(nonres_dt, f)
  }

  x <- ReadForecast("Forecasts/San Francisco.xlsx")
  write.csv(x, "Forecasts/SFresidents_and_nonresidents.csv", row.names = F)
  return(NULL)
}



