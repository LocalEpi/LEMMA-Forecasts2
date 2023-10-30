GetCountyData <- function() {
  county_dt <- data.table::fread("https://data.chhs.ca.gov/dataset/2df3e19e-9ee4-42a6-a087-9761f82033f6/resource/47af979d-8685-4981-bced-96a6b79d3ed5/download/covid19hospitalbycounty.csv")
  county_dt <- county_dt[, .(county, date = as.Date(todays_date), hosp.conf = hospitalized_covid_confirmed_patients)]
  county_dt[date %in% as.Date(c("2023-05-11", "2023-05-12")), hosp.conf := NA] #data problems

  cases <- data.table::fread("https://data.chhs.ca.gov/dataset/f333528b-4d38-4814-bebb-12db1f10f535/resource/046cdd2b-31e5-4d34-9ed3-b48cdbc4be7a/download/covid19cases_test.csv")[area_type == "County", .(date = as.Date(date), county = area, cases)]
  cases <- cases[!is.na(date)]
  data.table::setkey(deaths.cases, county, date)

  max_date <- cases[, max(date)]
  cases <- cases[date >= as.Date("2021/11/1") & date < (max.date - 5), .(date, county, cases.conf = as.double(cases), cases.pui = NA_real_)]

  holiday.set <- as.Date(c("2021/11/25", "2021/11/26", "2021/12/24", "2021/12/25", "2021/12/31", "2022/1/1", "2022/1/17", "2022/2/21",  "2022/5/30", "2022/6/20", "2022/7/4", "2022/9/5", "2022/11/11", "2022/11/25", "2022/11/26", "2022/12/24", "2022/12/25",  "2022/12/26", "2023/1/1", "2023/1/2"))
  cases[date %in% holiday.set, cases.conf := NA_real_]

  county_dt <- merge(county_dt, cases, all = T, by = c("county", "date"))

  county_dt[, hosp.pui := NA]
  county_dt[, cases.pui := NA]
  data.table::setkey(county_dt, county, date)
  return(county_dt)
}

