PlotOmicron <- function(hosp_dt, county_dt_orig, filestr) {
  devlist <- grDevices::dev.list()
  sapply(devlist[names(devlist) == "pdf"], grDevices::dev.off) #shuts down any old pdf (if there was a crash part way)

  pdf(filestr, width = 11, height = 6)
  for (county1 in c("Total", county_set)) {
    cat(county1,"\n")
    if (county1 == "Total") {
      county_set1 <- county_set
      county1_lab <- "California"
    } else {
      county_set1 <- county1
      county1_lab <- county1
    }
    actual_dt <- county_dt_orig[county %in% county_set1, .(hosp = sum(hosp.conf)), by = "date"]
    g <- dcast(hosp_dt[county == county1, .(date, quantile, hosp_census_with_covid)], formula = date ~ ..., value.var = "hosp_census_with_covid")

    print(ggplot(g, aes(x = date, ymin = `0.05`, ymax = `0.95`)) + geom_ribbon(alpha = 0.3, fill = "blue") + geom_line(aes(y = `0.5`)) + xlab("")  + ylab("Covid Hospital Census") + labs(title = county1_lab) + annotate("point", x = actual_dt[, date], y = actual_dt[, hosp], na.rm = T) + coord_cartesian(xlim = c(as.Date("2021/12/27"), NA), ylim = c(0, NA)) + scale_x_date(date_breaks = "1 month", date_labels = "%b"))
  }
  dev.off()
}

