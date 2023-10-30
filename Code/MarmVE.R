MK_VE_Severe <- function(nabs) {
  plogis(log2(nabs) * 0.400 + 3.355)
}

MK_VE_Infection <- function(nabs) {
  plogis(log2(nabs) * 0.308 + 1.02)
}

MK_VE_For_Transmission <- function(nabs) {
  VE_infection <- MK_VE_Infection(nabs)
  VE_transmission_given_infection <- plogis(log2(nabs)*0.549 - 1.17)
  1 - (1-VE_infection) * (1-VE_transmission_given_infection)
}

MK <- function(nabs_in, frac, omicron) {
  if (omicron) {
    fold_reduction <- 30
  } else {
    fold_reduction <- 2.73
  }
  nabs <- nabs_in / fold_reduction
  dt_out <- data.table(severe = MK_VE_Severe(nabs), infection = MK_VE_Infection(nabs), for_transmission = MK_VE_For_Transmission(nabs))
  dt_out[, severe_given_infection := 1 - (1 - severe) / (1 - for_transmission)]
  return(dt_out)
}

Get1 <- function(zz) {
  stopifnot(uniqueN(zz) == 1)
  zz[1]
}

#if waned, divide by 8.06
#if omicron, divide by 30 [if this changes, need to adjust nabs for booster]
#if delta, divide by 2.74 [overstates booster?]
#My aprox - if one dose, divide nabs by 5
#fresh_infected and waned_infected are fractions of population
CalcVE <- function(dt_in, fresh_infected, waned_infected) {
  dt <- copy(dt_in)
  dt[, N := as.numeric(N)]
  frac_vax <- 1 - dt[vax_type == "none", sum(N)] / dt[, sum(N)]

  dt[vax_type == "none", nabs := 0]
  dt[vax_type == "Pfizer", nabs := 2.37]
  dt[vax_type == "Moderna", nabs := 4.13]
  dt[vax_type == "Johnson & Johnson", nabs := 0.47]
  #dt[vax_type == "infection", nabs := 1]  #dt only has vaccines, not infections
  #dt[vax_type == "hybrid", nabs := 2.37 * 7.9] #dt only has vaccines, not infections

  fresh_infected_unvax_dt <- copy(dt[vax_type == "none"])
  waned_infected_unvax_dt <- copy(dt[vax_type == "none"])

  fresh_infected_unvax_dt[, vax_type := "infection"]
  fresh_infected_unvax_dt[, nabs := 1]
  fresh_infected_unvax_dt[, waned := F]
  fresh_infected_unvax_dt[, N := N * fresh_infected]

  waned_infected_unvax_dt[, vax_type := "infection"]
  waned_infected_unvax_dt[, nabs := 1]
  waned_infected_unvax_dt[, waned := T]
  waned_infected_unvax_dt[, N := N * waned_infected]
  dt[vax_type == "none", N := N * (1 - (fresh_infected + waned_infected))]

  #assumes hybrid "waned" refers to vaccine being > Xmonths, not infection being > Xmonths
  hybrid_dt <- copy(dt[!(vax_type %in% c("none", "BOOSTER"))])
  hybrid_dt[, vax_type := "hybrid"]
  hybrid_dt[, nabs := 2.37 * 7.9]
  hybrid_dt[, N := N * (fresh_infected + waned_infected)]
  dt[!(vax_type %in% c("none", "BOOSTER")), N := N * (1 - (fresh_infected + waned_infected))]

  dt <- rbind(dt, fresh_infected_unvax_dt, waned_infected_unvax_dt, hybrid_dt)

  # dt[dose == "BOOSTER", nabs := 36]
  dt[dose == "BOOSTER", nabs := 8] #Jan 5 - VEinf = 61%, VEsev = 93%
  dt[dose == "BOOSTER", waned := F] #assume no waning for boosters
  dt[dose == "1" & vax_type %in% c("Pfizer", "Moderna"), nabs := nabs / 5]
  dt[waned == T, nabs := nabs / 8.06]

  stopifnot(abs(dt_in[, sum(N)] / dt[, sum(N)] - 1) < 0.01)

  dt[, severity_weight := ifelse(old, 4, 1)] #ballpark weighting
  dt[, severity_fraction := (N * severity_weight) / sum(N * severity_weight)]

  dt_omicron <- cbind(dt, MK(nabs_in = dt[, nabs], omicron = T))
  dt_delta <- cbind(dt, MK(nabs_in = dt[, nabs], omicron = F))

  VE_infection <- dt_omicron[, sum(for_transmission * N / sum(N))]
  VE_infection_delta <- dt_delta[, sum(for_transmission * N / sum(N))]

  VE_severe_given_infection <- dt_omicron[, sum(severe_given_infection * severity_fraction)]

  booster_VE_infection <- Get1(dt_omicron[dose == "BOOSTER", for_transmission])
  booster_VE_severe_given_infection <- Get1(dt_omicron[dose == "BOOSTER", severe_given_infection])
  if (F) {
    #for debugging
    s <- dt[, sum(N)]
    d1 <- dt[, .(naive = sum(N * (vax_type == "none")),
                 infected_fresh = sum(N * (vax_type == "infection" & waned == F)),
                 infected_waned = sum(N * (vax_type == "infection" & waned == T)),
                 vax_fresh = sum(N * (vax_type %in% c("Pfizer", "Moderna", "Johnson & Johnson") & dose != "BOOSTER" & waned == F)),
                 vax_waned = sum(N * (vax_type %in% c("Pfizer", "Moderna", "Johnson & Johnson") & dose != "BOOSTER" & waned == T)),
                 boosted = sum(N * (dose == "BOOSTER")),
                 hybrid_fresh = sum(N * (vax_type == "hybrid" & dose != "BOOSTER" & waned == F)),
                 hybrid_waned = sum(N * (vax_type == "hybrid" & dose != "BOOSTER" & waned == T)))] / s
  }
  VE_severe_given_infection_delta <-  dt_delta[, sum(severe_given_infection * severity_fraction)]
  return(data.table(VE_infection, VE_infection_delta, VE_severe_given_infection, booster_VE_infection, booster_VE_severe_given_infection, VE_severe_given_infection_delta))

}

#P(hosp|infected,age,vax) = P(hosp & infected|age, vax)/P(infected|age, vax)=P(hosp|age,vax)/P(infected|age, vax)
#P(hosp|age,vax) = (1-VE_severe[vax]) * P(hosp|age, novax)
#P(infected|age,vax) = (1-VE_inf[vax]) * P(infected|age, novax)
#P(P(hosp|infected,age,vax) = (1-VE_severe[vax]) * P(hosp|age, novax) / [(1-VE_inf[vax]) * P(infected|age, novax)] = (1-VE_severe[vax]) / (1-VE_inf[vax]) * P(hosp|infected, age, novax)
