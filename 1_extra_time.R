# 1_extra_time.R
# function to estimate extra time after Tracheostomy
# some code taken from etm::clos
# Feb 2019
source('1_boot_clos_function.R') # bootstrap function to estimate SE for length of stay

extra.time = function(indata, outcome, n.boot=100){

## numbered events:
# 0 = ventilation, 1 = Tracheostomy, 2 = Outcome (e.g., physio), 3 = Death

## get our data into Allignol et al format
our.los.data = filter(indata, is.na(ETTInsertion)==FALSE ) %>% # remove one patient with missing ventilation start
  mutate(id = 1:n()) %>%
  dplyr::select('id', 'UR','ETTInsertion', 'tdate', outcome, 'ICUdeathDate', 'max.ICU.date') %>%
  rename('outcome' = outcome) %>% # replace outcome with generic name
  mutate(ICUdeathDate = ifelse(outcome=='ICUDeath', ICUdeathDate+1, ICUdeathDate), # add one day to date if outcome is ICU death (dummy to avoid overlap)
         censored = is.na(outcome), # flag for censored
         outcome = ifelse(is.na(outcome), max.ICU.date, outcome)) # replace missing outcome with date last seen in ICU

# loop through patients
time.data = NULL
for (p in 1:nrow(our.los.data)){ # loop through patients
    dates = data.frame(our.los.data[p, c('ETTInsertion', 'tdate', 'outcome', 'ICUdeathDate')]) # just get dates
    # if outcome happens on same date as ventilation then assume ventilation happened first (Anna-Liisa by email)
    if(is.na(dates[3])==FALSE){
      if(dates[1] == dates[3]){dates[3] = dates[3] + 0.5; dates[4] = dates[4] + 0.5} # move following event forward by half a day
    }
    # if Tracheostomy happens on same date as ventilation then assume ventilation happened first (Anna-Liisa by email)
    if(is.na(dates[2])==FALSE){
      if(dates[1] == dates[2]){dates[2] = dates[2] + 0.5; dates[4] = dates[4] + 0.5} # move following event forward by half a day
    }
    # if outcome happens on same date as Tracheostomy then assume Tracheostomy happened first (Anna-Liisa by email)
    if(is.na(dates[3])==FALSE){
      if(dates[2] == dates[3]){dates[3] = dates[3] + 0.5; dates[4] = dates[4] + 0.5} # move following event forward by half a day
    }
    # if death happens on same date as Tracheostomy then assume Tracheostomy happened first
    if(is.na(dates[2])==FALSE & is.na(dates[4])==FALSE){
      if(dates[2] == dates[4]){dates[4] = dates[4] + 0.5} # move death forward by half a day
    }
    # if death happens on same date as event then assume event happened first
    if(is.na(dates[3])==FALSE & is.na(dates[4])==FALSE){
      if(dates[3] == dates[4]){dates[4] = dates[4] + 0.5} # move death forward by half a day
    }
    o = order(dates) # what order did dates happen in
    events = seq(0,3)[o] # order of events (as numbers)
    if(events[1] != 0){cat('Error for id = ', p, '\n')} # first event is not ventilation
    # if outcome happens before Tracheostomy then remove Tracheostomy and death date (because we are not interested in these transitions)
    if(which(events==2) < which(events==1)){dates[c(2,4)] = NA}
    #
    dates = as.matrix(dates[o]) # order dates
    for (k in 1:sum(is.na(dates)==FALSE)){
      # censored if outcome never happened (moving to state number 2)
      to = ifelse(events[k+1]==2 & our.los.data$censored[p], 'cens', events[k+1]) 
      # make data frame  
      f = data.frame(id = our.los.data$id[p], from=events[k], to=to, time=dates[k+1] - dates[1], time.reset = dates[k+1] - dates[k])
      time.data = rbind(time.data, f)
      remove(f, cens)
    }
    
  } # end of patients loop

# remove transitions we are not interested in, plus non-existant transitions
  time.data = filter(time.data, from != 2, from != 3, is.na(time)==FALSE)
#  with(time.data, table(from, to)) # check
if(nrow(filter(time.data, time==0))){cat('Some zero transitions.\n')} # check

  # count the number censored
  n.censored = sum(time.data$to == 'cens')
  
  # create transition based on observed transitions
  # included all transitions (but exclude censored) by using factors
  time.data = mutate(time.data, from.f = factor(from, levels=0:3, labels=0:3),
                     to.f = factor(to, levels=0:3, labels=0:3))
  tab = with(time.data, table(from.f, to.f))
  tra <- as.matrix(tab>0)
  states = 0:3

  ## analysis   
  # etm
  tr.prob <- etm(data=time.data, state.names= states, tra=tra, cens.name="cens", s=0)
  cLOS <- etm::clos(tr.prob)
  #plot(cLOS)
  #
  mean.change = cLOS$e.phi # mean change in time

  ## bootstrap and calculate confidence intervals
  se <- sqrt(var(boot.clos(time.data, states, tra=tra, cens.name="cens", s=0,
                           nboot = n.boot)))
  z = qnorm(0.975)
  lower = mean.change - (z*se)
  upper = mean.change + (z*se)
  
  ## make data for Cox model
  demog = select(data, 'id','APACHE','SOFA1','Age','Gender') # variables to adjust for
  out.data = left_join(time.data, demog, by='id')
  
  # return stats, plot and data for Cox models
  to.return = list()
  to.return$out.data = out.data
  to.return$cLOS = cLOS
  to.return$stats = data.frame(outcome=outcome, n.censored=n.censored, mean=mean.change, lower=lower, upper=upper)
  return(to.return)
} # end of function

#e = extra.time(indata = data, outcome = 'food', n.boot=100)
#e = extra.time(indata = data, outcome = 'exercise', n.boot=100)

### TO DO, check data more, check censoring for frequently censored outcome
# add results to rmarkdown