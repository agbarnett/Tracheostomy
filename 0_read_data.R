# 0_read_data.R
# read the data
# from https://www.dropbox.com/s/pkre0u0glf7zjf1/Data%20to%20Adrian_25.11.18.xls?dl=0
# Feb 2019
library(readxl)
library(dplyr)

# some basics
last.alive.date = 43405 # data of national death index search; 01/11/2018 as a number matching Excel format... 
as.Date(last.alive.date, origin='1899-12-30') # ... check
dlabs = c('CVS' , 'RESP' , 'SEPS' , 'GIT', 'Neuro', 'Other') # Primary diagnosis labels
na.strings = c('N/A')

## sheet 1 - demography
demog = read_excel("data/Data to Adrian_25.11.18.xls", sheet=1, skip=0, na=na.strings) %>%
# 7777=data not available
    rename('UR' = 'UR #',
               'dead'='Long term Dead / Alive as at 1.11.18',
              'ICUsurv' = 'ICU survival',
              'diagnosis' = 'Primary diagnosis',
              'DeathDate' = 'Death date',
           'SOFA1' = 'SOFA 1',
           'SOFA2' = 'SOFA 2',
           'SOFA3' = 'SOFA 3',
              'SOFA4' = 'SOFA 4') %>%
  mutate(id=1:n(),  # for merging, because some URs came twice; both sheets are in same order
         UR = as.numeric(UR), 
         DeathDate = ifelse(DeathDate!=9999, DeathDate, NA),
         BMI = ifelse(BMI!=7777, BMI, NA),
         ICUsurv = ifelse(ICUsurv!=7777, ICUsurv, NA),
         Weight = ifelse(Weight!=7777, Weight, NA),
         APACHE = ifelse(APACHE!=7777, APACHE, NA),
         SOFA1 = ifelse(SOFA1!=7777, SOFA1, NA),
         SOFA2 = ifelse(SOFA2!=7777, SOFA2, NA),
         SOFA3 = ifelse(SOFA3!=7777, SOFA3, NA),
         SOFA4 = ifelse(SOFA4!=7777, SOFA4, NA),
         Gender = factor(Gender),
         ICUsurv = factor(ICUsurv, levels=2:3, labels=c('Yes','No')),
         diagnosis = factor(diagnosis, levels=1:6, labels=dlabs))

## sheet 2 - trach
# SV = speaking valve?
vent =  read_excel("data/Data to Adrian_25.11.18.xls", sheet=2, skip=0, na=na.strings) %>%
  select(-'UR #') %>%  # do not need, in other data
  rename(
    'ETTInsertion' = 'ETT Insertion',
    'ETTExtubation' = 'ETT Extubation',
    'TracheExt' = 'Trache extubation date',
    'SVDate' = 'SV Date',
    'physio' = 'Date of first physio Rx',
    'food' = 'Date food commenced',
    'exercise' = 'Date of first out of bed exercise',
    'walk' = "Date of first walk / stand",
    'mobscorebed' = "Best mobility score when first out of bed exercise",
    'mobscoredis' = "Best mobility score on discharge",
    'ventdays' = 'Total ventilation days') %>%
  mutate(id=1:n(),
         # replace missing
         ETTInsertion = ifelse(ETTInsertion %in% c(7777,9999), NA, ETTInsertion),
         ETTExtubation = ifelse(ETTExtubation %in% c(7777,8888,9999), NA, ETTExtubation),
         TracheExt = ifelse(TracheExt %in% c(7777,8888,9999), NA, TracheExt),
         SVDate = ifelse(SVDate %in% c(7777,9999), NA, SVDate),
         physio = ifelse(physio %in% c(7777,9999), NA, physio),
         food = ifelse(food %in% c(7777,9999), NA, food),
         walk = ifelse(walk %in% c(7777,9999), NA, walk),
         exercise = ifelse(exercise %in% c(7777,9999), NA, exercise),
         mobscorebed = ifelse(mobscorebed %in% c(7777,9999), NA, mobscorebed),
         mobscoredis = ifelse(mobscoredis %in% c(7777,9999), NA, mobscoredis),
         ventdays = ifelse(ventdays!=7777, ventdays, NA),
         # dates / times
         tdate = 25569 + floor(as.numeric(`Tracheotomy date`)/(24*60*60)), # make date into a number; fudge because of Excel dates and time/date format
         time.to.t = tdate - ETTInsertion # ETT insertion date and Tracheostomy date (meaning, looking at it from the beginning of mechanical ventilation)
#         time.to.p = physio - tdate, # time from Tracheostomy to first physio (may not be useful!)
#         time.to.e = exercise - tdate # time from Tracheostomy to first out of bed exercise
  )
as.Date(vent$tdate[1:2], origin='1899-12-30') # ... check
## make a variable that is date last seen in ICU
dates.for.max = c('ETTInsertion','ETTExtubation','TracheExt','SVDate','physio','food','walk','exercise')
vent = select(vent, 'id', dates.for.max) %>%
  tidyr::gather(key='event', value='date', dates.for.max) %>%
  group_by(id) %>%
  summarise(max.ICU.date = max(date, na.rm=TRUE)) %>%
  ungroup() %>%
  right_join(vent, by='id') 

## sheet 3 - drugs (awkard variable naming)
drugs.list = c('Fentanyl Total(mcg)','Morphine Total (mg)','Analgesics (Morphine - mg)','Midazolam Total (mg)','Propofol Total (mg)','Dexmedetomidine Total (mcg)','Sedatives (Propofol)','Haloperidol Total (mg)','Olanzipine Total (mg)','Risperidone Total (mg)','Quetiapine Total (mg)','Antipsychotics (CPZ - mg)','24hrs drug free')
drugs =  read_excel("data/Data to Adrian_25.11.18.xls", sheet=3, skip=1, na=na.strings)
#names(drugs) = c()

####
# merge by excel row number (id)
data = left_join(demog, vent, by='id') %>%
  filter(id != 57) %>% # remove duplicate error for UR 708232
# calculate survival time, including for censored patients
   mutate(surv.time = ifelse(dead==2, DeathDate - tdate, last.alive.date - tdate)) # dead=2
# censor times for UR numbers who had a second visit
duplicates = data$UR[duplicated(data$UR)]
dups = filter(data, UR %in%duplicates) # duplicates
not.dups = filter(data, UR %in%duplicates ==FALSE) # not duplicates
# now censor first time for duplicates
dups = arrange(dups, UR, tdate) %>%
  group_by(UR) %>%
  mutate(max.date = max(tdate),
         count=1:n(),
         dead = ifelse(count==1, 1, dead), # first row must be censored
         surv.time = ifelse(count==1, max.date - tdate, surv.time)) %>% # re-calculate for first row (censored)
  select(-count, -max.date) %>% # No longer need these variables
  ungroup()
  # select(dups, UR, tdate, max.date, dead, surv.time)
# put back together
data = rbind(dups, not.dups) %>%
  mutate(dead = factor(dead, levels=1:2, labels=c('Alive','Dead'))) # Long term Dead/Alive
# add scale on years for table/plots
data$surv.time.years = data$surv.time/365.25 

## define early vs late tracheostomy (Email from Anna-Liisa)
data = mutate(data,
              # using difference between ETT insertion date and Tracheostomy date 
              early.late = ifelse(time.to.t <7, 1, 2),
              early.late = factor(early.late, levels=1:2, labels=c('Early','Late')))

# save
save(last.alive.date, data, file='data/AnalysisReady.RData')

## checks
# check long survival times
filter(data, surv.time>20000) %>%
  select(UR, tdate, dead, DeathDate)

head(select(data, UR, tdate, dead, DeathDate, surv.time))

# check duplicates:
filter(data, UR%in%duplicates[1:2]) %>%
  select(UR, tdate, dead, DeathDate, surv.time)

