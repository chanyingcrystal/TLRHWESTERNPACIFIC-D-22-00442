library(dplyr)
library(splitstackshape)
library(stringr)

dx$key <- "dx"
pcr$key <- "pcr"

ip <- rbind(dx, pcr)
ip=ip%>%distinct()
rm(dx, pcr)

ip$Admission.Date..yyyy.mm.dd. <- as.Date(ip$Admission.Date..yyyy.mm.dd.)

ip = ip%>%filter(Admission.Date..yyyy.mm.dd. >= "2022-02-22")
ip = ip%>%distinct()
ip.check = ip%>%filter(!duplicated(ref))
#exclude plasma use, baricitinib ,casirivimab + imdevimab    ,  interferon beta-1b       ,      tocilizumab  use
plasma = plasma%>%
  filter(!duplicated(ref))%>%mutate(plasma = 1)

interferon = interferon%>%
  filter(!duplicated(ref))%>%mutate(interferon = 1)
ip$plasma <- plasma$plasma [match(ip$ref, plasma$ref)]
ip$interferon <- interferon$interferon [match(ip$ref, interferon$ref)]
ip=ip%>%filter(is.na(plasma) & is.na(interferon))
ip$pcr.date <- pcr.date$`LIS Reference Datetime`  [match(ip$ref, pcr.date$ref)]
ip.check = ip%>%filter(!duplicated(ref))
#####exclude pt who attended dc at any time point 
#create dc cohort 
dc$Appointment.Date..yyyy.mm.dd. <- as.Date(dc$Appointment.Date..yyyy.mm.dd.)
dc = dc%>% filter(Appointment.Date..yyyy.mm.dd. >= "2022-02-22")

library(stringr)
dc$mol <- grepl ("MOLNUPIRAVIR", dc$Active.Medication...Drug.Name)
dc$pax <- grepl ("PAXLOVID", dc$Active.Medication...Drug.Name)
dc = dc%>%
  mutate (antiviral = case_when(mol == "TRUE" & pax == "TRUE" ~3,
                                mol == "TRUE" & pax == "FALSE" ~2,
                                mol == "FALSE" & pax == "TRUE" ~1,
                                mol == "FALSE" & pax == "FALSE" ~0))
dc.drug <- dc%>%filter(antiviral != 0)

##################################Count number of drug course used in DC

dc.drug$mark <- dc.t0$key [match(dc.drug$ref, dc.t0$ref)]
dc.drug = dc.drug%>% filter(mark ==1)

dc.drug.count = dc.drug%>%
  group_by(ref)%>%
  arrange(Appointment.Date..yyyy.mm.dd.)%>%
  mutate(gap = difftime(lead(Appointment.Date..yyyy.mm.dd.),Appointment.Date..yyyy.mm.dd.,units = "days"))

dc.drug.count$gap [is.na(dc.drug.count$gap)] <- 5
dc.drug.count$gap <- as.integer(dc.drug.count$gap)
dc.drug.count = dc.drug.count%>%
  mutate(dc.drug.cost = case_when(gap >= 5 & mol ==T ~ 5*141.4,
                             gap <5 & mol ==T ~ gap*141.4,
                             gap >= 5 & pax ==T ~ 5*105.8,
                             gap <5 & pax ==T ~ gap*105.8))

dc.t0$dc.drug.cost <- dc.drug.count$dc.drug.cost[match(dc.t0$ref, dc.drug.count$ref)]

plasma$plasma <- 1
interferon$interferon <- 1

dc.check = dc%>%filter(!duplicated(ref))

dc$plasma <- plasma$plasma [match(dc$ref, plasma$ref)]
dc$interferon <- interferon$interferon [match(dc$ref, interferon$ref)]
dc=dc%>%filter(is.na(plasma) & is.na(interferon))

dc.t0 = dc%>% group_by(ref)%>%arrange(Appointment.Date..yyyy.mm.dd.)%>%ungroup()%>%filter(!duplicated(ref))

##exclude pt who attended dc at any time point 
dc.t0$key <- 1
ip$dc <- dc.t0$key[match(ip$ref, dc.t0$ref)]

ip = ip%>% filter(is.na(dc))
ip.check = ip%>%filter(!duplicated(ref))
#baseline
ip.t0 = ip%>% filter(Admission.Date..yyyy.mm.dd. >= "2022-02-22")%>% group_by(ref) %>% arrange(Admission.Date..yyyy.mm.dd.)%>% filter(!duplicated(ref))%>%ungroup()%>%arrange(Admission.Date..yyyy.mm.dd.)
ip.t0$Admission.Date..yyyy.mm.dd. <- as.Date(ip.t0$Admission.Date..yyyy.mm.dd.)
ip.t0$pcr.date <- pcr.date$`LIS Reference Datetime`  [match(ip.t0$ref, pcr.date$ref)]
ip.t0$start.episode <- case_when(!is.na(ip.t0$pcr.date) ~ as.Date(ip.t0$pcr.date),
                                 TRUE ~ as.Date(ip.t0$Admission.Date..yyyy.mm.dd.))
ip.t0 = ip.t0%>%filter(start.episode >= "2022-02-22")

#import antiviral
antiviral$molnupiravir <-str_detect(antiviral$Drug.Name,"MOLNUPIRAVIR")
antiviral$paxlovid <-str_detect(antiviral$Drug.Name,"PAXLOVID")

antiviral$key <- ip.t0$key [match(antiviral$ref, ip.t0$ref)]
antiviral.ip <- antiviral %>%filter(!is.na(key))%>% filter(molnupiravir == T | paxlovid ==T)
antiviral.ip$Admission.Date..yyyy.mm.dd. <- ip.t0$start.episode [match(antiviral.ip$ref, ip.t0$ref)]
antiviral.ip$Admission.Date..yyyy.mm.dd. <- as.Date(antiviral.ip$Admission.Date..yyyy.mm.dd.)

antiviral.ip$Prescription.Start.Date <- as.Date(antiviral.ip$Prescription.Start.Date)
antiviral.ip$Prescription.End.Date <- as.Date(antiviral.ip$Prescription.End.Date)

antiviral.ip$prescription.start <- antiviral.ip$Prescription.Start.Date - antiviral.ip$Admission.Date..yyyy.mm.dd.
antiviral.ip$Dispensing.Duration <- as.integer(antiviral.ip$Dispensing.Duration) +1 #start same day end same day = 1 day 
antiviral.ip = filter(antiviral.ip, prescription.start >=0)

antiviral.ip.t0 = antiviral.ip%>%group_by(ref)%>%arrange(prescription.start)
range(antiviral.ip.t0$Dispensing.Date..yyyy.mm.dd.)
antiviral.ip.t0 = antiviral.ip.t0%>%group_by(ref)%>%arrange(prescription.start)%>%ungroup()%>%filter(!duplicated(ref))
table(as.integer(antiviral.ip.t0$prescription.start))

#antiviral.ip$mark[antiviral.ip$molnupiravir == 0 & antiviral.ip$paxlovid ==0] <- 1
#remove prescription that start before 


antiviral.ip %>%
  filter(prescription.start >=0)%>%
  group_by(ref, prescription.start, molnupiravir, paxlovid) %>%
  arrange(-Dispensing.Duration)%>%
  filter(duplicated(ref))%>%
  mutate(group = 1:n())%>%
  mutate(nrow = Dispensing.Duration) %>% 
  #arrange(prescription.start)%>%
  expandRows(count = "nrow")%>%
  mutate(group  = 1:n() )%>%
  mutate(day = group + prescription.start-1)%>%
  group_by(ref, molnupiravir, paxlovid)%>%
  filter(!duplicated(day))%>%
  group_by(ref,prescription.start, molnupiravir, paxlovid)%>%
  summarise(max = max(day), min = min(day))%>%
  mutate(duration = max - min+1) %>%
  group_by(ref, molnupiravir, paxlovid)%>%
  summarise(duration = sum(duration))-> antiviral.ip.duration

antiviral.ip.duration = antiviral.ip.duration%>%
  mutate(cost = case_when(paxlovid == T ~ duration * 105.8,
                          molnupiravir == T~ duration *141.4))

antiviral.ip.duration$cost <- as.integer(antiviral.ip.duration$cost)
fif.ip.t0.drug$drug.cost <- antiviral.ip.duration$cost[match(fif.ip.t0.drug$ref, antiviral.ip.duration$ref)]

antiviral.ip.duration$antiviral.course <- 1
antiviral.ip.duration$antiviral.course[antiviral.ip.duration$duration ==5 ] <- 2

antiviral.ip.duration$drug.start <- antiviral.ip.t0$prescription.start [match(antiviral.ip.duration$ref, antiviral.ip.t0$ref)]

#antiviral.ip %>%
#  filter(prescriptoin.start >=0)-> antiviral.ip.duration.less

#antiviral.ip.duration$mark <- 1 
#antiviral.ip.duration.notcomplete <- antiviral.ip.duration[antiviral.ip.duration$Dispensing.Duration<5,]
#antiviral.ip.duration.less$mark <- antiviral.ip.duration.notcomplete$mark [match(antiviral.ip.duration.less$ref, antiviral.ip.duration.notcomplete$ref)]
#antiviral.ip.duration.less <- antiviral.ip.duration.less[!duplicated(antiviral.ip.duration.less$ref),]

ip.t0$antiviral.course <- antiviral.ip.duration$antiviral.course[match(ip.t0$ref, antiviral.ip.duration$ref)]
ip.t0$antiviral.course[is.na(ip.t0$antiviral.course)] <- 0

ip.t0$molnupiravir <- antiviral.ip.duration$molnupiravir[match(ip.t0$ref, antiviral.ip.duration$ref)]
ip.t0$molnupiravir[is.na(ip.t0$molnupiravir)] <- 0

ip.t0$paxlovid <- antiviral.ip.duration$paxlovid[match(ip.t0$ref, antiviral.ip.duration$ref)]
ip.t0$paxlovid[is.na(ip.t0$paxlovid)] <- 0

ip.t0$start.drug <- antiviral.ip.duration$drug.start[match(ip.t0$ref, antiviral.ip.duration$ref)]
ip.t0$start.drug[is.na(ip.t0$start.drug)] <- 0

#fif.ip.t0.drug$molnupiravir <- antiviral.ip.duration$molnupiravir[match(fif.ip.t0.drug$ref, antiviral.ip.duration$ref)]
#fif.ip.t0.drug$molnupiravir[is.na(fif.ip.t0.drug$molnupiravir)] <- 0

#fif.ip.t0.drug$paxlovid <- antiviral.ip.duration$paxlovid[match(fif.ip.t0.drug$ref, antiviral.ip.duration$ref)]
#fif.ip.t0.drug$paxlovid[is.na(fif.ip.t0.drug$paxlovid)] <- 0


#ip death

ip.dead <- ip[ip$Discharge.Status == "DEATH",]
ip.dead = ip.dead %>% distinct()

ip.t0$dead.date <- ip.dead$Discharge.Date..yyyy.mm.dd.[match(ip.t0$ref,ip.dead$ref)]
ip.t0$dead.date <- as.Date(ip.t0$dead.date)
ip.t0$dead.time <-ip.t0$dead.date - ip.t0$start.episode
ip.t0$dead <- 0
ip.t0$dead [!is.na(ip.t0$dead.date)] <-1

#ip readmission
ip.read <- ip

ip.read$start.episode <- ip.t0$start.episode[match(ip.read$ref, ip.t0$ref)]
ip.read <- ip.read[!is.na(ip.read$start.episode),]
ip.read$start.episode <- as.Date(ip.read$start.episode)
ip.read$Admission.Date..yyyy.mm.dd. <- as.Date(ip.read$Admission.Date..yyyy.mm.dd.)
ip.read$start.day <- ip.read$Admission.Date..yyyy.mm.dd.- ip.read$start.episode
ip.read$Discharge.Date..yyyy.mm.dd. <- as.Date(ip.read$Discharge.Date..yyyy.mm.dd.)
ip.read$end.date <- ip.read$Discharge.Date..yyyy.mm.dd.
ip.read$end.date [ip.read$Discharge.Status == "ACUTE" |ip.read$Discharge.Status == "CONV."]<- NA
ip.read$end.date <- as.Date(ip.read$end.date)
ip.read$end.day <- ip.read$end.date - ip.read$start.episode

gap = ip.read %>% distinct()
gap <- dplyr::select(ip.read, ref, start.day, end.day)
gap <- gap [gap$start.day >= 0,]
gap <- gap%>%distinct()



library(dplyr) 
gap%>%
  group_by(ref, start.day)%>%
  arrange(ref, start.day)%>%
  group_by(ref)%>%
  group_by(group = 1:n())%>%
  group_by(ref)%>%
  mutate(end.day = case_when(is.na(end.day) ~ lead(end.day),
                                  TRUE ~ as.numeric(end.day)))%>%
  group_by(ref)%>%
  mutate(gap.time = start.day - lag(end.day), timemark = NA^is.na(gap.time), timemark2=(1:n()*timemark))%>%
  mutate(gap.seq = timemark2 -1) %>%
  mutate(gap.time.mark = case_when(group == 1 ~ 9999999,
                                   TRUE ~ as.numeric(gap.seq)))%>% 
  mutate(start.day = start.day, end.day = end.day)%>% 
  dplyr::select(ref, start.day, end.day, gap.time, gap.time.mark) -> gap2


gap2 <- data.frame(gap2)
gap2$del.mark[is.na(gap2$start.day) & is.na(gap2$end.day)] <- 1 
gap2$del.mark[gap2$gap.time < 0] <- 1
gap2 <- gap2[is.na(gap2$del.mark),]
gap2 <- dplyr::select(gap2, ref, start.day, end.day)

gap2%>%
  group_by(ref, start.day)%>%
  arrange(ref, start.day)%>%
  group_by(ref)%>%
  group_by(group = 1:n())%>%
  ungroup%>%
  group_by(ref)%>%
  mutate(end.day = case_when(is.na(end.day) ~ lead(end.day),
                             TRUE ~ as.numeric(end.day)))%>%
  group_by(ref)%>%
  mutate(gap.time = start.day - lag(end.day), timemark = NA^is.na(gap.time), timemark2=(1:n()*timemark))%>%
  mutate(gap.seq = timemark2 -1) %>%
  mutate(gap.time.mark = case_when(group == 1 ~ 9999999,
                                   TRUE ~ as.numeric(gap.seq)))%>% 
  mutate(start.day = start.day, end.day = end.day)%>%# make the first admission of each pateitn as 9999999
  dplyr::select(ref, start.day, end.day, gap.time, gap.time.mark) -> gap3

gap3 <- data.frame(gap3)
gap3$del.mark[is.na(gap3$start.day) & is.na(gap3$end.day)] <- 1 
gap3$del.mark[gap3$gap.time < 0] <- 1
gap3 <- gap3[is.na(gap3$del.mark),]
gap3 <- dplyr::select(gap3, ref, start.day, end.day)

gap3%>%
  group_by(ref, start.day)%>%
  arrange(ref, start.day)%>%
  group_by(ref)%>%
  group_by(group = 1:n())%>%
  ungroup%>%
  group_by(ref)%>%
  mutate(end.day = case_when(is.na(end.day) ~ lead(end.day),
                             TRUE ~ as.numeric(end.day)))%>%
  group_by(ref)%>%
  mutate(gap.time = start.day - lag(end.day), timemark = NA^is.na(gap.time), timemark2=(1:n()*timemark))%>%
  mutate(gap.seq = timemark2 -1) %>%
  mutate(gap.time.mark = case_when(group == 1 ~ 9999999,
                                   TRUE ~ as.numeric(gap.seq)))%>% 
  mutate(start.day = start.day, end.day = end.day)%>%# make the first admission of each pateitn as 9999999
  dplyr::select(ref, start.day, end.day, gap.time, gap.time.mark) -> gap4


gap4 <- data.frame(gap4)
gap4$del.mark[is.na(gap4$start.day) & is.na(gap4$end.day)] <- 1 
gap4$del.mark[gap4$gap.time < 0] <- 1
gap4 <- gap4[is.na(gap4$del.mark),]
gap4 <- dplyr::select(gap4, ref, start.day, end.day)


gap4%>%
  group_by(ref, start.day)%>%
  arrange(ref, start.day)%>%
  group_by(ref)%>%
  group_by(group = 1:n())%>%
  ungroup%>%
  group_by(ref)%>%
  mutate(end.day = case_when(is.na(end.day) ~ lead(end.day),
                             TRUE ~ as.numeric(end.day)))%>%
  group_by(ref)%>%
  mutate(gap.time = start.day - lag(end.day), timemark = NA^is.na(gap.time), timemark2=(1:n()*timemark))%>%
  mutate(gap.seq = timemark2 -1) %>%
  mutate(gap.time.mark = case_when(group == 1 ~ 9999999,
                                   TRUE ~ as.numeric(gap.seq)))%>% 
  mutate(start.day = start.day, end.day = end.day)%>%# make the first admission of each pateitn as 9999999
  dplyr::select(ref, start.day, end.day, gap.time, gap.time.mark) -> gap5


gap5 <- data.frame(gap5)
gap5$del.mark[is.na(gap5$start.day) & is.na(gap5$end.day)] <- 1 
gap5$del.mark[gap5$gap.time < 0] <- 1
gap5 <- gap5[is.na(gap5$del.mark),]
gap5 <- dplyr::select(gap5, ref, start.day, end.day)


gap5%>%
  group_by(ref, start.day)%>%
  arrange(ref, start.day)%>%
  group_by(ref)%>%
  group_by(group = 1:n())%>%
  ungroup%>%
  group_by(ref)%>%
  mutate(end.day = case_when(is.na(end.day) ~ lead(end.day),
                             TRUE ~ as.numeric(end.day)))%>%
  group_by(ref)%>%
  mutate(gap.time = start.day - lag(end.day), timemark = NA^is.na(gap.time), timemark2=(1:n()*timemark))%>%
  mutate(gap.seq = timemark2 -1) %>%
  mutate(gap.time.mark = case_when(group == 1 ~ 9999999,
                                   TRUE ~ as.numeric(gap.seq)))%>% 
  mutate(start.day = start.day, end.day = end.day)%>%# make the first admission of each pateitn as 9999999
  dplyr::select(ref, start.day, end.day, gap.time, gap.time.mark) -> gap6

gap6 <- data.frame(gap6)
gap6$del.mark[is.na(gap6$start.day) & is.na(gap6$end.day)] <- 1 
gap6$del.mark[gap6$gap.time < 0] <- 1
gap6 <- gap6[is.na(gap6$del.mark),]
gap6 <- dplyr::select(gap6, ref, start.day, end.day)

gap6%>%
  group_by(ref, start.day)%>%
  arrange(ref, start.day)%>%
  group_by(ref)%>%
  group_by(group = 1:n())%>%
  ungroup%>%
  group_by(ref)%>%
  mutate(end.day = case_when(is.na(end.day) ~ lead(end.day),
                             TRUE ~ as.numeric(end.day)))%>%
  group_by(ref)%>%
  mutate(gap.time = start.day - lag(end.day), timemark = NA^is.na(gap.time), timemark2=(1:n()*timemark))%>%
  mutate(gap.seq = timemark2 -1) %>%
  mutate(gap.time.mark = case_when(group == 1 ~ 9999999,
                                   TRUE ~ as.numeric(gap.seq)))%>% 
  mutate(start.day = start.day, end.day = end.day)%>%# make the first admission of each pateitn as 9999999
  dplyr::select(ref, start.day, end.day, gap.time, gap.time.mark) -> gap7

total=gap7%>%
  filter(gap.time.mark != 9999999 & !is.na(gap.time.mark))

total = total%>%
  mutate(mark =1)%>%
  group_by(ref)%>%
  summarise(times = sum(mark))

fif.ip.t0.drug$ane.times <- total$times[match(fif.ip.t0.drug$ref, total$ref)]
fif.ip.t0.drug$ane.times[is.na(fif.ip.t0.drug$ane.times)] <- 0

gap.7dunplanned <- gap7%>%filter(gap.time<=7)
gap.7dunplanned$mark <-1
gap.14dunplanned <- gap7%>%filter(gap.time<=14)
gap.14dunplanned$mark <-1

gap.21dunplanned <- gap7%>%filter(gap.time<=21)
gap.21dunplanned$mark <-1
gap.28dunplanned <- gap7%>%filter(gap.time<=28)
gap.28dunplanned$mark <-1

ip.t0$unplan.7d <- gap.7dunplanned$mark [match(ip.t0$ref, gap.7dunplanned$ref)]
ip.t0$unplan.7d [is.na(ip.t0$unplan.7d )] <-0
ip.t0$unplan.14d <- gap.14dunplanned$mark [match(ip.t0$ref, gap.14dunplanned$ref)]
ip.t0$unplan.14d [is.na(ip.t0$unplan.14d )] <-0
ip.t0$unplan.21d <- gap.21dunplanned$mark [match(ip.t0$ref, gap.21dunplanned$ref)]
ip.t0$unplan.21d [is.na(ip.t0$unplan.21d )] <-0
ip.t0$unplan.28d <- gap.28dunplanned$mark [match(ip.t0$ref, gap.28dunplanned$ref)]
ip.t0$unplan.28d [is.na(ip.t0$unplan.28d )] <-0



#ip los
ip$key <- ip.t0$key [match(ip$ref, ip.t0$ref)]
ip$end.cohort <- "2022-04-15"
ip$end.cohort <- as.Date(ip$end.cohort)
ip$Admission.Date..yyyy.mm.dd. <- as.Date(ip$Admission.Date..yyyy.mm.dd.)
ip$make.up.end <- ip$end.cohort - ip$Admission.Date..yyyy.mm.dd.
ip$Length.of.Stay <- as.integer(ip$Length.of.Stay)
ip$make.up.end <- as.integer(ip$make.up.end)

ip = ip%>%mutate(Length.of.Stay = case_when(Length.of.Stay !="" ~ Length.of.Stay,
                                            TRUE ~ make.up.end))

ip <- ip%>% select(ref, HN.Number, Admission.Date..yyyy.mm.dd., Discharge.Date..yyyy.mm.dd., Length.of.Stay, key)
ip$start.episode <- ip.t0$start.episode[match(ip$ref, ip.t0$ref)]
ip <- ip[!is.na(ip$start.episode),]
ip$start.episode <- as.Date(ip$start.episode)
ip$Admission.Date..yyyy.mm.dd. <- as.Date(ip$Admission.Date..yyyy.mm.dd.)
ip$start.day <- ip$Admission.Date..yyyy.mm.dd.- ip$start.episode
ip = ip %>% distinct()
ip$Length.of.Stay.1 <- ip$Length.of.Stay+1 #add one more day to los

library(splitstackshape)
ip %>%
  filter(Admission.Date..yyyy.mm.dd.>= "2022-02-22" & start.day >=0 &  !is.na(key))%>%
  group_by(ref, start.day) %>%
  arrange(-Length.of.Stay.1)%>%
  filter(!duplicated(ref))%>%
  group_by(ref, start.day) %>%
  mutate(group = 1:n())%>%
  mutate(nrow = Length.of.Stay.1) %>% 
  arrange(start.day)%>%
  expandRows(count = "nrow")%>%
  mutate(group  = 1:n() )%>%
  mutate(day = group + start.day-1)%>%
  group_by(ref) %>%
  filter(!duplicated(day))%>%
  group_by(ref,start.day)%>%
  summarise(max = max(day), min = min(day))%>%
  mutate(duration = max - min +1) %>%
  group_by(ref)%>%
  summarise(Length.of.Stay = sum(duration))-> ip.los

ip.t0$los <- ip.los$Length.of.Stay [match(ip.t0$ref, ip.los$ref)]
ip.t0$los [is.na(ip.t0$los)] <-0


#narrow down cohrot to March 
#paxlovid: 14 March https://www.info.gov.hk/gia/general/202203/15/P2022031500280.htm?fontSize=1
#molnupiravirL Late february https://www.scmp.com/news/hong-kong/health-environment/article/3170596/coronavirus-how-do-oral-drugs-molnupiravir-and
#molnupiravir: 22Feb https://www.info.gov.hk/gia/general/202202/23/P2022022300303.htm

ip.t0$end.episode <- "2022-04-16"
ip.t0$end.episode <- as.Date(ip.t0$end.episode)

#narrow down to cci > 1 OR age >= 60
#CCI
cci$ref <- as.character(cci$ref)
ip.t0.cleaned <- left_join(ip.t0, cci, by = "ref")


fif.ip.t0.drug = ip.t0.cleaned%>% 
  mutate(fu = case_when(dead == 1 ~ dead.time,
                        TRUE ~ end.episode - start.episode))
rm(ip.t0, ip.dead, ip.los, ip.read, ip.t0.cleaned)

fif.ip.t0.drug = fif.ip.t0.drug%>%
  filter(Admission.Age..Year...episode.based. >= 60 | no.disease == 0)

fif.ip.t0.drug = fif.ip.t0.drug%>%
  filter(!duplicated(ref))

fif.ip.t0.drug = fif.ip.t0.drug%>%filter(Admission.Source.Indicator == "A&E")
saveRDS(fif.ip.t0.drug, "ip.t0_13May.rds")
##################find Fu readmission

los.28dread = gap7%>%
  filter(gap.time <= 28 & gap.time.mark == 1)

los.28dread$fu <-  fif.ip.t0.drug$fu [match(los.28dread$ref, fif.ip.t0.drug$ref)]
names(los.28dread)[3] <- "old.end"
los.28dread = los.28dread%>% mutate(end.day = case_when(is.na(old.end) ~ fu,
                                                        TRUE ~ old.end),
                                    los = end.day - start.day)

los.28dread$los <- as.integer(los.28dread$los)
summary(los.28dread$los)

fif.ip.t0.drug$key <- 1
los.28dread$key <- fif.ip.t0.drug$key[match(los.28dread$ref, fif.ip.t0.drug$ref)]
los.28dread = filter(los.28dread, !is.na(key))
summary(los.28dread$los[los.28dread$los > 0])
hist(los.28dread$los[los.28dread$los > 0])

saveRDS(los.28dread, "read.ip.los.rds")
