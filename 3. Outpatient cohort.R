#create designated clinic cohort

dc.t0.demo <- left_join(dc.t0, dc.supp, by = "ref")
dc.t0.demo = dc.t0.demo%>% group_by(ref)%>%arrange(Appointment.Date..yyyy.mm.dd.)%>%ungroup()%>%filter(!duplicated(ref))

dc.t0.demo$start.episode <- dc.t0.demo$Appointment.Date..yyyy.mm.dd.
dc.t0.demo$start.episode <- as.Date(dc.t0.demo$start.episode)
dc.t0.demo$end.episode <- "2022-04-27"
dc.t0.demo$end.episode <- as.Date(dc.t0.demo$end.episode)

#readmission
dc.read = dc%>% #one row for one patient in one visit
  group_by(ref, Appointment.Date..yyyy.mm.dd.)%>%
  filter(!duplicated(ref))%>%
  ungroup()%>%
  group_by(ref)%>%
  mutate(k = 1)%>%
  summarise(dc.read = sum(k)) #count = number of reattendance to designated clinic

dc.t0.demo$dc.read <- dc.read$dc.read [match(dc.t0.demo$ref, dc.read$ref)]

dc$start.episode <- dc.t0.demo$start.episode [match(dc$ref, dc.t0.demo$ref)]

class(dc$Appointment.Date..yyyy.mm.dd.)

dc.read.week = dc%>% #one row for one patient in one visit
  group_by(ref, Appointment.Date..yyyy.mm.dd.)%>%
  filter(!duplicated(ref))%>%
  ungroup()%>%
  mutate(days = Appointment.Date..yyyy.mm.dd. - start.episode)%>%
  filter(days != 0)%>%
  group_by(ref, days)%>%
  mutate(k = 1)%>%
  summarise(dc.read = sum(k)) 


unplanned7 <- dc.read.week%>%filter(days<=7)
unplanned7$mark <-1
unplanned14 <- dc.read.week%>%filter(days<=14)
unplanned14$mark <-1

unplanned21 <- dc.read.week%>%filter(days<=21)
unplanned21$mark <-1
unplanned28 <- dc.read.week%>%filter(days<=28)
unplanned28$mark <-1



#AnE 
dc.ane = dc.hospital%>%filter(Emergency.Admission..Y.N. == "Y")
dc.ane$key <- 1

names(dc.hospital)[1] <- "ref"
dc.ane = dc.ane%>% distinct()%>% filter(!Admission.Date..yyyy.mm.dd.=="")
dc.t0.demo$key <- 1
dc.ane$t0 <- dc.t0.demo$key [match(dc.ane$ref, dc.t0.demo$ref)]
dc.ane = dc.ane%>%filter(!is.na(t0))

dc.ane$start.episode <- dc.t0.demo$start.episode [match(dc.ane$ref, dc.t0.demo$ref)]
dc.ane$start.episode <- as.Date(dc.ane$start.episode)
dc.ane$Admission.Date..yyyy.mm.dd. <- as.Date(dc.ane$Admission.Date..yyyy.mm.dd.)
dc.ane$ane.fu <- dc.ane$Admission.Date..yyyy.mm.dd. - dc.ane$start.episode 

dc.ane = filter(dc.ane, ane.fu>=0)
dc.ane = filter(dc.ane, ane.fu<=28)
dc.ane = dc.ane%>% 
  group_by(ref)%>%
  summarise(ane = sum(key))

dc.t0.demo$ane.times <- dc.ane$ane [match(dc.t0.demo$ref, dc.ane$ref)]
dc.t0.demo$ane <- ifelse(dc.t0.demo$ane.times >=1, 1, 0)

#death
dc.dead = dc.hospital%>%filter(Discharge.Status == "DEATH")
dc.dead$key <- 1
dc.t0.demo$dead <- dc.dead$key [match(dc.t0.demo$ref, dc.dead$ref)]
dc.t0.demo$dead[is.na(dc.t0.demo$dead)] <- 0
dc.t0.demo$dead.date <- dc.dead$Discharge.Date..yyyy.mm.dd.[match(dc.t0.demo$ref, dc.dead$ref)]
dc.t0.demo$dead.date <- as.Date(dc.t0.demo$dead.date)
dc.t0.demo$dead.time <- dc.t0.demo$dead.date - dc.t0.demo$start.episode
dc.t0.demo$fu <- case_when(is.na(dc.t0.demo$dead.date) ~ dc.t0.demo$end.episode - dc.t0.demo$start.episode,
                           TRUE ~ dc.t0.demo$dead.time)

#cci
dc.t0$key <- 1
cci$dc <-dc.t0$key [match(cci$ref, dc.t0$ref)]
table(cci$dc)

cci.dc <- cci[cci$dc ==1,]

rm(dc.t0)
cci$ref <- as.character(cci$ref)
dc.t0 <- left_join(dc.t0.demo, cci, by = "ref")


dc.t0$unplan.7d <- unplanned7$mark [match(dc.t0$ref, unplanned7$ref)]
dc.t0$unplan.7d [is.na(dc.t0$unplan.7d )] <-0
dc.t0$unplan.14d <- unplanned14$mark [match(dc.t0$ref, unplanned14$ref)]
dc.t0$unplan.14d [is.na(dc.t0$unplan.14d )] <-0
dc.t0$unplan.21d <- unplanned21$mark [match(dc.t0$ref, unplanned21$ref)]
dc.t0$unplan.21d [is.na(dc.t0$unplan.21d )] <-0
dc.t0$unplan.28d <- unplanned28$mark [match(dc.t0$ref, unplanned28$ref)]
dc.t0$unplan.28d [is.na(dc.t0$unplan.28d )] <-0

dc.t0$antiviral <- dc.drug$antiviral [match(dc.t0$ref, dc.drug$ref)]
dc.t0$antiviral[is.na(dc.t0$antiviral)] <- 0

dc.t0$antiviral <- factor(dc.t0$antiviral, levels = c("0", "1","2"),
                       labels = c("Did not use", "Paxlovid only", "Molnupiravir only"))
dc.t0$antiviral.date <- dc.drug$Appointment.Date..yyyy.mm.dd. [match(dc.t0$ref, dc.drug$ref)]
dc.t0$start.drug <- dc.t0$end.episode - dc.t0$antiviral.date

saveRDS(dc.t0, "dc.t0_13May.rds")