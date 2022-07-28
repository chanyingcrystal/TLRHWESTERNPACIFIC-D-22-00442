#import antiviral drugs records
library(dplyr)
library(purrr)
#dir <- "C:/Users/chany/OneDrive - The Chinese University of Hong Kong/4. Research Projects/2021.08 Abraham sepsis/4. covid/"
dir <- "C:/Users/crystalchan/OneDrive - The Chinese University of Hong Kong/4. Research Projects/2021.08 Abraham sepsis/4. covid/"

antiviral <- list.files(dir, pattern = "_Antiviral-COVID_.+DRG.+xls", full.names = T)   %>%
  map_df(~read.csv(., colClasses="character")) %>%
  bind_rows

names(antiviral)[1] <- "ref"
antiviral=antiviral%>% distinct()%>% arrange(Dispensing.Date..yyyy.mm.dd.)%>%filter(!Dispensing.Date..yyyy.mm.dd.=="")

#designated clinic
dc <- list.files(dir, pattern = "ith_drug", full.names = T)   %>%
  map_df(~read.csv(., colClasses="character")) %>%
  bind_rows

names(dc)[1] <- "ref"
dc = dc%>% distinct()%>% filter(!Appointment.Date..yyyy.mm.dd.=="")

#dc.supp
dir <- "C:/Users/crystalchan/OneDrive - The Chinese University of Hong Kong/4. Research Projects/2021.08 Abraham sepsis/4. covid/full.dc/"
library(readxl)
dc.supp <- list.files(dir, pattern ="Clinic", full.names = T)%>%
  map_df(~read_xlsx(., col_types="text")) %>%
  bind_rows
names(dc.supp)[1] <- "ref"

dc.supp = dc.supp%>% distinct()%>% filter(!`Appointment Date (yyyy-mm-dd)`=="")



#IP PCR records
dir <- "C:/Users/crystalchan/OneDrive - The Chinese University of Hong Kong/4. Research Projects/2021.08 Abraham sepsis/4. covid/"

pcr <- list.files(dir, pattern = "_Hosp_.+(PCR).+IP", full.names = T)%>%
  map_df(~read.csv(., colClasses="character")) %>%
  bind_rows
names(pcr)[1] <- "ref"
pcr = pcr%>% distinct()%>% filter(!Admission.Date..yyyy.mm.dd.=="")


#IP diagnosis records
dx <- list.files(dir, pattern = "_Hosp.+(Dx).+IP", full.names = T)%>%
  map_df(~read.csv(., colClasses="character")) %>%
  bind_rows

names(dx)[1] <- "ref"
dx = dx%>% distinct()%>% filter(!Admission.Date..yyyy.mm.dd.=="")

#DC ip records HAVE A LOT OF PROBLEM
dir <- "C:/Users/crystalchan/OneDrive - The Chinese University of Hong Kong/4. Research Projects/2021.08 Abraham sepsis/4. covid/"

dc.hospital <- list.files(dir, pattern = "_Hosp.+DC)_IP", full.names = T)%>%
   map_df(~read.csv(., colClasses="character")) %>%
   bind_rows
  
names(dc.hospital)[1] <- "ref"
dc.hospital = dc.hospital%>% distinct()%>% filter(!Admission.Date..yyyy.mm.dd.=="")


#antimicrobial
dir <- "C:/Users/crystalchan/OneDrive - The Chinese University of Hong Kong/4. Research Projects/2021.08 Abraham sepsis/4. covid/"

antimicrobial <- list.files(dir, pattern = "Antimicrobial", full.names = T)%>%
  map_df(~read.csv(., colClasses="character")) %>%
  bind_rows

names(antimicrobial)[1] <- "ref"
antimicrobial = antimicrobial%>%distinct()%>%filter(Dispensing.Date..yyyy.mm.dd. !=  "")

#dc demographic
library(readxl)
dc.demo <- list.files(dir, pattern = "Sex_Age", full.names = T)%>%
  map_df(~read_xlsx(., col_types="text")) %>%
  bind_rows

#dc.demo <- read.csv("C:/Users/chany/OneDrive - The Chinese University of Hong Kong/4. Research Projects/2021.08 Abraham sepsis/4. covid/6371262_COVID-19_202202DC_demographics_OP.csv", 
#                    colClasses="character")
names(dc.demo)[1] <- "ref"


#dc.age
#dc.age <- list.files(dir, pattern = "_Sex_Age.xls", full.names = T)%>%
#  map_df(~read.csv(., colClasses="character")) %>%
#  bind_rows
#names(dc.age)[1] <- "ref"

#dc details march
#dc.earlymar <- list.files(dir, pattern = "_full", full.names = T)%>%
#  map_df(~read.csv(., colClasses="character")) %>%
#  bind_rows

#dx list
#dx.ref <- list.files(dir, pattern = "_DX.csv", full.names = T)%>%
#  map_df(~read.csv(., colClasses="character")) %>%
#  bind_rows

#names(dx.ref)[1] <- "ref"


#plasma
plasma <- list.files(dir, pattern = "Plasma", full.names = T)%>%
  map_df(~read.csv(., colClasses="character")) %>%
  bind_rows
names(plasma)[1] <- "ref"

plasma = plasma%>% distinct()%>% filter(!Blood.Product.Issued.Date..yyyy.mm.dd.HH.MM.=="")

#interferon
interferon <- list.files(dir, pattern = "IFN__Immune", full.names = T)%>%
  map_df(~read.csv(., colClasses="character")) %>%
  bind_rows
names(interferon)[1] <- "ref"

interferon = interferon%>% distinct()%>% filter(!Dispensing.Date..yyyy.mm.dd.=="")

#CT 
library(readxl)
ct <- list.files(dir, pattern = "Test_POS", full.names = T)%>%
  map_df(~read_xlsx(., col_types="text"))%>%
  bind_rows
names(ct)[1] <- "ref"
ct = ct%>% distinct()%>% filter(!`LIS Reference Datetime`=="")

ct$`LIS Reference Datetime` <- as.Date(ct$`LIS Reference Datetime`)

library(stringr)
ct$pcr<- str_detect(ct$`LIS Test Description`,"PCR")
pcr.date = ct%>% filter(pcr == 1)%>% filter(`LIS Result` == "Detected" | `LIS Result` == "Detected  **")
rm(ct)

#comorbidty
library(readxl)
library(dplyr)
dir <- "C:/Users/crystalchan/OneDrive - The Chinese University of Hong Kong/4. Research Projects/2021.08 Abraham sepsis/4. covid/"
como <- list.files(dir, pattern = "CCI", full.names = T)%>%
  map_df(~read_xlsx(., col_types="text")) %>%
  bind_rows
names(como)[1] <- "ref"

como = como%>% distinct()%>% filter(!`All Diagnosis Code (ICD9)`=="")
como$`Reference Date` <- as.Date(como$`Reference Date`)

library(comorbidity)
como$ref <- as.numeric(como$ref)
names(como)[2]<-"all.diagnosis.code"
cci <- comorbidity(x=como, id="ref", code = "all.diagnosis.code", map = "charlson_icd9_quan", assign0 = FALSE)
rm(como)
library(tidyr)
cci= cci%>%mutate_at(vars(mi:aids), ~replace_na(., 0))%>%mutate(no.disease = ifelse(mi == 0 & chf == 0 & pvd == 0 & cevd == 0 & cpd == 0& rheumd == 0 & pud == 0 & mld ==0 & diab == 0& diabwc == 0& hp == 0 & rend == 0 & canc == 0 & msld == 0& metacanc == 0 & aids == 0,1,0))


#####death causes
library(readxl)
death <- read_xlsx("C:/Users/crystalchan/OneDrive - The Chinese University of Hong Kong/4. Research Projects/2021.08 Abraham sepsis/4. covid/death_03.xlsx",col_types = "text")
death = death%>% distinct()%>% filter(!is.na(`Death Cause (Main Cause)`))

death = death%>%
  mutate(covid = case_when(`Death Cause (Main Cause)` == "B342" | `Death Cause (Main Cause)` == "U071" | `Death Cause (Main Cause)` == "J184"~ 1, 
                           TRUE ~ 0),
         sepsis = case_when(`Death Cause (Main Cause)` == "A415" | `Death Cause (Main Cause)` == "A419" ~ 1, 
                            TRUE ~ 0),
         cancer = case_when(grepl ("C", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                            TRUE ~ 0),
         benign.neoplasms = case_when(grepl ("D1", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                                      grepl ("D2", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                                      grepl ("D3", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                                      grepl ("D4", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                                      TRUE ~ 0),
         diabetes = case_when(grepl ("E", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                            TRUE ~ 0),
         dementia = case_when(grepl ("F", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                              TRUE ~ 0),
         nervous.sys = case_when(grepl ("G", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                                 TRUE ~ 0),
         ischemic.heart = case_when(grepl ("I20", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                                             grepl ("I21", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                                             grepl ("I22", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                                             grepl ("I23", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                                             grepl ("I24", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                                             grepl ("I25", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                                             TRUE ~ 0),
         pulmonary.heart =  case_when(grepl ("I26", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                                      grepl ("I27", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                                      grepl ("I28", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                                      TRUE ~ 0),
         other.heart =  case_when(grepl ("I3", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                                      grepl ("I4", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                                      grepl ("I5", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                                      TRUE ~ 0),
         stoke = case_when(grepl ("I6", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                           TRUE ~ 0),
         respiratory = case_when(`Death Cause (Main Cause)` == "B342" | `Death Cause (Main Cause)` == "U071" | `Death Cause (Main Cause)` == "J184"~ 1,
                                 grepl ("J", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                                 TRUE ~ 0),
         skin = case_when(grepl ("L", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                          TRUE ~ 0),
         musculoskeletal = case_when(grepl ("M", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                                     TRUE ~ 0),
         genitourinary = case_when(grepl ("N", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                                   TRUE ~ 0),
         shock =  case_when(grepl ("R5", `Death Cause (Main Cause)`) == "TRUE" ~ 1,
                            TRUE ~ 0)
         )
fif.ip.t0.drug$key <- 1
death$ip <- fif.ip.t0.drug$key [match(death$`Reference Key`, fif.ip.t0.drug$ref)]
death.ip = death%>%filter(!is.na(death$ip))
fif.ip.t0.drug$death.icd <- 
names(death.ip)[1] <-"ref"

library(tidyr)
tab.death =  death.ip %>%
  dplyr::select(ref, covid,sepsis,cancer,benign.neoplasms,diabetes,dementia,nervous.sys,ischemic.heart,pulmonary.heart,other.heart,stoke,respiratory,skin,musculoskeletal,genitourinary,shock) %>%
  mutate_at(vars(covid:shock), ~replace_na(., 0)) %>%
  # factor all variables
  mutate_all(~as.factor(.))

tab.death = tab.death%>%arrange(covid)%>%
  filter(!duplicated(ref))

tab.death$ref <- as.integer(tab.death$ref)
death.cause <- left_join(fif.ip.t0.drug, death.ip, by = "ref")
death.cause = death.cause%>%
  mutate(heart = case_when(ischemic.heart == 1 | pulmonary.heart ==1 | other.heart == 1 ~ 1,
                           TRUE ~ 0),
         others = case_when(diabetes == 0 & ischemic.heart == 0 & pulmonary.heart ==0 & other.heart == 0&
                              covid == 0& cancer ==0 & stoke  ==0&nervous.sys ==0&genitourinary ==0 ~ 1,
                            TRUE ~ 0),
         all = case_when(diabetes == 1 | ischemic.heart == 1 | pulmonary.heart ==1 | other.heart == 1|
                           covid == 1| cancer ==1 | stoke  ==1|nervous.sys ==1|genitourinary ==1 |others ==1 ~ 1,
                         TRUE ~ 0))
var = c("covid", "cancer", "heart", "stoke", "nervous.sys", "genitourinary", "diabetes", "others", "all")
death.cause <- death.cause[death.cause$all ==1,]
death.t1 <- CreateCatTable(var=var,data = death.cause, test =T)
print(death.t1)
death.t1 <- CreateCatTable(var=var,data = death.cause, strata = "antiviral", test =T)
print(death.t1, smd = T)

death.cause$all <- factor(death.cause$all)
death.cause$covid <- factor(death.cause$covid)
death.cause$cancer <- factor(death.cause$cancer)
death.cause$heart <- factor(death.cause$heart)
death.cause$stoke <- factor(death.cause$stoke)
death.cause$nervous.sys <- factor(death.cause$nervous.sys)
death.cause$genitourinary <- factor(death.cause$genitourinary)
death.cause$diabetes <- factor(death.cause$diabetes)
death.cause$others <- factor(death.cause$others)

#clus <- svydesign(id =~ 1, weights =~ weight.tab1.twodrugs$weights, data = death.cause)

#tab1_weighted = svyCreateTableOne(var = var, data = clus, strata = "antiviral", test = T)
#print(tab1_weighted, smd = T)
#tab1_weighted_Mat_two <- print(tab1_weighted, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = T)
#tab1_weighted_Mat_two = as.data.frame(cbind(" " = rownames(tab1_weighted_Mat_two),tab1_weighted_Mat_two))
#writexl::write_xlsx(tab1_weighted_Mat_two, path = "./tab1drug.xlsx")

dc.t0$key <- 1
death$op <- dc.t0$key [match(death$`Reference Key`, dc.t0$ref)]
death.op = death%>%filter(!is.na(death$op))
names(death.op)[1] <-"ref"

library(tidyr)
tab.death =  death.op %>%
  dplyr::select(ref, covid,sepsis,cancer,benign.neoplasms,diabetes,dementia,nervous.sys,ischemic.heart,pulmonary.heart,other.heart,stoke,respiratory,skin,musculoskeletal,genitourinary,shock) %>%
  mutate_at(vars(covid:shock), ~replace_na(., 0)) %>%
  # factor all variables
  mutate_all(~as.factor(.))

tab.death = tab.death%>%arrange(covid)%>%
  filter(!duplicated(ref))

tab.death$ref <- as.integer(tab.death$ref)
death.cause <- left_join(dc.t0, death.op, by = "ref")
death.cause = death.cause%>%
  mutate(heart = case_when(ischemic.heart == 1 | pulmonary.heart ==1 | other.heart == 1 ~ 1,
                           TRUE ~ 0),
         others = case_when(diabetes == 0 & ischemic.heart == 0 & pulmonary.heart ==0 & other.heart == 0&
                              covid == 0& cancer ==0 & stoke  ==0&nervous.sys ==0&genitourinary ==0 ~ 1,
                            TRUE ~ 0),
         all = case_when(diabetes == 1 | ischemic.heart == 1 | pulmonary.heart ==1 | other.heart == 1|
                           covid == 1| cancer ==1 | stoke  ==1|nervous.sys ==1|genitourinary ==1 |others ==1 ~ 1,
                         TRUE ~ 0))
var = c("covid", "cancer", "heart", "stoke", "nervous.sys", "genitourinary", "diabetes", "others", "all")
death.cause <- death.cause[death.cause$all ==1,]
death.t1 <- CreateCatTable(var=var,data = death.cause, test =T)
print(death.t1)
death.t1 <- CreateCatTable(var=var,data = death.cause, strata = "antiviral", test =T)
print(death.t1, smd = T)

death.cause$all <- factor(death.cause$all)
death.cause$covid <- factor(death.cause$covid)
death.cause$cancer <- factor(death.cause$cancer)
death.cause$heart <- factor(death.cause$heart)
death.cause$stoke <- factor(death.cause$stoke)
death.cause$nervous.sys <- factor(death.cause$nervous.sys)
death.cause$genitourinary <- factor(death.cause$genitourinary)
death.cause$diabetes <- factor(death.cause$diabetes)
death.cause$others <- factor(death.cause$others)
