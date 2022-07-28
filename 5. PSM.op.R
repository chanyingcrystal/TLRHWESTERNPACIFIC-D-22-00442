library(dplyr)
library(tableone)
library(WeightIt)
library(survival)
library(survey)
dc.t0 <- readRDS("dc.t0_17May.rds")
names(dc.t0)

library(dplyr)
dc.t0 =  dc.t0 %>%
  mutate(paycode = case_when(
    grepl("DGS", `Paycode (OPAS)`) == T ~"Non PA",
    grepl("DH", `Paycode (OPAS)`) == T ~"Non PA",
    grepl("EP", `Paycode (OPAS)`) == T ~"Non PA",
    grepl("GAZ", `Paycode (OPAS)`) == T ~"PA",
    grepl("HA", `Paycode (OPAS)`) == T ~"Non PA",
    grepl("NE", `Paycode (OPAS)`) == T ~"Non PA",
    `Paycode (OPAS)` == "P"  ~ "PA",
    `Paycode (OPAS)` == "PA"~ "PA",
    grepl("RH", `Paycode (OPAS)`) == T ~"Non PA",
    TRUE ~ as.character(`Paycode (EIS)`)))


dc.t0 =  dc.t0 %>%
  mutate(paycode.bi = case_when(paycode == "PA" ~ "PA",
    TRUE ~ "Non PA"))
dc.t0 = dc.t0%>% 
  mutate(chin = case_when(grepl("CH", `Race Code`) == T ~ "Yes",
                     TRUE ~ "No"))

dc.t0$age <- as.integer(dc.t0$`Age On Appointment (Year)`)
dc.t0<- mutate(dc.t0, age.gp = case_when(age <60 ~ 1,
                                                           age >= 60 & age <=69 ~2,
                                                           age >= 70 & age <=79 ~3,
                                                           age >= 80 & age <=89 ~4,
                                                           age >= 90 ~5,
                                                           TRUE ~ NA_real_))

dc.t0$age.gp <- factor(dc.t0$age.gp, levels = c("1", "2", "3","4","5"),
                                labels = c("<60", "60-69", "70-79","80-89",">=90"))

dc.t0 = mutate(dc.t0, use.antiviral = case_when(antiviral == "Did not use" ~ 0,
                                                                  TRUE ~1))
dc.t0$use.antiviral <- factor(dc.t0$use.antiviral, levels = c("0", "1"), labels = c("Did not use", "Used Paxlovid/Molnupiravir"))                        

dc.t0 = dc.t0%>%filter(age >= 60 | no.disease == 0)

# IPTW table 5
# dc.t0.rds
# 10 May 2022: population size is correct
library(dplyr)
library(tidyr)
dc.t0 = dc.t0%>%
  mutate_at(vars(mi:aids), ~replace_na(., 0))%>%
  mutate_at(vars(ane), ~replace_na(., 0))
tab5_df = dc.t0 %>%
  dplyr::select(ane, fu, Sex, age, age.gp, paycode.bi, chin,  mi:aids, use.antiviral, antiviral) %>%
  # age >= 60 or any comorb == 1
  dplyr::select(-age) %>%
  
  # replace NA in comorbs, factorise
  mutate_all(~as.factor(.))

tab5_df$fu <- as.integer(tab5_df$fu)
#tab5_df = tab5_df%>%filter(ane == 1)
# optional
tab5_df %>% filter_all((any_vars(is.na(.))))  
lapply(tab5_df, function(x) unique(x))
#optional
tab5 = tableone::CreateTableOne(data = tab5_df, strata = "use.antiviral", test = T)
print(tab5, smd = T)

tab5 = tableone::CreateTableOne(data = tab5_df, test = T)
print(tab5, smd = T)

tab5 = tableone::CreateTableOne(data = tab5_df, strata = "antiviral", test = T)
print(tab5, smd = T)

##################IPTW - one###############
col_removed = c("antiviral", "age","use.antiviral","dead"
                # alias of treatment 
                )
x_factors = colnames(tab5_df)
x_factors = x_factors[! x_factors %in% col_removed]
function_call<-paste0("weight.tab5 <- weightit(use.antiviral ~ ",paste(x_factors, collapse = "+"), 
                      ", data = tab5_df, method = \"ps\", 
              estimand = \"ATT\")"
)
eval(parse(text = function_call))
# balance of weighting
library(cobalt)
bal.tab(weight.tab5, un = T)
summary(weight.tab5)
tab5_df $ fu <- as.integer(tab5_df$fu)

clus.tab5 <- svydesign(id =~ 1, weights = weight.tab5$weights, data = tab5_df)
tab5_weighted = svyCreateTableOne(data = clus.tab5, strata = "use.antiviral", test = T)
print(tab5_weighted, smd = T)
tab5_weighted_Mat.1 <- print(tab5_weighted, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = T)
#tab5_weighted_Mat.1 = as.data.frame(cbind(" " = rownames(tab5_weighted_Mat.1),tab5_weighted_Mat.1))
library(TableOne)
tab5.1 = CreateTableOne(data = tab5_df, strata = "use.antiviral", test = T)
print(tab5.1, smd = T)
tab5 = CreateTableOne(data = tab5_df)

###reattendance
dc.t0 = dc.t0%>%
  filter(age >= 60 | no.disease == 0)

dc.t0$dc.read <- case_when(dc.t0$dc.read >= 2 ~ 1,
                           TRUE ~ 0)

summary(model1 <- glm(dc.read~ factor(Sex) + factor(use.antiviral) +age + factor(paycode.bi)+  factor(chin) + factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
                      +factor(canc)+factor(msld)+factor(metacanc)+factor(aids) , family="binomial", data=dc.t0))
exp(cbind(OR=coef(model1), confint(model1)))

#summary(model1 <- glm(unplan.7d~ factor(Sex) + factor(use.antiviral) +age + factor(paycode.bi)+  factor(chin) + factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
#                      +factor(canc)+factor(msld)+factor(metacanc)+factor(aids) , family="binomial", data=dc.t0))
#exp(cbind(OR=coef(model1), confint(model1)))

#summary(model1 <- glm(unplan.14d~ factor(Sex) + factor(use.antiviral) +age + factor(paycode.bi)+  factor(chin) + factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
#                      +factor(canc)+factor(msld)+factor(metacanc)+factor(aids) , family="binomial", data=dc.t0))
#exp(cbind(OR=coef(model1), confint(model1)))

#summary(model1 <- glm(unplan.21d~ factor(Sex) + factor(use.antiviral) +age + factor(paycode.bi)+  factor(chin) + factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
#                      +factor(canc)+factor(msld)+factor(metacanc)+factor(aids) , family="binomial", data=dc.t0))
#exp(cbind(OR=coef(model1), confint(model1)))

summary(model1 <- glm(unplan.28d~ factor(Sex) + factor(use.antiviral) +age + factor(paycode.bi)+  factor(chin) + factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
                      +factor(canc)+factor(msld)+factor(metacanc)+factor(aids) , family="binomial", data=dc.t0))
exp(cbind(OR=coef(model1), confint(model1)))


summary(model1 <- glm(dc.read~ factor(Sex) + factor(antiviral) +age + factor(paycode)+  factor(chin) + factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
                      +factor(canc)+factor(msld)+factor(metacanc)+factor(aids) , family="binomial", data=dc.t0))
exp(cbind(OR=coef(model1), confint(model1)))

summary(model1 <- glm(unplan.7d~ factor(Sex) + factor(antiviral) +age + factor(paycode)+  factor(chin) + factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
                      +factor(canc)+factor(msld)+factor(metacanc)+factor(aids) , family="binomial", data=dc.t0))
exp(cbind(OR=coef(model1), confint(model1)))

summary(model1 <- glm(unplan.14d~ factor(Sex) + factor(antiviral) +age + factor(paycode)+  factor(chin) + factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
                      +factor(canc)+factor(msld)+factor(metacanc)+factor(aids) , family="binomial", data=dc.t0))
exp(cbind(OR=coef(model1), confint(model1)))

summary(model1 <- glm(unplan.21d~ factor(Sex) + factor(antiviral) +age + factor(paycode)+  factor(chin) + factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
                      +factor(canc)+factor(msld)+factor(metacanc)+factor(aids) , family="binomial", data=dc.t0))
exp(cbind(OR=coef(model1), confint(model1)))

summary(model1 <- glm(unplan.28d~ factor(Sex) + factor(antiviral) +age + factor(paycode.bi)+  factor(chin) + factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
                      +factor(canc)+factor(msld)+factor(metacanc)+factor(aids) , family="binomial", data=dc.t0))
exp(cbind(OR=coef(model1), confint(model1)))

clus <- svydesign(id =~ 1, weights = weight.tab5$weights, data = dc.t0)

res <- svyglm(dc.read ~ use.antiviral, design = clus ,family = binomial)
summary(res)
"Odds ratios and confidence intervals"
exp(cbind(OR = coef(res), confint(res, level = 0.95)))
car::infIndexPlot(res)

res <- svyglm(unplan.7d ~ use.antiviral, design = clus ,family = binomial)
summary(res)
"Odds ratios and confidence intervals"
exp(cbind(OR = coef(res), confint(res, level = 0.95)))
car::infIndexPlot(res)

res <- svyglm(unplan.14d ~ use.antiviral, design = clus ,family = binomial)
summary(res)
"Odds ratios and confidence intervals"
exp(cbind(OR = coef(res), confint(res, level = 0.95)))

res <- svyglm(unplan.21d ~ use.antiviral, design = clus ,family = binomial)
summary(res)
"Odds ratios and confidence intervals"
exp(cbind(OR = coef(res), confint(res, level = 0.95)))

res <- svyglm(unplan.28d ~ use.antiviral, design = clus ,family = binomial)
summary(res)
"Odds ratios and confidence intervals"
exp(cbind(OR = coef(res), confint(res, level = 0.95)))

#######ane 
dc.t0$ane[is.na(dc.t0$ane)] <-0
summary(model1 <- glm(ane~ factor(Sex) + factor(use.antiviral) +age + factor(paycode.bi)+  factor(chin) + factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
                      +factor(canc)+factor(msld)+factor(metacanc)+factor(aids), family="binomial", data=dc.t0))
exp(cbind(OR=coef(model1), confint(model1)))

summary(model1 <- glm(ane~ factor(Sex) + factor(antiviral) +age + factor(paycode.bi)+  factor(chin) + factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
                      +factor(canc)+factor(msld)+factor(metacanc)+factor(aids), family="binomial", data=dc.t0))
exp(cbind(OR=coef(model1), confint(model1)))

clus <- svydesign(id =~ 1, weights = weight.tab5$weights, data = dc.t0)

res <- svyglm(ane ~ use.antiviral, design = clus ,family = binomial)
summary(res)
"Odds ratios and confidence intervals"
exp(cbind(OR = coef(res), confint(res, level = 0.95)))

########death
library(survival)
library(survminer)

dc.t0$dead <- as.integer(dc.t0$dead)
surv_object <- Surv(time = dc.t0$fu, event = dc.t0$dead)
fit1 <- survfit(surv_object ~ use.antiviral, data = dc.t0)
fit2 <- survfit(surv_object ~ antiviral, data = dc.t0)
dead.antiviral<- ggsurvplot(fit1, data = dc.t0, pval = TRUE, pval.method =  T, surv.median.line = "hv",
                            risk.table = TRUE, legend.title = "Antiviral use",               # Change legend titles
                            legend.labs = c("Did not use","Paxlovid/Molnupiravir"))


dead.antiviral.2<- ggsurvplot(fit2, data = dc.t0, pval = TRUE, pval.method =  T, surv.median.line = "hv",
                              risk.table = TRUE, legend.title = "Antiviral use",               # Change legend titles
                              legend.labs = c("Did not use","Paxlovid","Molnupiravir"))


fit.coxph1 <- coxph(surv_object ~Sex + use.antiviral + paycode.bi + chin  +age + mi + chf + pvd + cevd  + dementia+ cpd +rheumd +pud+mld+diab+diabwc+hp+rend
                    +canc+msld+metacanc+aids , data = dc.t0)
exp(cbind(HR=coef(fit.coxph1), confint(fit.coxph1)))


fit.coxph2 <- coxph(surv_object ~Sex + antiviral + paycode.bi + chin + age + mi + chf + pvd + cevd  + dementia+ cpd +rheumd +pud+mld+diab+diabwc+hp+rend
                    +canc+msld+metacanc+aids, data = dc.t0)

exp(cbind(HR=coef(fit.coxph2), confint(fit.coxph2)))

####IPTW
hr1 <- coxph(surv_object~ Sex + use.antiviral + paycode.bi + chin  +age + mi + chf + pvd + cevd  + dementia+ cpd +rheumd +pud+mld+diab+diabwc+hp+rend
             +canc+msld+metacanc+aids, weights = weight.tab5$weights, data = dc.t0) 
exp(cbind(HR=coef(hr1), confint(hr1)))



#####################IPTW two #####################
tab5_df$mi[is.na(tab5_df$mi)] <- 0
tab5_df$chf[is.na(tab5_df$chf)] <- 0

#tab5_df=tab5_df%>%filter(ane==0)

#tab5_df = tab5_df%>%select(-ane)

col_removed = c("antiviral", "age","use.antiviral","dead", "ane"
                # only 1 level, 0
)
x_factors = colnames(tab5_df)
x_factors = x_factors[! x_factors %in% col_removed]
# install package "mlogit"
function_call<-paste0("weight.tab5.two <- weightit(antiviral ~ ",paste(x_factors, collapse = "+"), 
                      ", data = tab5_df, method = \"ps\", use.mlogit = F,
              focal = \"Did not use\",
              estimand = \"ATT\")"
)
eval(parse(text = function_call))
# balance of weighting
library(cobalt)
bal.tab(weight.tab5.two, un = T)
summary(weight.tab5.two)

dc.t0$ane[is.na(dc.t0$ane)] <- 0

dc.t0.ane <- dc.t0 [dc.t0$ane == 1,]
dc.t0.no.ane <- dc.t0 [dc.t0$ane == 0,]
dc.t0.no.ane$los <- as.integer(dc.t0.ane$los)
dc.t0.no.ane$dead <- factor(dc.t0.no.ane$dead)
clus.tab5 <- svydesign(id =~ 1, weights = weight.tab5.two$weights, data = dc.t0.no.ane)
tab5_two = CreateTableOne(data = tab5_df, strata = "antiviral", test = T)
print(tab5_two, smd = T)

dc.t0$dc.read[is.na(dc.t0$dc.read)] <- 0
dc.t0$los <- as.integer(dc.t0$los)
dc.t0$ane.times[is.na(dc.t0$ane.times)]<-0
dc.t0$dead <- factor(dc.t0$dead)
clus <- svydesign(id =~ 1, weights = weight.tab5.two$weights, data = dc.t0)
tab5_weighted = svyCreateTableOne(var = c("total.drug.cost"),data = clus, strata = "antiviral", test = T)
print(tab5_weighted, smd = T)
tab5_weighted_Mat <- print(tab5_weighted, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = T)
tab5_weighted_Mat = as.data.frame(cbind(" " = rownames(tab5_weighted_Mat),tab5_weighted_Mat))
## Save to a xlsx
#writexl::write_xlsx(tab5_weighted_Mat, path = "./tab5_weighted_Mat.xlsx")
#saveRDS(weight.tab5, file = "./weight.tab5.rds")
#saveRDS(clus.tab5, file = "./svydesign.tab5.rds")

###reattendance+

dc.t0$dead <- factor(dc.t0$dead)
clus <- svydesign(id =~ 1, weights = weight.tab5.two$weights, data = dc.t0)

df = clus$variables

df.dead = df %>%
  filter(ane == 1)

clus <- svydesign(id =~ 1, weights = weight.tab5.two$weights, data = df.dead)





table(df.dead$dead, df.dead$antiviral)

res <- svyglm(dc.read ~ antiviral, design = clus ,family = binomial)
summary(res)
"Odds ratios and confidence intervals"
exp(cbind(OR = coef(res), confint(res, level = 0.95)))
car::infIndexPlot(res)

res <- svyglm(unplan.7d ~ antiviral, design = clus ,family = binomial)
summary(res)
"Odds ratios and confidence intervals"
exp(cbind(OR = coef(res), confint(res, level = 0.95)))
car::infIndexPlot(res)

res <- svyglm(unplan.14d ~ antiviral, design = clus ,family = binomial)
summary(res)
"Odds ratios and confidence intervals"
exp(cbind(OR = coef(res), confint(res, level = 0.95)))

res <- svyglm(unplan.21d ~ antiviral, design = clus ,family = binomial)
summary(res)
"Odds ratios and confidence intervals"
exp(cbind(OR = coef(res), confint(res, level = 0.95)))

res <- svyglm(unplan.28d ~ antiviral, design = clus ,family = binomial)
summary(res)
"Odds ratios and confidence intervals"
exp(cbind(OR = coef(res), confint(res, level = 0.95)))

#######ane 
res <- svyglm(ane ~ antiviral, design = clus ,family = binomial)
summary(res)
"Odds ratios and confidence intervals"
exp(cbind(OR = coef(res), confint(res, level = 0.95)))

########death
ibrary(survival)
library(survminer)
dc.t0$dead <- case_when(dc.t0$dead == "Yes" ~ 1,
                        TRUE ~ 0)

dc.t0$dead <- as.integer(dc.t0$dead)
surv_object <- Surv(time = dc.t0$FU.dead, event = dc.t0$dead)
fit1 <- survfit(surv_object ~ antiviral, data = dc.t0)

####IPTW
hr1 <- coxph(surv_object~ Sex + antiviral + paycode.bi + chin  +age + mi + chf + pvd + cevd  + dementia+ cpd +rheumd +pud+mld+diab+diabwc+hp+rend
             +canc+msld+metacanc+aids, weights = weight.tab5.two$weights, data = dc.t0) 
exp(cbind(HR=coef(hr1), confint(hr1)))

############################################################################################################
############################################## New iptw for who survived ##################################
############################################################################################################
dc.surv <- dc.t0 %>%
  filter(dead == 0)

dc.surv = dc.surv%>%
  # replace NA in comorbs, factorise
  dplyr::mutate_at(vars(Sex, age.gp, paycode.bi, chin,  mi:aids, use.antiviral, antiviral), ~as.factor(.)) 


tab5_df.surv = dc.surv %>%
  dplyr::select(fu, Sex, age.gp, paycode.bi, chin,  mi:aids, use.antiviral, antiviral) %>%
  # replace NA in comorbs, factorise
  dplyr::mutate_at(vars(Sex, age.gp, paycode.bi, chin,  mi:aids, use.antiviral, antiviral), ~as.factor(.)) 
# optional
tab5_df.surv %>% filter_all((any_vars(is.na(.))))  
lapply(tab5_df.surv, function(x) unique(x))
#optional
tab5 = CreateTableOne(data = tab5_df.surv, strata = "use.antiviral", test = T)
print(tab5, smd = T)



##################IPTW - one###############
col_removed = c("antiviral","use.antiviral"
                # alias of treatment 
)
x_factors = colnames(tab5_df.surv)
x_factors = x_factors[! x_factors %in% col_removed]
function_call<-paste0("weight.tab5 <- weightit(use.antiviral ~ ",paste(x_factors, collapse = "+"), 
                      ", data = tab5_df.surv, method = \"ps\", 
              estimand = \"ATT\")"
)
eval(parse(text = function_call))
# balance of weighting
bal.tab(weight.tab5, un = T)
summary(weight.tab5)

clus.tab5 <- svydesign(id =~ 1, weights = weight.tab5$weights, data = tab5_df.surv)
tab5_weighted = svyCreateTableOne(data = clus.tab5, strata = "use.antiviral", test = T)
print(tab5_weighted, smd = T)
tab5_weighted_Mat.1 <- print(tab5_weighted, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = T)
#tab5_weighted_Mat.1 = as.data.frame(cbind(" " = rownames(tab5_weighted_Mat.1),tab5_weighted_Mat.1))


col_removed = c("antiviral","use.antiviral","dead", "ane"
                # only 1 level, 0
)
x_factors = colnames(tab5_df)
x_factors = x_factors[! x_factors %in% col_removed]
# install package "mlogit"
function_call<-paste0("weight.tab5.two <- weightit(antiviral ~ ",paste(x_factors, collapse = "+"), 
                      ", data = tab5_df.surv, method = \"ps\", use.mlogit = F,
              focal = \"Did not use\",
              estimand = \"ATT\")"
)
eval(parse(text = function_call))

### LOS
library(MASS)

dc.surv$fu <- as.integer(dc.surv$fu)
dc.surv$los <- as.integer(dc.surv$los)
m1 <- MASS::glm.nb(los ~  factor(Sex) + factor(use.antiviral) + factor(paycode.bi) + factor(chin)  + age + mi +chf + pvd + cevd  + dementia+ cpd +rheumd +pud+mld+diab+diabwc+hp+rend
                   +canc+msld+metacanc+aids+offset(log(fu)),data =  dc.surv)

summary(m1)
est <- cbind(Estimate = coef(m1), confint(m1))
exp(est)

dc.surv$los <- as.integer(dc.surv$los)

m1 <-pscl::zeroinfl(los ~  factor(Sex) + factor(antiviral) + factor(paycode) + factor(chin)  + factor(age.gp) + factor(mi) + factor(chf) + factor(pvd) + factor(cevd)  + factor(dementia)+ factor(cpd) +factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+
                      factor(rend) +factor(canc)+factor(aids) + offset(log(fu))|1,data =  dc.surv, dist = "negbin", weights = weight.tab5.two$weights)

summary(m1)
est <- cbind(Estimate = coef(m1), confint(m1))
exp(est)
