library(dplyr)
library(tableone)
library(WeightIt)
library(survival)
library(survey)

fif.ip.t0.drug <- readRDS("ip.t0_13May.rds")
#fif.ip.t0.drug = fif.ip.t0.drug[,-c(104)]

fif.ip.t0.drug$age <- as.integer(fif.ip.t0.drug$Admission.Age..Year...episode.based.)

fif.ip.t0.drug$molnupiravir <- factor(fif.ip.t0.drug$molnupiravir, levels = c("0", "1"),
                                      labels = c("No", "Yes"))
fif.ip.t0.drug$paxlovid <- factor(fif.ip.t0.drug$paxlovid, levels = c("0", "1"),
                                  labels = c("No", "Yes"))
fif.ip.t0.drug = fif.ip.t0.drug%>%
  mutate (antiviral = case_when(molnupiravir == "Yes" & paxlovid == "Yes" ~3,
                                molnupiravir == "Yes" & paxlovid == "No" ~2,
                                molnupiravir == "No" & paxlovid == "Yes" ~1,
                                molnupiravir == "No" & paxlovid == "No" ~0))
fif.ip.t0.drug$antiviral <- factor(fif.ip.t0.drug$antiviral, levels = c("0", "1","2"),
                                   labels = c("Did not use", "Paxlovid only", "Molnupiravir only"))

fif.ip.t0.drug = mutate(fif.ip.t0.drug, use.antiviral = case_when(antiviral == "Did not use" ~ 0,
                                                                  TRUE ~1))
fif.ip.t0.drug$use.antiviral <- factor(fif.ip.t0.drug$use.antiviral, levels = c("0", "1"), labels = c("Did not use", "Used Paxlovid/Molnupiravir"))                        

fif.ip.t0.drug<- fif.ip.t0.drug %>% mutate(age.gp = case_when(age <60 ~ 1,
                                                            age >= 60 & age <=69 ~2,
                                                            age >= 70 & age <=79 ~3,
                                                            age >= 80 & age <=89 ~4,
                                                            age >= 90 ~5,
                                                            TRUE ~ NA_real_),
                                           eq5d.age = case_when(age >=18 & age <=24 ~ 1,
                                                    age >=25 & age <=34 ~2,
                                                    age >= 35 & age <=44 ~3,
                                                    age >= 45 & age <=54 ~4,
                                                    age >= 55 & age <= 64 ~5,
                                                    age >= 65 ~ 6,
                                                    TRUE ~ NA_real_))
                                           
                      

fif.ip.t0.drug$age.gp <- factor(fif.ip.t0.drug$age.gp, levels = c("1", "2", "3","4","5"),
                                labels = c("<60", "60-69", "70-79","80-89",">=90"))

fif.ip.t0.drug = fif.ip.t0.drug%>%filter(start.drug <= 7)

######## readmission stay 

# IPTW table 1
# analysis.R > for fif.ip.t0.drug.rds and ip.t0.nonop.RDS, aka table 1
# 10 May 2022: population size is different

fif.ip.t0.drug = fif.ip.t0.drug%>% filter(age >= 60 | no.disease == 0)

library(readxl)
dir <- "C:/Users/crystalchan/OneDrive - The Chinese University of Hong Kong/4. Research Projects/2021.08 Abraham sepsis/4. covid/"
lifetable <-read_xlsx("life table 2022.xlsx", col_types="text")
fif.ip.t0.drug <- mutate(fif.ip.t0.drug, eq5d.weight = case_when(eq5d.age == 1 ~ 0.938,
                                                                 eq5d.age == 2 ~ 0.935,
                                                                 eq5d.age ==3 ~ 0.942,
                                                                 eq5d.age == 4~ 0.925,
                                                                 eq5d.age == 5~ 0.894,
                                                                 eq5d.age == 6 ~ 0.884,
                                                                 TRUE ~ 0.938))

fif.ip.t0.drug$life.F <- lifetable$female [match(fif.ip.t0.drug$age, lifetable$age)]
fif.ip.t0.drug$life.M <- lifetable$male [match(fif.ip.t0.drug$age, lifetable$age)]
fif.ip.t0.drug$life.F[fif.ip.t0.drug$Sex == "M"] <- 0
fif.ip.t0.drug$life.M[fif.ip.t0.drug$Sex == "F"] <- 0
fif.ip.t0.drug$life.F <- as.double(fif.ip.t0.drug$life.F)
fif.ip.t0.drug$life.M <- as.double(fif.ip.t0.drug$life.M)
fif.ip.t0.drug$life=fif.ip.t0.drug$life.F + fif.ip.t0.drug$life.M
fif.ip.t0.drug$qaly = fif.ip.t0.drug$eq5d.weight * fif.ip.t0.drug$life

library(tidyr)
fif.ip.t0.drug = fif.ip.t0.drug%>%
  mutate_at(vars(mi:no.disease), ~replace_na(., 0))
fif.ip.t0.drug$los <- as.numeric(fif.ip.t0.drug$los)
tab1df = fif.ip.t0.drug %>%
  dplyr::select(Sex, 
         Admission.from.Elderly.Home..Y.N., 
         fu,
         antiviral,
         use.antiviral,
         # comorbs 
         mi:aids,
         age.gp
  ) %>%
  mutate_at(vars(mi:aids), ~replace_na(., 0)) %>%
  # factor all variables
  mutate_all(~as.factor(.))
# check NA
tab1df  %>% filter_all((any_vars(is.na(.))))  
tab1df$fu <- as.integer(tab1df$fu)
# check NA in sex and age
tab1df  %>% filter_at(vars(c(1:5)), any_vars(is.na(.)))

fif.ip.t0.drug$los <- as.integer(fif.ip.t0.drug$los)
fif.ip.t0.drug$fu <- as.integer(fif.ip.t0.drug$fu)

tab1 = CreateTableOne(var = "los" ,data = fif.ip.t0.drug, strata = "antiviral", test = T, smd = T)
print(tab1, smd =T)

var = c("fu","Sex","age.gp","Admission.from.Elderly.Home..Y.N.","mi","chf","pvd","cevd","dementia","cpd","rheumd","pud","mld","diab","diabwc","hp","rend","canc","msld","metacanc","aids","dead")
varF = c("mi","chf","pvd","cevd","dementia","cpd","rheumd","pud","mld","diab","diabwc","hp","rend","canc","msld","metacanc","aids")
fif.ip.t0.drug[varF] <- lapply(fif.ip.t0.drug[varF], factor)
fif.ip.t0.drug$fu <- as.integer(fif.ip.t0.drug$fu)
fif.ip.t0.drug$dead <- factor(fif.ip.t0.drug$dead)
tab1.all = CreateTableOne(var = var, data = fif.ip.t0.drug, strata = "antiviral",  test = T)
print(tab1.all, smd =T)

tab1 = CreateTableOne(strata  = "antiviral", data = tab1df,  test = T)
print(tab1, smd =T)

print(tab1, smd = T)
# weightit 
col_removed = c("antiviral", "use.antiviral", "age", 
                # alias of treatment
                "antiviral.course")
x_factors = colnames(tab1df)
x_factors = x_factors[! x_factors %in% col_removed]
function_call<-paste0("weight.tab1 <- weightit(use.antiviral ~ ",paste(x_factors, collapse = "+"), 
                      ", data = tab1df, method = \"ps\", 
              estimand = \"ATT\")"
)
eval(parse(text = function_call))
# lapply(tab1df, function(x) unique(x))
# balance of weighting (optional)
#install.packages("cobalt")
library(cobalt)
bal.tab(weight.tab1, un = T)
summary(weight.tab1)

clus.tab1 <- svydesign(id =~ 1, weights = weight.tab1$weights, data = tab1df)
tab1_weighted = svyCreateTableOne(data = clus.tab1, strata = "use.antiviral", test = T)
print(tab1_weighted, smd = T)
tab1_weighted_Mat <- print(tab1_weighted, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = T)
tab1_weighted_Mat = as.data.frame(cbind(" " = rownames(tab1_weighted_Mat),tab1_weighted_Mat))
#tab1.all <- as.data.frame(cbind(" " = rownames(tab1.all),tab1.all))
#tab1 <- as.data.frame(cbind(" " = rownames(tab1),tab1))

## Save to a xlsx
writexl::write_xlsx(tab1, path = "./tab1.xlsx")
writexl::write_xlsx(tab1.all, path = "./tab1.all.xlsx")
writexl::write_xlsx(tab1_weighted_Mat, path = "./tab1_weighted_Mat.xlsx")
saveRDS(weight.tab1, file = "./weight.tab1.rds")
saveRDS(clus.tab1, file = "./svydesign.tab1.rds")

#readmission
#summary(model1 <- glm(unplan.7d~ factor(Sex) + factor(use.antiviral)+factor(Admission.from.Elderly.Home..Y.N.) +age +factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
#                      +factor(canc), family="binomial", data=fif.ip.t0.drug))
#exp(cbind(OR=coef(model1), confint(model1)))

#summary(model1 <- glm(unplan.14d~ factor(Sex) +  factor(use.antiviral)+factor(Admission.from.Elderly.Home..Y.N.)+age +factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
#                      +factor(canc), family="binomial", data=fif.ip.t0.drug))
#exp(cbind(OR=coef(model1), confint(model1)))

#summary(model1 <- glm(unplan.21d~ factor(Sex) +  factor(use.antiviral)+factor(Admission.from.Elderly.Home..Y.N.)+age +factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
#                      +factor(canc), family="binomial", data=fif.ip.t0.drug))
#exp(cbind(OR=coef(model1), confint(model1)))

#summary(model1 <- glm(unplan.28d~ factor(Sex) +  factor(use.antiviral)+factor(Admission.from.Elderly.Home..Y.N.)+age +factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
#                      +factor(canc), family="binomial", data=fif.ip.t0.drug))
#exp(cbind(OR=coef(model1), confint(model1)))

#weight glm 
library(survey)
clus <- svydesign(id =~ 1, weights =~ weight.tab1$weights, data = fif.ip.t0.drug)
#res <- svyglm(unplan.7d ~ use.antiviral, design = clus ,family = binomial)
#summary(res)
#"Odds ratios and confidence intervals"
#exp(cbind(OR = coef(res), confint(res, level = 0.95)))
#car::infIndexPlot(res)

#res <- svyglm(unplan.14d ~ use.antiviral, design = clus ,family = binomial)
#summary(res)
"Odds ratios and confidence intervals"
#exp(cbind(OR = coef(res), confint(res, level = 0.95)))

#res <- svyglm(unplan.21d ~ use.antiviral, design = clus ,family = binomial)
#summary(res)
"Odds ratios and confidence intervals"
#exp(cbind(OR = coef(res), confint(res, level = 0.95)))

res <- svyglm(unplan.28d ~ use.antiviral, design = clus ,family = binomial)
car::vif(res)
summary(res)
"Odds ratios and confidence intervals"
exp(cbind(OR = coef(res), confint(res, level = 0.95)))
car::infIndexPlot(res, vars = "Cook")


model.data <- broom::augment(res) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)
library(ggplot2)
ggplot2::ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = unplan.28d), alpha = .5) +
  theme_bw()


#fif.ip.t0.drug<- mutate(fif.ip.t0.drug, los.st.gp1 = case_when(los.st <=4.421 ~ 1,
#                                                               los.st > 4.421 & los.st <=8.400 ~ 2,
#                                                               los.st > 8.400 & los.st <=15.359 ~3,
#                                                               TRUE ~ 4
#                                                               ))
#test <- nnet::multinom(los.st.gp1 ~ factor(Sex) + factor(antiviral)+ factor(Admission.from.Elderly.Home..Y.N.) +factor(age.gp2) +factor(ami)+factor(chf)+factor(pvd)+factor(cevd)+factor(copd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
#                                      +factor(canc)+factor(msld) +factor(metacanc) +factor(aids) , data = fif.ip.t0.drug[fif.ip.t0.drug$dead== "No",])
#summary(test)
#exp(coef(test))

#library(censReg)
#fif.ip.t0.drug$los.st <- as.double(fif.ip.t0.drug$los.st)
#summary(model2a <- censReg(los.st~factor(Sex) + factor(antiviral)+ factor(Admission.from.Elderly.Home..Y.N.) +factor(age.gp2) +factor(ami)+factor(chf)+factor(pvd)+factor(cevd)+factor(copd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
#                           +factor(canc)+factor(msld) +factor(metacanc) +factor(aids) , left = 0, right = 28, data = fif.ip.t0.drug[fif.ip.t0.drug$dead== "No",]))
#confint(model2a)

library(MASS)
fif.ip.t0.drug$fu <- as.integer(fif.ip.t0.drug$fu)
fif.ip.t0.drug$los <- as.integer(fif.ip.t0.drug$los)
m1 <-glm.nb(los ~  factor(Sex) + factor(use.antiviral)+factor(Admission.from.Elderly.Home..Y.N.) +age +factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
            +factor(canc) + offset(log(fu)),data =  fif.ip.t0.drug[fif.ip.t0.drug$dead==0,])

summary(m1)
est <- cbind(Estimate = coef(m1), confint(m1))
exp(est)

fif.ip.t0.drug$fu <- as.integer(fif.ip.t0.drug$fu)
fif.ip.t0.drug$los <- as.integer(fif.ip.t0.drug$los)

m2 <-glm.nb(los ~ factor(Sex) + factor(antiviral)+factor(Admission.from.Elderly.Home..Y.N.) +age +factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
            +factor(canc) + offset(log(fu)),data =  fif.ip.t0.drug[fif.ip.t0.drug$dead== 0,])

summary(m2)
est <- cbind(Estimate = coef(m2), confint(m2))
exp(est)



#summary(model2a <- censReg(los~factor(Sex) + factor(use.antiviral)+ factor(Admission.from.Elderly.Home..Y.N.) +factor(age.gp2) +factor(ami)+factor(chf)+factor(pvd)+factor(cevd)+factor(copd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
#                           +factor(canc)+factor(msld) +factor(metacanc) +factor(aids) , left = 0, right = 52, data = fif.ip.t0.drug[fif.ip.t0.drug$dead== "No",]))

#
#fif.ip.t0.drug$los <- as.integer(fif.ip.t0.drug$los)
#summary(model2b <- censReg(los~factor(Sex) + factor(antiviral)+ factor(Admission.from.Elderly.Home..Y.N.) +age +factor(ami)+factor(chf)+factor(pvd)+factor(cevd)+factor(copd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
#                           +factor(canc) , left = 0, right = 52, data = fif.ip.t0.drug[fif.ip.t0.drug$dead== "No",]))
#confint(model2b)

#need to imputate a weighting for people who were dead
#trial


### survival
library(survival)
library(survminer)
fif.ip.t0.drug$dead <- as.integer(fif.ip.t0.drug$dead)

surv_object1 <- Surv(time = fif.ip.t0.drug$fu, event = fif.ip.t0.drug$dead)
fit1 <- survfit(surv_object1 ~ use.antiviral, data = fif.ip.t0.drug, weights = weight.tab1$weights)
fit2 <- survfit(surv_object1 ~ antiviral, data = fif.ip.t0.drug, weights = weight.tab1.twodrugs$weights)
summary(fit1)

pairwise_survdiff(Surv(time = fu, event = dead) ~ antiviral, data = fif.ip.t0.drug)

survdiff(data = fif.ip.t0.drug, formula = surv_object1 ~ antiviral)

dead.antiviral<- ggsurvplot(fit1, data = fif.ip.t0.drug, pval = TRUE, pval.method =  T, surv.median.line = "hv",
                            risk.table = TRUE, legend.title = "Antiviral use",               # Change legend titles
                            legend.labs = c("Did not use","Paxlovid/Molnupiravir"))


dead.antiviral.2<- ggsurvplot(fit2, fun = "cumhaz", data = fif.ip.t0.drug, pval = T, pval.method =  T, xlim = c(0,35),
                            risk.table = TRUE, legend.title = "Antiviral use",               # Change legend titles
                            legend.labs = c("Control","Nirmatrelvir-ritonavir","Molnupiravir"),xlab = "Follow-up days"
                            )

fit.coxph1 <- coxph(surv_object1 ~ Sex  + use.antiviral + Admission.from.Elderly.Home..Y.N. +age + mi + chf + pvd + cevd  + dementia+ cpd +rheumd +pud+mld+diab+diabwc+hp+rend
                   +canc , data = fif.ip.t0.drug)
exp(cbind(HR=coef(fit.coxph1), confint(fit.coxph1)))
ggforest(fit.coxph1, data = dead.ip)

fit.coxph2 <- coxph(surv_object1 ~ Sex  + antiviral + Admission.from.Elderly.Home..Y.N. +age + mi + chf + pvd + cevd  + dementia+ cpd +rheumd +pud+mld+diab+diabwc+hp+rend
                    +canc , data = fif.ip.t0.drug)

exp(cbind(HR=coef(fit.coxph2), confint(fit.coxph2)))
ggforest(fit.coxph2, data = dead.ip)

#####iptw one 
library(survival)
library(survminer)
hr <- coxph(surv_object1~ use.antiviral, data = fif.ip.t0.drug, weights = weight.tab1$weights) 
cox.zph(hr)
ggcoxzph(cox.zph(hr))

library(survival)
library(rms)  # one possible source for a `vif`-function .... there are many
cvif <- vif(  hr1  )

hr1 <- coxph(surv_object1~ strata(Sex)+ use.antiviral + strata(Admission.from.Elderly.Home..Y.N.) + strata(age.gp) + strata(mi) + chf + pvd + cevd  + dementia + strata(cpd) +rheumd + strata(pud)+ strata(mld)+diab+diabwc+ strata(hp)+rend
             +canc, data = fif.ip.t0.drug, weights = weight.tab1$weights) 
exp(cbind(HR=coef(hr1), confint(hr1)))
cox.zph(hr1)
ggcoxzph(cox.zph(hr1))
ggcoxdiagnostics(hr1, type = "deviance")


p1.1.iptw <- ggcoxdiagnostics(hr1, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())

hr1a <- coxph(surv_object1~ strata(Sex)+ use.antiviral + strata(Admission.from.Elderly.Home..Y.N.) + strata(age.gp) + strata(mi) + chf + pvd + cevd  + dementia + strata(cpd) +rheumd + strata(pud)+ strata(mld)+diab+diabwc+ strata(hp)+rend
              +canc, data = fif.ip.t0.drug) 
cox.zph(hr1a)
ggcoxzph(cox.zph(hr1a), font.submain = 6, font.x = 5, font.y = 5)

p1.1.un <- ggcoxdiagnostics(hr1a, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())

ggarrange(p1.1.un, p1.1.iptw, ncol = 2, nrow = 1, labels = c("Unadjusted", "IPTW"))

rms::vif(hr1)
rms::vif(hr1a)
rms::vif(hr2)
rms::vif(hr3)

library(survival)
hr2 <- coxph(surv_object1~ strata(Sex)+ antiviral + strata(Admission.from.Elderly.Home..Y.N.) + strata(age.gp) + strata(mi) + chf + pvd + cevd  + dementia + strata(cpd) +rheumd + strata(pud)+ strata(mld)+diab+diabwc+ strata(hp)+rend
             +canc, weights = weight.tab1.twodrugs$weights, data = fif.ip.t0.drug) 
summary(hr2)
cox.zph(hr2)
 ggcoxzph(cox.zph(hr2), font.submain = 6, font.x = 5, font.y = 5)
vif(hr2)

 
sf1.4.de<- ggcoxdiagnostics(hr2, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())


hr3 <- coxph(surv_object1~ strata(Sex)+ antiviral + strata(Admission.from.Elderly.Home..Y.N.) + strata(age.gp) + strata(mi) + chf + pvd + cevd  + dementia + strata(cpd) +rheumd + strata(pud)+ strata(mld)+diab+diabwc+ strata(hp)+rend
             +canc , data = fif.ip.t0.drug) 
summary(hr3)
cox.zph(hr3)
ggcoxzph(cox.zph(hr3), font.submain = 6, font.x = 5, font.y = 5)

sf1.4.un.de<- ggcoxdiagnostics(hr3, type = "deviance",
                            linear.predictions = FALSE, ggtheme = theme_bw())

ggarrange(sf1.4.un.de, sf1.4.de, ncol = 2, nrow = 1, labels = c("Unadjusted", "IPTW"))

##### IPTW using two 

# weightit 
library(tidyr)
tab1df.twodrug = fif.ip.t0.drug %>%
  dplyr::select(Sex, 
                fu,
                Admission.from.Elderly.Home..Y.N., 
                antiviral,
                use.antiviral,
                # comorbs 
                mi:aids,
                age.gp)%>%
  dplyr::mutate_at(vars(mi:aids), ~replace_na(., 0)) 
         
tab1df.twodrug$fu <- as.integer(tab1df.twodrug$fu)

col_removed = c("antiviral", "use.antiviral",
                # alias of treatment
                "antiviral.course")
x_factors = colnames(tab1df.twodrug)
x_factors = x_factors[! x_factors %in% col_removed]
function_call<-paste0("weight.tab1.twodrugs <- weightit(antiviral ~ ",paste(x_factors, collapse = "+"), 
                      ", data = tab1df.twodrug, stabilize = T, method = \"ps\",, use.mlogit = F,
              focal = \"Did not use\",
              estimand = \"ATT\")"
)
eval(parse(text = function_call))


library(cobalt)
bal.tab(weight.tab1.twodrugs, un = T)
summary(weight.tab1.twodrugs)

tab1df.twodrug[6:22] <- lapply(tab1df.twodrug[6:22], factor)
tab1df.twodrug[3] <- lapply(tab1df.twodrug[3], factor)
fif.ip.t0.drug$fu <- as.integer(fif.ip.t0.drug$fu)
fif.ip.t0.drug$antiviral.course <- factor(fif.ip.t0.drug$antiviral.course)

fif.ip.t0.drug$drug.cost[is.na(fif.ip.t0.drug$drug.cost)] <- 0
clus <- svydesign(id =~ 1, weights =~ weight.tab1.twodrugs$weights, data = fif.ip.t0.drug)


tab1_weighted.life = svyCreateTableOne(var=c("drug.cost","ane.times"), data = clus, strata = "antiviral", test = T)
print(tab1_weighted.life, smd = T)
tab1_weighted_Mat_two <- print(tab1_weighted, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = T)
tab1_weighted_Mat_two = as.data.frame(cbind(" " = rownames(tab1_weighted_Mat_two),tab1_weighted_Mat_two))
writexl::write_xlsx(tab1_weighted_Mat_two, path = "./tab1drug.xlsx")

tab1.two = CreateTableOne(var = "fu", data = fif.ip.t0.drug)
print(tab1.two, smd = T)

#### eq5d
clus <- svydesign(id =~ 1, weights =~ weight.tab1.twodrugs$weights, data = fif.ip.t0.drug)

tab1_weighted.life = svyCreateTableOne(var = "qaly" , data = clus, strata = "antiviral", test = T)
print(tab1_weighted.life, smd = T)


library(survey)


summary(model1 <- glm(unplan.7d~ factor(Sex) + factor(antiviral)+factor(Admission.from.Elderly.Home..Y.N.) +age +factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
                      +factor(canc), family="binomial", data=fif.ip.t0.drug))
exp(cbind(OR=coef(model1), confint(model1)))

summary(model1 <- glm(unplan.14d~ factor(Sex) +  factor(antiviral)+factor(Admission.from.Elderly.Home..Y.N.)+age +factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
                      +factor(canc), family="binomial", data=fif.ip.t0.drug))
exp(cbind(OR=coef(model1), confint(model1)))

summary(model1 <- glm(unplan.21d~ factor(Sex) +  factor(antiviral)+factor(Admission.from.Elderly.Home..Y.N.)+age +factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
                      +factor(canc), family="binomial", data=fif.ip.t0.drug))
exp(cbind(OR=coef(model1), confint(model1)))

summary(model1 <- glm(unplan.28d~ factor(Sex) +  factor(antiviral)+factor(Admission.from.Elderly.Home..Y.N.)+factor(age.gp) +factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
                      +factor(canc)+factor(msld)+factor(metacanc)+factor(aids), family="binomial", data=fif.ip.t0.drug))
exp(cbind(OR=coef(model1), confint(model1)))
car::vif(model1)
#
fif.ip.t0.drug$unplan.28d <- factor(fif.ip.t0.drug$unplan.28d, levels = c(0,1), labels = c("Did not re-admit", "Re-admitted"))
library(ggplotify)
library("grid")
p1 <-as.ggplot(as.grob(~plot(model1, which = 4, id.n = 3)))
# Extract model results
library(broom)
model.data <- augment(model1) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)

p2 <- ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = unplan.28d), alpha = .5) +
  theme_bw()
model.data %>% 
  filter(abs(.std.resid) > 3)
library(ggpubr)
ggarrange(p1, p2,
          labels = c("A", "B"),
          ncol = 1, nrow = 2)

####calculate life years 
clus.life <- svydesign(id =~ 1, weights =~ weight.tab1.twodrugs$weights, data = fif.ip.t0.drug)
tab1_weighted.life = svyCreateTableOne(var = "life", data = clus.life, strata = "antiviral", test = T)
print(tab1_weighted.life, smd = T)
fif.ip.t0.drug$life <- as.double(fif.ip.t0.drug$life)


clus <- svydesign(id =~ 1, weights =~ weight.tab1.twodrugs$weights, data = fif.ip.t0.drug)

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

#####iptw for people who did not die at entrance
library(survival)
hr1 <- coxph(surv_object1~ Sex  + antiviral + Admission.from.Elderly.Home..Y.N. +age + mi + chf + pvd + cevd  + dementia+ cpd +rheumd +pud+mld+diab+diabwc+hp+rend
             +canc , weights = weight.tab1.twodrugs$weights, data = fif.ip.t0.drug) 
summary(hr1)
cox.zph(fit.coxph1)
ggcoxzph(cox.zph(fit.coxph1))
############################################################################################################
############################################## New iptw for who survived ##################################
############################################################################################################
ip.surv <- fif.ip.t0.drug %>%
  filter(dead == 0)

tab1df.survived <-ip.surv%>%
  dplyr::select(Sex, 
         #fu,
         Admission.from.Elderly.Home..Y.N., 
         antiviral,
         use.antiviral,
         mi:aids,
         age.gp
         
  ) 
# check NA
tab1df.survived  %>% filter_all((any_vars(is.na(.))))  
# check NA in sex and age
tab1df.survived  %>% filter_at(vars(c(1:5)), any_vars(is.na(.)))

tab1df.survived[3] <- lapply(tab1df.survived[3], factor) 
tab1df.survived[5:22] <- lapply(tab1df.survived[5:22], factor) 

tab1.sur = CreateTableOne(data = tab1df.survived, strata = "use.antiviral", test = T, smd = T)
print(tab1.sur, smd =T)
tab1.sur.all = CreateTableOne(data = tab1df.survived,  test = T)
print(tab1.sur.all, smd =T)

col_removed = c("antiviral", "use.antiviral", "age",
                # alias of treatment
                "antiviral.course")
x_factors = colnames(tab1df.survived)
x_factors = x_factors[! x_factors %in% col_removed]
function_call<-paste0("weight.tab1.sur <- weightit(use.antiviral ~ ",paste(x_factors, collapse = "+"), 
                      ", data = tab1df.survived, method = \"ps\", 
              estimand = \"ATT\")"
)
eval(parse(text = function_call))

clus.tab1 <- svydesign(id =~ 1, weights = weight.tab1.sur$weights, data = ip.surv)
tab1_weighted = svyCreateTableOne(data = clus.tab1, strata = "use.antiviral", test = T)
tab1_weighted_Mat.sur <- print(tab1_weighted, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = T)

######LOS

library(MASS)
ip.surv$los <- as.integer(ip.surv$los)
ip.surv$fu <- as.integer(ip.surv$fu)

m1 <-glm.nb(los ~  factor(Sex) + factor(use.antiviral)+factor(Admission.from.Elderly.Home..Y.N.) +age +factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
            +factor(canc) + offset(log(fu)),weights = weight.tab1.sur$weights, data =  ip.surv)

summary(m1)
est <- cbind(Estimate = coef(m1), confint(m1))
exp(est)


############################IPTW using two drugs ###############################

# weightit 
col_removed = c("antiviral", "use.antiviral", "age", 
                # alias of treatment
                "antiviral.course")
x_factors = colnames(tab1df.survived)
x_factors = x_factors[! x_factors %in% col_removed]
function_call<-paste0("weight.tab1.twodrugs <- weightit(antiviral ~ ",paste(x_factors, collapse = "+"), 
                      ", data = tab1df.survived, method = \"ps\",, use.mlogit = F,
              focal = \"Did not use\",
              estimand = \"ATT\")"
)
eval(parse(text = function_call))

ip.surv$los <- as.integer(ip.surv$los)
clus <- svydesign(id =~ 1, weights =~ weight.tab1.twodrugs$weights, data = ip.surv)
tab1_weighted = svyCreateTableOne(var = "los",data = clus, strata = "antiviral", test = T)
print(tab1_weighted, smd = T)
tab1_weighted <- print(tab1_weighted, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = T)
tab1_weighted = as.data.frame(cbind(" " = rownames(tab1_weighted),tab1_weighted))
writexl::write_xlsx(tab1_weighted, path = "./tab1_weighted.xlsx")

x1 <- print(CreateTableOne(data = tab1df.survived, strata = "antiviral", test = T), smd=T)
x1 = as.data.frame(cbind(" " = rownames(x1),x1))

writexl::write_xlsx(x1, path = "./x2.xlsx")

######LOS
library(MASS)
ip.surv$los <- as.integer(ip.surv$los)
ip.surv$fu <- as.integer(ip.surv$fu)
m1 <-glm.nb(los ~  factor(Sex) + factor(antiviral)+factor(Admission.from.Elderly.Home..Y.N.) +factor(age.gp) +factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
            +factor(canc) + offset(log(fu)),weights = weight.tab1.twodrugs$weights, data =  ip.surv)

m1 <-glm.nb(los ~  factor(Sex) + factor(antiviral)+factor(Admission.from.Elderly.Home..Y.N.) +age +factor(no.disease) + offset(log(fu)), data =  ip.surv)


summary(m1)
est <- cbind(Estimate = coef(m1), confint(m1))
exp(est)

m1 <- MASS::glm.nb(los ~  factor(Sex) + factor(antiviral) + factor(Admission.from.Elderly.Home..Y.N.) + age + factor(mi) + factor(chf) + pvd + cevd  + dementia+ cpd +rheumd +pud+mld+diab+diabwc+hp+rend
                   +canc+msld+metacanc+aids+ offset(log(fu)),data =  ip.surv)

