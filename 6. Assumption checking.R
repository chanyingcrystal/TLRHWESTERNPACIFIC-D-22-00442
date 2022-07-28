#load dataset
fif.ip.t0.drug <- readRDS("ip.t0.rds")

######COX REGRESSION

#IP

### survival
library(survival)
library(survminer)
fif.ip.t0.drug$dead <- as.integer(fif.ip.t0.drug$dead)
surv_object1 <- Surv(time = fif.ip.t0.drug$fu, event = fif.ip.t0.drug$dead)

fit.coxph1 <- coxph(surv_object1 ~ Sex  + Sex  + use.antiviral + Admission.from.Elderly.Home..Y.N. + age.gp + mi + chf + pvd + cevd  + dementia+ cpd +rheumd +pud+mld+diab+diabwc+hp+rend
                    +canc  , data = fif.ip.t0.drug)

cox.zph(fit.coxph1)
ggcoxzph(cox.zph(fit.coxph1))

fit.coxph2 <- coxph(surv_object1 ~ Sex  + antiviral*fu + age.gp + Admission.from.Elderly.Home..Y.N. + hp + mi + chf + pvd + cevd  + dementia+ cpd +rheumd +pud+mld+diab+diabwc+rend
                    +canc , data = fif.ip.t0.drug)
cox.zph(fit.coxph2)
ggcoxzph(cox.zph(fit.coxph2))




#check assumption for logistic regression

# OP - 28d reattend
summary(model3a <- glm(`28 days re-attendance`~ factor(Sex) + factor(use.antiviral) +factor(age.gp) + factor(paycode.bi)+  factor(chin) + factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
                      +factor(canc)+factor(msld)+factor(metacanc)+factor(aids) , family="binomial", data=dc.t0))
exp(cbind(OR=coef(model1), confint(model1)))

summary(model3b <- glm(`28 days re-attendance`~ factor(Sex) + factor(antiviral) +factor(age.gp) + factor(paycode.bi)+  factor(chin) + factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
                       +factor(canc)+factor(msld)+factor(metacanc)+factor(aids) , family="binomial", data=dc.t0))
exp(cbind(OR=coef(model1), confint(model1)))

car::vif(model3a)
car::vif(model3b)

dc.t0$`28 days re-attendance` <- dc.t0$unplan.28d

library("ggplotify")
library("grid")
p1 <-as.ggplot(as.grob(~plot(model3a, which = 4, id.n = 3)))



# Extract model results
library(broom)
model.data <- augment(model3a) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)

p2 <- ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = `28 days re-attendance`), alpha = .5) +
  theme_bw()

model.data %>% 
  filter(abs(.std.resid) > 3)%>%
  filter(.cooksd > 0.0001892327)

library(ggpubr)
ggarrange(p1, p2,
          labels = c("A", "B"),
          ncol = 1, nrow = 2)

car::vif(model4a)
car::vif(model4b)

#OP - 28d hostpial admission
dc.t0$ane[is.na(dc.t0$ane)] <-0
dc.t0$`28 days hospital admission` <- factor(dc.t0$ane, levels = c("0","1"), labels = c("Did not admit", "Admitted"))
summary(model4a <- glm(`28 days hospital admission`~ factor(Sex) + factor(use.antiviral) + factor(age.gp) + factor(paycode.bi)+  factor(chin) + factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
                      +factor(canc)+factor(msld)+factor(metacanc)+factor(aids), family="binomial", data=dc.t0))

summary(model4b <- glm(`28 days hospital admission`~ factor(Sex) + factor(antiviral) +factor(age.gp) + factor(paycode.bi)+  factor(chin) + factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
                      +factor(canc)+factor(msld)+factor(metacanc)+factor(aids), family="binomial", data=dc.t0))
vif(model4a)
vif(model4b)


library("ggplotify")
library("grid")
p1 <-as.ggplot(as.grob(~plot(model4b, which = 4, id.n = 3)))

# Extract model results
library(broom)
model.data <- augment(model4b) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)

p2 <- ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = `28 days hospital admission`), alpha = .5) +
  theme_bw()

model.data %>% 
  filter(abs(.std.resid) > 3)%>%
  filter(.cooksd > 1)

library(ggpubr)
ggarrange(p1, p2,
          labels = c("A", "B"),
          ncol = 1, nrow = 2)

#exclude outliner
refined_use.antiviral <- model.data %>% 
  filter(abs(.std.resid) <= 3 & .cooksd <= 1)
summary(
  model4a2 <- glm(`28 days hospital admission`~ `factor(Sex)` + `factor(use.antiviral)` + `factor(age.gp)` + `factor(paycode.bi)`+  `factor(chin)` +`factor(mi)`+`factor(chf)`+`factor(pvd)`+`factor(cevd)`+`factor(cpd)`+`factor(rheumd)` +`factor(pud)`+`factor(mld)`+`factor(diab)`+`factor(diabwc)`+`factor(hp)`+`factor(rend)`
                       +`factor(canc)`+`factor(msld)`+`factor(metacanc)`+`factor(aids)` , family="binomial", data=refined_use.antiviral))
exp(cbind(OR=coef(model4a2), confint(model4a2)))



#IP - 28d readmission
summary(model1 <- glm(unplan.28d~ Sex +  antiviral+Admission.from.Elderly.Home..Y.N.+age.gp +mi+chf+pvd+cevd+cpd+rheumd+pud+mld+diab+diabwc+hp+rend
                      +canc, family="binomial", data=fif.ip.t0.drug))
exp(cbind(OR=coef(model1), confint(model1)))

summary(model1b <- glm(unplan.28d~ factor(Sex) +  factor(use.antiviral)+factor(Admission.from.Elderly.Home..Y.N.)+age.gp +factor(mi)+factor(chf)+factor(pvd)+factor(cevd)+factor(cpd)+factor(rheumd) +factor(pud)+factor(mld)+factor(diab)+factor(diabwc)+factor(hp)+factor(rend)
                      +factor(canc), family="binomial", data=fif.ip.t0.drug))


#
fif.ip.t0.drug$unplan.28d <- factor(fif.ip.t0.drug$unplan.28d, levels = c(0,1), labels = c("Did not re-admit", "Re-admitted"))
library("ggplotify")
library("grid")
p1 <-as.ggplot(as.grob(~plot(model1b, which = 4, id.n = 3)))

# Extract model results
library(broom)
model.data <- augment(model1b) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)

p2 <- ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = unplan.28d), alpha = .5) +
  theme_bw()

model.data %>% 
  filter(abs(.std.resid) > 3)%>%
  filter(.cooksd > 0.0001892327)
  
library(ggpubr)
ggarrange(p1, p2,
          labels = c("A", "B"),
          ncol = 1, nrow = 2)

require(GGally)
bp2 <- fif.ip.t0.drug %>% dplyr::select(Sex ,  antiviral,Admission.from.Elderly.Home..Y.N.,age.gp ,mi,chf,pvd,cevd,cpd,rheumd,pud,mld,diab,diabwc,hp,rend
                                        ,canc)
ggcorr(bp2, label=TRUE)

car::vif(model1)
car::vif(model1b)


