library(haven)
library(descr)
library(Matching)
library(rgenoud)
library(psych)
library(sandwich)
library(lmtest)
library(multiwayvcov)
library(survey)
library(margins)
library(ggeffects)
library(sjmisc)
library(ggplot2)
library(effects)
library(MASS)
library(ggeffects)

setwd("d:/dustin/Desktop/Desktop/ceu stats folder")
NDI19 <- read_dta("NDI_2019_July_04.08.19.dta")
names(NDI19)
options(scipen = 99)

freq(as_factor(NDI19$q3), w=NDI19$indwt)
freq(as_factor(NDI19$q45), w=NDI19$indwt)


NDI19$q45_r<-NDI19$q45
NDI19$q45_r[NDI19$q45_r==-3]<-NA
NDI19$q45_r[NDI19$q45_r<=-1]<-0
freq(as_factor(NDI19$q45_r), w=NDI19$indwt)
NDI19$q3_r<-NDI19$q3
NDI19$q3_r[NDI19$q3_r==1]<-0
NDI19$q3_r[NDI19$q3_r==2]<-0
NDI19$q3_r[NDI19$q3_r>=3]<-1
freq((NDI19$q3_r), w=NDI19$indwt)
freq(as_factor(NDI19$q3_r), w=NDI19$indwt)
crosstab(NDI19$q3_r, NDI19$q45_r, w= NDI19$indwt, prop.c = TRUE)

NDI19svy <- svydesign(id=~psu, strata=~substratum, 
                      weights=~indwt,  
                      data=NDI19)

table(as_factor(NDI19svy$variables$q74))
table(NDI19svy$variables$q74)
NDI19svy$variables$q74_r<-NDI19svy$variables$q74
NDI19svy$variables$q74_r[NDI19svy$variables$q74_r<=-1]<-NA
NDI19svy$variables$q74_r[NDI19svy$variables$q74_r<=3]<-1
NDI19svy$variables$q74_r[NDI19svy$variables$q74_r==4]<-2
NDI19svy$variables$q74_r[NDI19svy$variables$q74_r>=5]<-3

NDI19svy$variables$q76_1_r<-NDI19svy$variables$q76_1
NDI19svy$variables$q76_1_r[NDI19svy$variables$q76_1_r<=-1]<-NA

NDI19svy$variables$q76_2_r<-NDI19svy$variables$q76_2
NDI19svy$variables$q76_2_r[NDI19svy$variables$q76_2_r<=-1]<-NA

NDI19svy$variables$q76_3_r<-NDI19svy$variables$q76_3
NDI19svy$variables$q76_3_r[NDI19svy$variables$q76_3_r<=-1]<-NA

NDI19svy$variables$q76_4_r<-NDI19svy$variables$q76_4
NDI19svy$variables$q76_4_r[NDI19svy$variables$q76_4_r<=-1]<-NA

NDI19svy$variables$q76_5_r<-NDI19svy$variables$q76_5
NDI19svy$variables$q76_5_r[NDI19svy$variables$q76_5_r<=-1]<-NA

NDI19svy$variables$q76_6_r<-NDI19svy$variables$q76_6
NDI19svy$variables$q76_6_r[NDI19svy$variables$q76_6_r<=-1]<-NA

NDI19svy$variables$q76_7_r<-NDI19svy$variables$q76_7
NDI19svy$variables$q76_7_r[NDI19svy$variables$q76_7_r<=-1]<-NA

NDI19svy$variables$q76_8_r<-NDI19svy$variables$q76_8
NDI19svy$variables$q76_8_r[NDI19svy$variables$q76_8_r<=-1]<-NA

NDI19svy$variables$q76_9_r<-NDI19svy$variables$q76_9
NDI19svy$variables$q76_9_r[NDI19svy$variables$q76_9_r<=-1]<-NA

NDI19svy$variables$q76_10_r<-NDI19svy$variables$q76_10
NDI19svy$variables$q76_10_r[NDI19svy$variables$q76_10_r<=-1]<-NA

NDI19svy$variables$assets<-(NDI19svy$variables$q76_1_r+
                              NDI19svy$variables$q76_2_r+
                              NDI19svy$variables$q76_3_r+
                              NDI19svy$variables$q76_4_r+
                              NDI19svy$variables$q76_5_r+
                              NDI19svy$variables$q76_6_r+
                              NDI19svy$variables$q76_7_r+
                              NDI19svy$variables$q76_8_r+
                              NDI19svy$variables$q76_9_r+
                              NDI19svy$variables$q76_10_r)
NDI19svy$variables$age_r<-NDI19svy$variables$age
NDI19svy$variables$age_r[NDI19svy$variables$age_r<=35]<-1
NDI19svy$variables$age_r[NDI19svy$variables$age_r>=56]<-3
NDI19svy$variables$age_r[NDI19svy$variables$age_r>=36]<-2
NDI19svy$variables$age_r<-as.factor(NDI19svy$variables$age_r)

freq(NDI19svy$variables$q43)
NDI19svy$variables$q43_r<-NDI19svy$variables$q43
NDI19svy$variables$q43_r[NDI19svy$variables$q43_r<=-3]<-NA
freq(as_factor(NDI19svy$variables$q43_r), w=NDI19svy$variables$indwt)
crosstab(as_factor(NDI19svy$variables$q43_r), NDI19svy$variables$sex, w=NDI19svy$variables$indwt, prop.c = TRUE)
NDI19svy$variables$responsibilities<-NDI19svy$variables$q43_r
table(as_factor(NDI19svy$variables$responsibilities))
NDI19svy$variables$responsibilities[NDI19svy$variables$responsibilities<=-1]<-NA

table(as_factor(NDI19svy$variables$substratum))
NDI19svy$variables$substratum_r<-NDI19svy$variables$substratum
NDI19svy$variables$substratum_r[NDI19svy$variables$substratum_r==10]<-1
NDI19svy$variables$substratum_r[NDI19svy$variables$substratum_r==52]<-3
NDI19svy$variables$substratum_r[NDI19svy$variables$substratum_r==51]<-2
NDI19svy$variables$substratum_r[NDI19svy$variables$substratum_r>=41]<-3
NDI19svy$variables$substratum_r[NDI19svy$variables$substratum_r>=21]<-2
table(NDI19svy$variables$substratum_r)

crosstab(NDI19$q3_r, NDI19$q45_r, w= NDI19$indwt, prop.c = TRUE)
crosstab(NDI19svy$variables$q3_r, NDI19svy$variables$employfact, w= NDI19svy$variables$indwt, prop.c = TRUE)
crosstab(NDI19svy$variables$q3_r, NDI19svy$variables$sex, w= NDI19svy$variables$indwt, prop.c = TRUE)
crosstab(NDI19svy$variables$q3_r, NDI19svy$variables$age_r, w= NDI19svy$variables$indwt, prop.c = TRUE)
crosstab(NDI19svy$variables$q3_r, NDI19svy$variables$q74_r, w= NDI19svy$variables$indwt, prop.c = TRUE)
crosstab(NDI19svy$variables$q3_r, NDI19svy$variables$substratum_r, w= NDI19svy$variables$indwt, prop.c = TRUE)

NDI19svy$variables$q3_r_r<-NDI19svy$variables$q3_r
NDI19svy$variables$q3_r_r[NDI19svy$variables$q3_r_r<=-1]<-NA
NDI19svy$variables$q41_r<-NDI19svy$variables$q41
NDI19svy$variables$q41_r[NDI19svy$variables$q41_r<=-1]<-NA
NDI19svy$variables$q41_r[NDI19svy$variables$q41_r<=-1]<-0

NDI19svy$variables$employfact<-(NDI19svy$variables$q41_r+NDI19svy$variables$q45_r)
NDI19svy$variables$employfact<-as.factor(NDI19svy$variables$employfact)
NDI19svy$variables$q3_r_r<-as.factor(NDI19svy$variables$q3_r_r)
NDI19svy$variables$q45_r<-as.factor(NDI19svy$variables$q45_r)
table(NDI19svy$variables$q45_r)
table(NDI19svy$variables$q45)
table(NDI19svy$variables$q3_r_r)
table(NDI19svy$variables$q41)

NDI19svy$variables$q74_r<-as.factor(NDI19svy$variables$q74_r)
NDI19svy$variables$substratum_r<-as.factor(NDI19svy$variables$substratum_r)
table(NDI19svy$variables$employfact)
table(NDI19svy$variables$sex)
table(NDI19svy$variables$q3_r_r)
table(NDI19svy$variables$sex)

crosstab(NDI19svy$variables$q3_r_r, NDI19svy$variables$employfact, w= NDI19svy$variables$indwt, prop.c = TRUE)

NDI19svy$variables$sex<-as_factor(NDI19svy$variables$sex)

model1<-svyglm(q3_r_r~employfact*substratum_r+
                 employfact*assets+
                 sex+             
                 q74_r+ 
                 age, design = NDI19svy, family = "binomial")

summary(model1)

model2<-svyglm(q3_r_r~employfact+substratum_r+
                 assets+
                 sex+             
                 q74_r+ 
                 age, design = NDI19svy, family = "binomial")

summary(model1)

ggemmeans(model1, terms = c("employfact"), data = NDI19svy$variables)
plot(ggemmeans(model1, terms = c( "assets","employfact"), data = NDI19svy$variables))
x<-ggemmeans(model1, terms = c( "assets","employfact"), data = NDI19svy$variables)
x$predicted
plot(ggemmeans(model1, terms = c( "substratum_r","employfact"), data = NDI19svy$variables))
plot(ggemmeans(model1, terms = c("employfact"), data = NDI19svy$variables))
plot(ggemmeans(model1, terms = c("employfact"), data = NDI19svy$variables))
plot(ggemmeans(model1, terms = c("q74_r"), data = NDI19svy$variables))
plot(ggemmeans(model1, terms = c("substratum_r"), data = NDI19svy$variables))


