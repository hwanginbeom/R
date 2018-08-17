install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
install.packages("readxl")

raw_welfare<-read.spss(file ="Koweps_hpc10_2015_beta1.sav",to.data.frame = T)

welfare<-raw_welfare
head(welfare)
tail(welfare)
view(welfare)
dim(welfare)
str(welfare)
summary(welfare)


welfare<-rename(welfare, sex=h10_g3,brith=h10_g4,marriage=h10_g10,religion=h10_g11,income=p1002_8aq1,code_job=h10_eco9,code_region=h10_reg7)


class(welfare$sex)
table(welfare$sex)
class(welfare$income)
table(welfare$income)

welfare$sex<-ifelse(welfare$sex==9 ,NA,welfare$sex)

table(is.na(welfare$sex))

welfare$sex<-ifelse(welfare$sex==1,"male","female")
table(welfare$sex)

qplot(welfare$sex)


class(welfare$income)

summary(welfare$income)

qplot(welfare$income)

qplot(welfare$income) + xlim(0,1000)


summary(welfare$income)

welfare$income <-ifelse(welfare$income %in% c(0,9999), NA,welfare$income)

table(is.na(welfare$income))

sex_income<- welfare %>%
  filter(!is.na(income)) %>%
  group_by(sex)%>%
  summarise(mean_income=mean(income))

sex_income


class(welfare$brith)

summary(welfare$brith)

welfare$age <-2015 - welfare$brith+1
summary(welfare$age)
qplot(welfare$age)

age_income<-welfare %>%
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income = mean(income))

head(age_income)

ggplot(data=age_income, aes(x=age,y=mean_income))+geom_line()

welfare<-welfare %>%
  mutate(ageg=ifelse(age<30 , "young",ifelse(age<=59,"middle","old")))

table(welfare$ageg)
qplot(welfare$ageg)

ageg_income<-welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg) %>%
  summarise(mean_income=mean(income))

ageg_income

ggplot(data=ageg_income, aes(x=ageg,y=mean_income)) +geom_col()

ggplot(data = ageg_income , aes(x= ageg,y=mean_income,fill=sex)) +geom_col()+
  scale_x_discrete(limits=c("young","middle", "old"))

sex_income<-welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg,sex) %>%
  summarise(mean_income=mean(income))


ggplot(data=sex_income, aes(x=ageg,y=mean_income, fill=sex))+geom_col()+
  scale_x_discrete(limits=c("young","middle","old"))



ggplot(data=sex_income, aes(x=ageg,y=mean_income,fill=sex))+geom_col(position="dodge")+scale_x_discrete(limits=c("young","middle","old"))

sex_age<