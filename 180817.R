exam <-read.csv("csv_exam.csv")
setwd("C:\\Users\\user\\Documents\\RData")
getwd()

name <- data.frame(class=c(1,2,3,4,5), teacher = c("kim","lee","park","choi","jung"))
name
exam <-read.csv("csv_exam.csv")
exam_new <- left_join(exam,name,by="class")
install.packages("dplyr")
library(dplyr)

mpg
mpg <- as.data.frame(ggplot2::mpg)
fuel <-data.frame(f1=c("c","d","e","p","r"), price_fl=c(2.35,2.38,2.11,2.76,2.22),stringsAsFactors=F)
mpg1 <- left_join(mpg,fuel,by="fl")

value <-read.csv("1-4호선승하차승객수.csv")
value[,"1"]

midwest<- as.data.frame(ggplot2::midwest)
midwest
midwest2<-midwest %>%
  mutate(percent = popadults /poptotal)
midwest %>%
  mutate(percent = popadults /poptotal)%>%
  arrange(percent)%>%
  select(county,percent)%>%
  head(5)
  
midwest3<-midwest2 %>%
  mutate(grade=1-percent)
  
midwest2 %>%
  mutate(grade=1-percent)%>%
  mutate(grade2= ifelse(grade >=0.4, "large",
         ifelse(grade >=0.3, "middle","small")))

midwest4<-midwest2 %>%
  mutate(grade=1-percent)%>%
  mutate(grade2= ifelse(grade >=0.4, "large",
                        ifelse(grade >=0.3, "middle","small")))

count(midwest4,grade2)

midwest %>%
  mutate(asianpercent = popasian /poptotal)%>%
  arrange(asianpercent)%>%
  select(state,county,asianpercent)%>%
  head(10)

asianpercent


value <-read.csv("1-4호선승하차승객수.csv")

value2<-value %>%
  mutate(line_color=ifelse(노선번호 =="line_1","블루라인",ifelse(노선번호 =="line_2","그린라인",
                              ifelse(노선번호 =="line_3","레드라인",
                                                           "엘로우라인") )))

value2 %>%
  group_by(노선번호) %>%
  summarise(sum=sum(승차))
         
value2 %>%
  group_by(노선번호) %>%
  summarise(하차=sum(하차))

value2 %>%
  summarise(승차=sum(승차),
              하차=sum(하차))

value2 %>%
  group_by(노선번호) %>%
  summarise(승차하차=sum(승차)+sum(하차))

value2 %>%
  group_by(노선번호) %>%
  summarise(승차=sum(승차))

value2 %>%
  group_by(노선번호) %>%
  summarise(하차=sum(하차))


mpg4$drv <- ifelse(mpg$drv %in% c("4","r","f"), mpg4$drv, NA)



df<-data.frame(sex=c("M","F",NA,"M","F"),score=c(5,5,3,4,NA))
df
is.na(df)
table(is.na(df))


boxplot(mpg$hwy)
mpg <- as.data.frame(ggplot::mpg)
install.packages("ggplot")
library(mpg)
mpg


mpg[c(10,14,58,93),"drv"] <- "k"
mpg[c(29,43,129,203),"cty"] <-c(3,4,39,42)

is.na(mpg$drv)
mpg$drv <- ifelse(is.na(mpg$drv) ,NA,mpg$drv )
mpg

mpg_3<-na.omit(mpg)
mpg_3
mpg3<-as.data.frame(ggplot2::mpg)


mpg4<-mpg
table(is.na(mpg4$drv))
mpg4$drv <- ifelse(mpg4$drv=="k",NA, mpg4$drv)

mpg4$drv <- ifelse(mpg$drv %in% c("4","r","f"), mpg4$drv, NA)
table(is.na(mpg4$drv))
mpg4 %>%  summarise(mean_drv = mean(drv, na.rm = T))



library(ggplot2)
install.packages("ggplot2")
ggplot(data=mpg , aes(x=displ, y=hwy))
ggplot(data=mpg,aes(x=displ,y=hwy))+geom_point()+xlim(3,6)+ylim(10,30)

library(dplyr)

df_mpg<-mpg %>%
  group_by(drv)%>%
  summarise(mean_hwy = mean(hwy))

df_mpg
ggplot(data=df_mpg, aes(x=drv,y=mean_hwy))+geom_col()
ggplot(data=df_mpg, aes(x=reorder(drv,-mean_hwy), y=mean_hwy))+geom_col()
ggplot(data=mpg, aes(x=drv)) + geom_bar()

df_mpg <-mpg %>%
  filter(class=="suv") %>%
  group_by(manufacturer) %>%
  summarise(sum_cty=mean(cty)) %>%
  arrange(desc(sum_cty))%>%
  head(5)            
df_mpg

ggplot(data=mpg, aes(x=class,fill=class))+geom_bar()

ggplot(data=df_mpg,aes(x=reorder(manufacturer,-sum_cty),y=sum_cty,fill=sum_cty))+geom_col()

ggplot(data=mpg,aes(x=class))+geom_bar()

ggplot(data=economics, aes(x=date,y=unemploy))+geom_line



install.packages()

library(dplyr)

welfare <- left_join(welfare,list_job,id="code_job")

library(readxl)
list_job <- read_excel("../RData/Koweps_Codebook.xlsx", col_names = T,sheet=2)
head(list_job)
dim(list_job)

welfare %>%
  filter(!is.na(code_job)) %>%
  select(code_job,job) %>%
  head(10)

job_income <- welfare%>%
  filter(!is.na(job) & !is.na(income)) %>%
  group_by(job) %>%
  summarise(mean_income = mean(income))

head(job_income)

top10 <- job_income %>%
  arrange(desc(mean_income)) %>%
  head(10)
library(ggplot2)
#reorder는 뒤집어서 출력한다. 해당 x 를 반대로 나오게한다. 
ggplot(data = top10 , aes(x=reorder(job,mean_income), y= mean_income))+geom_col()+coord_flip()

bottom10 <- job_income %>%
  arrange(mean_income) %>%
  head(10)

ggplot(data = bottom10 , aes ( x= reorder(job,-mean_income),y=mean_income))+
  geom_col()+
  coord_flip()

job_male <- welfare %>%
  filter(!is.na(job) & sex=="male") %>%
  group_by(job) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  head(10)
job_male

job_female <- welfare %>%
  filter(!is.na(job) & sex== "female") %>%
  group_by(job) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  head(10)

job_female

ggplot(data = job_male, aes(x=reorder(job,n), y= n )) +geom_col()+coord_flip()
ggplot(data = job_female, aes(x=reorder(job,n), y= n )) +geom_col()+coord_flip()

class(welfare$religion)
table(welfare$religion)

welfare$religion <- ifelse(welfare$religion == 1 , "yes","no")
table(welfare$ religion)

qplot(welfare$religion)

class(welfare$marriage)
table(welfare$marriage)

welfare$group_marrige<- ifelse(welfare$marriage == 1, "marriage",
                        ifelse(welfare$marriage == 3, "divorce",NA))

table(welfare$group_marrige)

table(is.na(welfare$group_marrige))

qplot(welfare$group_marrige)

religion_marrige<- welfare %>%
  filter(!is.na(group_marrige)) %>%
  group_by(religion,group_marrige) %>%
  summarise(n=n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct=round(n/tot_group*100,1))

religion_marrige


religion_marrige<-welfare %>%
  filter(!is.na(group_marrige)) %>%
  count(religion,group_marrige) %>%
  group_by(religion) %>%
  mutate(pct = round(n/sum(n) * 100,1))

religion_marrige

divorce <- religion_marrige %>%
  filter(group_marrige=="divorce") %>%
  select(religion , pct)
divorce

ggplot(data=divorce , aes(x=religion, y= pct)) + geom_col()

ageg_marriage <-welfare %>%
  filter(!is.na(group_marrige)) %>%
  group_by(ageg, group_marrige) %>%
  summarise(n=n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct=round(n/tot_group*100,1))

ageg_divorce <- ageg_marriage %>%
  filter(ageg !="young" & group_marrige== "divorce") %>%
  select(ageg,pct)

ageg_divorce

ggplot(data=ageg_divorce, aes(x=ageg, y=pct))+geom_col()

ageg_religion_marriage <- welfare %>%
  filter(!is.na(group_marrige) & ageg != "young") %>%
  group_by(ageg,religion,group_marrige) %>%
  summarise(n=n()) %>%
  mutate(tot_group =sum(n)) %>%
  mutate(pct = round(n/tot_group * 100 , 1))
ageg_religion_marriage         

ageg_religion_marriage<-welfare %>%
  filter(!is.na(group_marrige) & ageg != "young") %>%
  count(ageg , religion, group_marrige) %>%
  group_by(ageg,religion) %>%
  mutate(pct=round(n/sum(n) * 100 , 1))

ageg_religion_marriage

df_divorce <- ageg_religion_marriage %>%
  filter(group_marrige == "divorce") %>%
  select(ageg,religion , pct)
df_divorce


ggplot(data=df_divorce , aes(x= ageg , y=pct, fill = religion )) + geom_col(position = "dodge")

class(welfare$code_region)
table(welfare$code_region)

list_region <- data.frame(code_region = c(1:7),
                          region = c ("서울","수도권(인천/경기","부산/경남/울산",
                                      "대구/경북","대전/충남","강원/충북",
                                      "광주/전남/전북/제주도"))

list_region

welfare <- left_join(welfare, list_region, id= " code_region")

welfare %>%
  select(code_region, region) %>%
  head

region_ageg <- welfare %>%
  group_by(region,ageg) %>%
  summarise(n=n()) %>%
  mutate(tot_group= sum(n)) %>%
  mutate(pct=round ( n/tot_group *100,2))

head(region_ageg)

region_ageg<- welfare %>%
  count(region,ageg) %>%
  group_by(region) %>%
  mutate(pct=round(n/sum(n) * 100, 2))

ggplot(data=region_ageg , aes(x=region,y=pct, fill = ageg))+geom_col()+coord_flip()


list_order_old <- region_ageg %>% 
  filter(ageg=="old") %>%
  arrange(pct)

list_order_old

order <- list_order_old$region
order

ggplot(data=region_ageg , aes(x=region , y=pct, fill=ageg)) + geom_col()+
  coord_flip() + scale_x_discrete(limits=order)

class(region_ageg$ageg)

levels(region_ageg$ageg)

region_ageg$ageg <- factor(region_ageg$ageg,level=c("old","middle","young"))

class(region_ageg$ageg)

levels(region_ageg$ageg)

ggplot(data=region_ageg, aes(x=region , y=pct, fill=ageg)) +geom_col()+
  coord_flip()+scale_x_discrete(limits=order)
