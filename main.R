# Loading 
install.packages("tidyverse",dependencies = TRUE)
library(vroom)
library(mgcv)
library(lubridate)
library(tidyverse)
install.packages("slider")
library(slider)
library(weathercan)
library(tidymv)
library(gamm4)
library(splines)
library(zoo)
library(tidyverse)
library(gridExtra)
library(broom)

# Labels for plots below
l_parks<-"Park visits mobility index (100 = Jan 2020)"
l_nonres<-"Non-residential mobility index (100 = Jan 2020)"
l_month<-"Month"
l_week<-"Week"
l_ratio<-"Weekly COVID-19 growth rate"
l_cases<-"Cases (N)"
l_province<-"Province"
l_gap<-"Mobility gap"

# Geo units
small_provinces<-c("Northwest Territories", "Yukon", "Nunavut","Prince Edward Island", "New Brunswick","Newfoundland and Labrador")
regions<-c("Alberta","British Columbia","Manitoba","Nova Scotia","Ontario","Quebec", "Saskatchewan")
regions_list<-as.list(regions); 
regions_list$canada<-regions
names(regions_list)<-c(regions, "Canada")
regions_list


# get a confidence interval for a reg coefficient in position n
get<-function(x,n=2) {
  co<-coef(x)[n];
  sd<-sqrt(vcov(x))[n,n];
  c(co,co-1.96*sd,co+1.96*sd);
}

# get a formatted confidence interval from estimate and standard error
pasteci<-function(e,se) {
  e.<-sprintf("%.2f",exp(e))
  l.<-sprintf("%.2f",exp(e-1.96*se))
  u.<-sprintf("%.2f",exp(e+1.96*se))
  paste(e.," (",l.,", ",u.,")",sep="")
}

std<-function(x) { (x - mean(x)) / sd(x)}

miqr<-function(x){
  q<-quantile(x,c(.5,.25,.75),na.rm=T)
  f<-sprintf("%.1f",q)
  paste(f[1]," (",f[2],", ",f[3],")",sep="")
}
miqr0<-function(x){
  q<-quantile(x,c(.5,.25,.75),na.rm=T)
  f<-sprintf("%.0f",q)
  paste(f[1]," (",f[2],", ",f[3],")",sep="")
}

# Data prep
# today's directory

dir_out<-paste(getwd(), "/output/",Sys.Date(),sep="")
dir.create(dir_out, recursive=TRUE)
setwd(dir_out)

# A - Load mobility data

location<-"https://www.gstatic.com/covid19/mobility/2020_CA_Region_Mobility_Report.csv"
#location<-"/Users/kevinbrown/Dropbox/Kevin_2020/COVID_mobility_2/data/2020_CA_Region_Mobility_Report_20201215.csv"

mob_ca<-vroom(location,delim=",")
max(mob_ca$date)

mob_ca<-mob_ca %>% mutate(
  week = floor_date(mob_ca$date, "week"),
  prov = sub_region_1,
  work = 100 + (workplaces_percent_change_from_baseline),
  retail = 100 + (retail_and_recreation_percent_change_from_baseline),
  nonres = 100 - 100*(residential_percent_change_from_baseline)/30, #rescale 
  parks = 100 + (parks_percent_change_from_baseline)
) %>% filter(is.na(sub_region_2)==T)

# B - Load weather data

all<-c(52200,27174,43124,54239,49608,47707,888,50310,50089)
w<-weather_dl(all,start=ymd("2020-01-01"), end=Sys.Date(),  interval = "day")
w$week<-floor_date(w$date,"week")
w$province<-NULL
w$province[w$prov == "BC"] <- "British Columbia"
w$province[w$prov == "AB"] <- "Alberta"
w$province[w$prov == "SK"] <- "Saskatchewan"
w$province[w$prov == "MB"] <- "Manitoba"
w$province[w$prov == "ON"] <- "Ontario"
w$province[w$prov == "QC"] <- "Quebec"
w$province[w$prov == "NB"] <- "New Brunswick"
w$province[w$prov == "NS"] <- "Nova Scotia"
w$province[w$prov == "NL"] <- "Newfoundland and Labrador"

# C - Cases data
cases_can<-vroom("https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/timeseries_canada/cases_timeseries_canada.csv",delim=",")
cases_ca<-vroom("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_prov/cases_timeseries_prov.csv",delim=",")

cases_ca$date<-dmy(cases_ca$date_report)
cases_ca$province[cases_ca$province == "BC"] <- "British Columbia"
cases_ca$province[cases_ca$province == "NL"] <- "Newfoundland and Labrador"
cases_ca$province[cases_ca$province == "NWT"] <- "Northwest Territories"
cases_ca$province[cases_ca$province == "PEI"] <- "Prince Edward Island"

tests_ca<-vroom("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_prov/testing_timeseries_prov.csv",delim=",")
tests_ca$date<-dmy(tests_ca$date_testing)
tests_ca$province[tests_ca$province == "BC"] <- "British Columbia"
tests_ca$province[tests_ca$province == "NL"] <- "Newfoundland and Labrador"
tests_ca$province[tests_ca$province == "NWT"] <- "Northwest Territories"
tests_ca$province[tests_ca$province == "PEI"] <- "Prince Edward Island"
cases_ca <- cases_ca %>% left_join(tests_ca %>% select(province,date,testing))

# D - Merging 3 datasets at the daily level

merged <- cases_ca %>% select(date, cases, testing, sub_region_1 = province) %>% left_join( mob_ca %>%  filter(is.na(sub_region_2)) )
merged$province <- merged$sub_region_1
merged <- merged %>% drop_na(sub_region_1) %>% left_join(w %>% select("province","prov","date","mean_temp"),by=c("province","date"))
merged$week<-floor_date(merged$date,"week")

# E - Aggregate up to weeks 

merged<-merged %>% group_by(sub_region_1, week) %>% summarise(
  days = length(unique(date)),
  work = 100 + mean(workplaces_percent_change_from_baseline),
  retail = 100 + mean(retail_and_recreation_percent_change_from_baseline),
  nonres = 100 - 100*mean(residential_percent_change_from_baseline)/30,
  parks = 100 + mean(parks_percent_change_from_baseline),
  cases = sum(cases),
  testing = sum(testing),
  positivity = cases/testing,
  start = min(date),
  month = max(ymd("2020-03-01"),floor_date(date, "month")),
  temp = mean(mean_temp,na.rm=T)
)


merged. <- merged %>% group_by(sub_region_1) %>% mutate(
  
  cases_lag1=slider::slide_dbl(cases, mean, .before = 1, .after = -1),
  cases_lag2=slider::slide_dbl(cases, mean, .before = 2, .after = -2),
  cases_lag3=slider::slide_dbl(cases, mean, .before = 3, .after = -3),
  
  ratio = cases / cases_lag1,
  
  testing_lag1 = slider::slide_dbl(testing, mean, .before = 1, .after = -1),
  positivity_lag1 = cases_lag1/testing_lag1,
  positivity_change = positivity/positivity_lag1,
  
  work_lag1=slider::slide_dbl(work, mean, .before = 1, .after = -1),
  work_lag2=slider::slide_dbl(work, mean, .before = 2, .after = -2),
  work_lag3=slider::slide_dbl(work, mean, .before = 3, .after = -3),
  work_lag4=slider::slide_dbl(work, mean, .before = 4, .after = -4),
  work_lag = (1*work_lag1+1*work_lag2+1*work_lag3+0*work_lag4) / 3,

  retail_lag1=slider::slide_dbl(retail, mean, .before = 1, .after = -1),
  retail_lag2=slider::slide_dbl(retail, mean, .before = 2, .after = -2),
  retail_lag3=slider::slide_dbl(retail, mean, .before = 3, .after = -3),
  retail_lag4=slider::slide_dbl(retail, mean, .before = 4, .after = -4),
  retail_lag = (1*retail_lag1+1*retail_lag2+1*retail_lag3+0*retail_lag4) / 3,
  
  nonres_lag1=slider::slide_dbl(nonres, mean, .before = 1, .after = -1),
  nonres_lag2=slider::slide_dbl(nonres, mean, .before = 2, .after = -2),
  nonres_lag3=slider::slide_dbl(nonres, mean, .before = 3, .after = -3),
  nonres_lag4=slider::slide_dbl(nonres, mean, .before = 4, .after = -4),
  nonres_lag = (1*nonres_lag1+1*nonres_lag2+1*nonres_lag3+0*nonres_lag4) / 3,
  
  parks_lag1=slider::slide_dbl(parks, mean, .before = 1, .after = -1),
  parks_lag2=slider::slide_dbl(parks, mean, .before = 2, .after = -2),
  parks_lag3=slider::slide_dbl(parks, mean, .before = 3, .after = -3),
  parks_lag4=slider::slide_dbl(parks, mean, .before = 4, .after = -4),
  parks_lag = (1*parks_lag1+1*parks_lag2+1*parks_lag3+0*parks_lag4) / 3,
  
  mobility_lag = (nonres_lag),

  temp_lag1=slider::slide_dbl(temp, mean, .before = 1, .after = -1),
  temp_lag2=slider::slide_dbl(temp, mean, .before = 2, .after = -2),
  temp_lag3=slider::slide_dbl(temp, mean, .before = 3, .after = -3),
  
  temp_lag = (temp_lag1 + temp_lag2 + temp_lag3)/3,
  
  srw = paste(sub_region_1,week)
) %>% ungroup()

# Exclude small provinces and the last week (if it has < 7d of data)
merged..<-merged. %>% filter(week>ymd("2020-03-01"),cases_lag1 > 20, is.na(sub_region_1) == F, 
                             sub_region_1 %in% small_provinces == F, week<max(week) | (week==max(week) & days==7)
                             )
merged..$sub_region_1<-factor(merged..$sub_region_1)

##########################
##########################
##########################
#### Analysis and graphing
##########################
##########################
##########################

# Cohort descriptives

merged.. %>% group_by(sub_region_1) %>% summarise(
  weeks = length(week),
  cases = sum(cases)
)

merged.. %>% summarise(
  weeks = length(week),
  cases = sum(cases)
)

# 1 -- Mobility alone
# A = plot based on daily, B = plot based on weekly, C = plot of correlation (not used)
# 1A

mob_ca.<-mob_ca %>% drop_na(nonres,date) %>% filter(date >  (Sys.Date()-28*12.5),  sub_region_1 %in% c(small_provinces)==F) %>% 
  group_by(sub_region_1) %>% mutate (
  p_nonres_roll=rollmean(nonres, 7, na.pad=TRUE),
  p_parks_roll=rollmean(parks, 7, na.pad=TRUE),
                                    )
knots_daily<-round(length(unique(mob_ca.$date))/7) # a smooth in GAM with this many knots is very slow (~ 1 min each for now but will get slower)
m<-gam(nonres ~  s(as.numeric(date),factor(sub_region_1),k=knots_daily,bs="fs") + s(wday(date),k=7,bs="cc"), data=mob_ca., na.action="na.exclude"); mob_ca.$p_nonres <- predict.gam(m,exclude = "s(wday(date))")
# m.parks<-gam(parks ~  s(as.numeric(date),factor(sub_region_1),k=knots_daily,bs="fs") + s(wday(date),k=7,bs="cc"), data=mob_ca., na.action="na.exclude"); mob_ca.$p_parks <- predict.gam(m.parks,exclude = "s(wday(date))")

# 2 -- month 

# By month/prov
merged..$month <- floor_date(merged..$week,"month")

knots_overall <- round(length(unique(merged..$week))/8); knots_each<-max(3,knots_overall/2)
m<-gamm4(log(cases / cases_lag1) ~ mobility_lag + s(as.numeric(month),k=knots_overall) + s(sub_region_1,bs="re"), family="gaussian", data=merged.. ); m.mer<-m$mer; m<-m$gam
merged..$p_mobility_bymonth_prov<-exp(predict(m))
merged..$p_mobility_bymonth<-exp(predict(m, exclude=c("s(sub_region_1)")))
summary(m); summary(m.mer)

# By week

# Mobility by week/province
m<-gam(log(cases/cases_lag1) ~ mobility_lag , family="gaussian", method="REML", data=merged..)
summary(m); exp(get(m,2)*5) %>% round(.,2)

m<-gam(log(cases/cases_lag1) ~ mobility_lag + s(as.numeric(week),k=knots_overall) + s(as.numeric(week),by=sub_region_1,k=knots_each*1.5,m=1)  + s(sub_region_1,bs="re") + s(sub_region_1,mobility_lag,bs="re"), family="gaussian", method="REML", data=merged..)
merged..$mobility_r1 <- -1*(coef(m)[1] + apply(predict(m,type="terms",exclude=c("mobility_lag")),1,sum)) / coef(m)[names(coef(m)) == "mobility_lag"]
merged..$p_mobility_week_prov<-exp(predict(m))

# 3 -- Mobility gap

merged..
t0<-merged.. %>% 
  group_by(week) %>% summarise(
    n_weeks = n(),
    n_prov = length(unique(sub_region_1)),
    cases = sum(cases),
    positivity = miqr(positivity*100),
    cases_lag1 = sum(cases_lag1),
    ratio. = miqr(ratio),
    ratio_pos = miqr(positivity_change),
    ratio_cor = miqr(ratio*positivity_change),
    mobility_lag. = miqr0(mobility_lag),
    mobility_r1. = miqr0(mobility_r1),
    mobility_gap =  miqr0(mobility_lag - mobility_r1),
  ) %>% rename(var=week) %>% mutate(var=as.character(var))
t0

t1<-merged.. %>% 
  group_by(month) %>% summarise(
    n_weeks = n(),
    n_prov = length(unique(sub_region_1)),
    cases = sum(cases),
    positivity = miqr(positivity*100),
    ratio. = miqr(ratio),
    ratio_pos = miqr(positivity_change),
    ratio_cor = miqr(ratio*positivity_change),
    mobility_lag. = miqr0(mobility_lag),
    mobility_r1. = miqr0(mobility_r1),
    mobility_gap =  miqr0(mobility_lag - mobility_r1),
  ) %>% rename(var=month) %>% mutate(var=as.character(var))
t1

t2<-merged.. %>% 
  group_by(sub_region_1) %>% summarise(
    n_weeks = n(),
    n_prov = length(unique(sub_region_1)),
    cases = sum(cases),
    positivity = miqr(positivity*100),
    cases_lag1 = sum(cases_lag1),
    ratio. = miqr(ratio),
    ratio_pos = miqr(positivity_change),
    ratio_cor = miqr(ratio*positivity_change),
    mobility_lag. = miqr0(mobility_lag),
    mobility_r1. = miqr0(mobility_r1),
    mobility_gap =  miqr0(mobility_lag - mobility_r1),
  ) %>% rename(var=sub_region_1)

t2

t3 <- merged.. %>% summarise(
    n_weeks = n(),
    n_prov = length(unique(sub_region_1)),
    cases = sum(cases),
    positivity = miqr(positivity*100),
    cases_lag1 = sum(cases_lag1),
    ratio. = miqr(ratio),
    ratio_pos = miqr(positivity_change),
    ratio_cor = miqr(ratio*positivity_change),
    mobility_lag. = miqr0(mobility_lag),
    mobility_r1. = miqr0(mobility_r1),
    mobility_gap =  miqr0(mobility_lag - mobility_r1),
  ) 

t3



bind_rows(t1,t0 %>% filter(var %in% c(min(var),max(var))),t3,t2 ) 
bind_rows(t1,t0 %>% filter(var %in% c(min(var),max(var))),t3,t2 )  %>% write_csv(.,"t2_mobility_canada_v1.csv")

t3<-merged.. %>%
  group_by(sub_region_1,start) %>% summarise(
    n_weeks = n(),
    n_prov = length(unique(sub_region_1)),
    cases = sum(cases),
    cases_lag1 = sum(cases_lag1),
    ratio = exp(mean(log(ratio))),
    ratio_pos = miqr(positivity_change),
    nonres_lag = mean(nonres_lag),
    work_lag = mean(work_lag),
    retail_lag = mean(retail_lag), 
    mobility_lag = mean(mobility_lag),
    mobility_r1 = mean(mobility_r1),
    mobility_gap = mobility_lag - mobility_r1,
    sub_region_1. = as.character(sub_region_1),
    mobility_gap_f = cut(mobility_gap, c(-Inf,-5,5,Inf)),
    quarter = floor_date(start,"quarter"),
    month = floor_date(start,"month")
  ) 
t3

levels(t3$mobility_gap_f)<-c("Below (< -5)","At (+/- 5)","Above (>5)")

for(i in 1:length(regions_list)){
  
  reg <- regions_list[[i]]
  nreg <- length(reg)
  nam <- names(regions_list)[i]
  dir.create(paste(dir_out,"/",nam,"/",sep=""))
  
  p<-mob_ca.  %>% drop_na(p_nonres,nonres) %>% filter(date >  (Sys.Date()-28*4.5), sub_region_1 %in% reg) %>%
    ggplot(aes(x=date, col=sub_region_1)) +
    geom_line(aes(y=p_nonres),alpha=.8)  +
    geom_line(aes(y=p_nonres_roll), alpha=.25,size=.5) +
    # facet_wrap(facets=vars(sub_region_1)) +
    theme_bw()  +
    xlab(l_week) + ylab(l_nonres) + labs(color=l_province) +
    scale_x_date(date_breaks = "2 weeks",minor_breaks= "1 week", date_labels =  "%b %d \n %Y")
  p
  # ggsave( paste(nam,"/mobilityAlone_recent.png",sep="") ,p,"png",height=4,width=8)
  
  p<-mob_ca.  %>% drop_na(p_nonres,nonres) %>% filter(date >  (Sys.Date()-28*12.5), sub_region_1 %in% reg) %>%
    ggplot(aes(x=date, col=sub_region_1)) +
    geom_line(aes(y=p_nonres),alpha=.8)  +
    #  geom_line(aes(y=p_nonres + (nonres-p_nonres)/5), alpha=.2,size=.5) +
    geom_line(aes(y=p_nonres_roll), alpha=.25,size=.5) +
    theme_bw()  +
    xlab(l_week) + ylab(l_nonres) + labs(color=l_province) +
    scale_x_date(date_breaks = "4 weeks",minor_breaks= "2 week", date_labels =  "%b %d \n %Y")
  p
  ggsave( paste(nam,"/mobilityAlone_1yr.png",sep="") ,p,"png",height=4,width=8)
  
  p<-merged.. %>% filter(cases / cases_lag1 < 8, cases / cases_lag1>.125, sub_region_1 %in% reg) %>%
    ggplot(aes(y=cases / cases_lag1 , x= mobility_lag)) + 
    geom_point(aes(col=sub_region_1,size=cases_lag1),alpha=.5) + 
    facet_wrap(facets=vars(month %>% substr(1,7)),ncol=3) + 
    theme_bw() +   
    geom_line(aes(y=p_mobility_bymonth),alpha=0.5) + 
    geom_line(aes(y=p_mobility_bymonth_prov, col=sub_region_1),alpha=0.5) + 
    scale_y_continuous(trans="log2",breaks=c(.25,1,4)) +  
    geom_hline(yintercept=1,col="red",alpha=.25)+ 
    ylab(l_ratio) + 
    xlab(l_nonres) + 
    labs(color=l_province, size=l_cases) 
  p
  
  ggsave(paste(nam,"/mobility_byMonth.png",sep=""),p,"png",width=9,height= merged..$month %>% unique() %>% length() / 3 %>% ceiling()*2 )
  
  p1<- t3 %>%  filter(sub_region_1 %in% reg,start>=ymd("20200301")) %>% 
    ggplot(aes(x=start)) + geom_point(aes(y=mobility_r1,col="Threshold"),alpha=1,shape=95,size=3.5)  +
    geom_point(aes(y=mobility_lag,size=cases, col=mobility_gap_f), alpha=0.75) + 
    # geom_line(aes(y=mobility_r1 ,col=sub_region_1),alpha=.5)  +
    facet_wrap(vars(sub_region_1),ncol=1 ) + 
    theme_bw() +   ylab(l_nonres) + 
    xlab(l_month) + labs(color="Mobility",size=l_cases) +
   scale_x_date(date_breaks = "8 week",minor_breaks= "2 week", date_labels =  "%b %d \n %Y")
  
  p2<- t3 %>% filter(start>=ymd("20200301"),sub_region_1 %in% reg) %>% 
    ggplot(aes(x=mobility_gap,y=(cases/cases_lag1))) + 
    geom_point(aes(col=mobility_gap_f,size=cases), alpha=0.75) +  facet_wrap(vars(sub_region_1),ncol=1) + 
    theme_bw() +   ylab(l_ratio) + geom_smooth(method="lm",aes(col="Trend")) + scale_y_continuous(trans="log2",breaks=2^c(-2,0,2)) +
    xlab(l_gap) + labs(color="Mobility",size=l_cases) 

  p<-grid.arrange(p1,p2,ncol=2);
  ggsave(paste(nam,"/mobilityGap_both.png",sep=""),p,"png",height=max(5,1.5*nreg),width=10)
}
d1 <- t3 %>%  filter(start >  (Sys.Date()-28*12.5),sub_region_1 %in% reg) 
d2<-mob_ca.  %>% drop_na(p_nonres,nonres) %>% filter(date >  (Sys.Date()-28*1), sub_region_1 %in% reg)
d1
d2

p1<- ggplot(data=d1,aes(x=start)) + geom_point(aes(y=mobility_r1,shape="Threshold"),alpha=1,shape=95,size=3.5)  +
  geom_point(aes(y=mobility_lag,size=cases, col=mobility_gap_f), alpha=0.75) + 
  geom_line(data=d2, aes(x=date+10.5, y=p_nonres,col=sub_region_1),alpha=.8)  +
  geom_line(data=d2, aes(x=date+10.5, y=p_nonres_roll,col=sub_region_1),alpha=.3)  +
  
  facet_wrap(vars(sub_region_1),ncol=1 ) + 
  theme_bw() +   ylab(l_nonres) + 
  xlab(l_month) + labs(color="Mobility",shape="Threshold") +
  scale_x_date(date_breaks = "8 week",minor_breaks= "2 week", date_labels =  "%b %d \n %Y")
p1


# 

mob_ca.<-mob_ca %>% drop_na(nonres,date) %>% filter(date >  (Sys.Date()-28*3), sub_region_1 %in% c(small_provinces)==F)
knots_daily<-round(length(unique(mob_ca.$date))/3.5) # a smooth in GAM with this many knots is very slow (~ 1 min each for now but will get slower)
m<-gam(nonres ~ s(as.numeric(date), by=factor(sub_region_1),k=knots_daily), data=mob_ca., na.action="na.exclude"); mob_ca.$p_nonres <- predict(m)
# m<-gam(retail ~ s(as.numeric(date), by=factor(sub_region_1), k=knots_daily), data=mob_ca.,na.action="na.exclude"); mob_ca.$p_retail <- predict(m)
# m<-gam(work ~  s(as.numeric(date), by=factor(sub_region_1), k=knots_daily), data=mob_ca.,na.action="na.exclude"); mob_ca.$p_work <- predict(m)
# m<-gam(parks ~  s(as.numeric(date), by=factor(sub_region_1), k=knots_daily), data=mob_ca.,na.action="na.exclude"); mob_ca.$p_parks <- predict(m)

max(mob_ca.$date)

p1<- t3 %>% group_by(sub_region_1) %>% mutate(cases_tot = sum(cases)) %>% ungroup() %>% 
  filter(dense_rank(cases_tot) > 2 ,start>=ymd("20200301")) %>% 
  ggplot(aes(x=start)) + geom_point(aes(y=mobility_r1,col="Threshold"),alpha=1,shape=95,size=3.5)  +
  geom_point(aes(y=mobility_lag,size=cases,
                 col=mobility_gap_f), 
             alpha=0.75) + 
  # geom_line(aes(y=mobility_r1 ,col=sub_region_1),alpha=.5)  +
  facet_wrap(vars(sub_region_1),ncol=1 ) + 
  theme_bw() +   ylab(l_nonres) + 
  xlab(l_month) + labs(color="Mobility",size=l_cases) +
  scale_x_date(date_breaks = "1 month",date_labels =  "%b") +
  geom_line(data=mob_ca. %>% filter(sub_region_1 %in% c("Ontario","Quebec","British Columbia","Alberta","Manitoba")) ,aes(x=date+21/2,y=p_nonres), alpha=.5,size=1.1) 
  
p1
# ggsave("mobility_vs_need_largest_withCURRENT.png",p1,"png",height=8*1.33,width=4.5*1.33)


# 4 -- Calibration and validation of mobility gap

p<- t3 %>% ungroup() %>% 
  filter(start > ymd("20200115")) %>% 
  ggplot(aes(x=mobility_gap,y=ratio,col=sub_region_1,size=cases_lag1)) + geom_point(alpha=.5)  + 
  facet_grid(rows=vars(sub_region_1) ) + geom_smooth(method=lm,se=F) + 
  theme_bw() +  scale_y_continuous(trans="log2",breaks=c(.25,1,4)) +  
  geom_hline(yintercept=1,col="red",alpha=.25)+ 
  ylab(l_ratio) + 
  xlab(l_gap) + 
  labs(color=l_province, size=l_cases) 
p

# ggsave("calibration_all.png",p,"png",height=10,width=8)

p<- t3 %>%  mutate(cases_tot = sum(cases)) %>% ungroup() %>%
  filter(dense_rank(cases_tot) > 3 ,start>=ymd("20200125")) %>%
  ggplot(aes(x=mobility_gap,y=ratio,col=sub_region_1,size=cases_lag1)) + geom_point(alpha=.5)  + 
  facet_wrap(vars(sub_region_1) ) + geom_smooth(method=lm,se=F) + 
  theme_bw() +  scale_y_continuous(trans="log2",breaks=c(.25,1,4)) +  
  geom_hline(yintercept=1,col="red",alpha=.25)+ 
  ylab(l_ratio) + 
  xlab(l_gap) + 
  labs(color=l_province, size=l_cases) 
p

# ggsave("calibration_largest.png",p,"png",height=10,width=8)

M<- t3 %>% filter(start > ymd("20200115")) %>% group_by(sub_region_1) %>% do(model = lm(log2(ratio) ~ mobility_gap, data = .))
val1<-M$model %>% map_dfr(glance) %>% mutate(province = M$sub_region_1) %>% select(province, r.squared, p.value,nobs)

M<- t3 %>% filter(start > ymd("20200115")) %>% group_by(quarter) %>% do(model = lm(log2(ratio) ~ mobility_gap, data = .))
val2<-M$model %>% map_dfr(glance) %>% mutate(quarter = M$quarter) %>% select(quarter, r.squared, p.value,nobs)

M<- t3 %>% filter(start > ymd("20200115")) %>% group_by(month) %>% do(model = lm(log2(ratio) ~ mobility_gap, data = .))
val3<-M$model %>% map_dfr(glance) %>% mutate(month = M$month) %>% select(month, r.squared, p.value,nobs)

bind_rows(val1,val2,val3) 
bind_rows(val1,val2,val3) %>% write_csv(.,"appendix_validation.csv")


# val; val %>% write_csv(.,"appendix_validation.csv")