library(pals)
library(dplyr)
mypal<-cols25(25)
#mypal<-glasbey(25)
mypal<-brewer.ylorrd(10)

# dat<-read.csv("C:/Users/Lucy/Dropbox (SPH Imperial College)/2019 nCov Lucy/IFR/Appendix_country_specific_suppression_measures.csv")
# ecdc<-read.csv("C:/Users/Lucy/Dropbox (SPH Imperial College)/IFR update/Data/daily_deaths_ECDC20200518.csv")
# pop<-read.csv("C:/Users/Lucy/Dropbox (SPH Imperial College)/IFR update/Data/WPP2019_TotalPopulationBySex.csv")
# usdeaths<-read.csv("C:/Users/Lucy/Dropbox (SPH Imperial College)/IFR update/Data/raw_data/covid_deaths_usafacts.csv")
# uspop<-read.csv("C:/Users/Lucy/Dropbox (SPH Imperial College)/IFR update/Data/raw_data/covid_county_population_usafacts.csv")

dat<-read.csv("Data/Appendix_country_specific_suppression_measures.csv")
ecdc<-read.csv("Data/daily_deaths_ECDC20200518.csv")
pop<-read.csv("Data/WPP2019_TotalPopulationBySex.csv")
usdeaths<-read.csv("Data/raw_data/covid_deaths_usafacts.csv")
uspop<-read.csv("Data/raw_data/covid_county_population_usafacts.csv")


dat$date_lockdown<-as.Date(dat$Date.of.suppression,"%d/%m/%Y")
# hist(dat$date_lockdown, breaks=100)
# table(dat$Deaths.at.suppression)

ecdc$Country<-gsub("_"," ", ecdc$countriesAndTerritories)
ecdc$Country[which(ecdc$Country=="United States of America")]<-"United States"
ecdc$dateRep<-as.Date(ecdc$dateRep,"%d/%m/%Y")

pop<-pop[which(pop$MidPeriod=="2019.5"),]
names(pop)[which(names(pop)=="Location")]<-"Country"
pop$Country[which(pop$Country=="United States of America")]<-"United States"

# ### countries with >10 deaths at lockdown
# x<-subset(dat, Deaths.at.suppression>=10)
# inds<-which(ecdc$country %in% x$Country)
# ecdc<-ecdc[inds,]
# table(ecdc$country)
# 
# inds<-which(ecdc$country=="France")
# plot(ecdc)

temp<-subset(dat, select=c(Country,date_lockdown))
ecdc<-full_join(ecdc,temp,by="Country")
ecdc<-subset(ecdc,!is.na(date_lockdown))
countries_ecdc<-unique(ecdc$Country)

lag<-0
ecdc0<-ecdc
for(i in 1:length(countries_ecdc)) {
  inds<-which(ecdc0$Country==countries_ecdc[i] & (ecdc0$dateRep<(ecdc0$date_lockdown+lag)))
  ecdc0$deaths[inds]<-NA
}
temp<-tapply(ecdc0$deaths,ecdc0$Country,sum,na.rm=T)
ecdc_deaths_after<-data.frame(Country=row.names(temp), deaths_after_suppression=as.numeric(temp))

ecdc0<-ecdc
for(i in 1:length(countries_ecdc)) {
  inds<-which(ecdc0$Country==countries_ecdc[i] & (ecdc0$dateRep>=(ecdc0$date_lockdown+lag)))
  ecdc0$deaths[inds]<-NA
}
temp<-tapply(ecdc0$deaths,ecdc0$Country,sum,na.rm=T)
ecdc_deaths_before<-data.frame(Country=row.names(temp), deaths_before_suppression=as.numeric(temp))

## peak daily deaths
temp<-tapply(ecdc$deaths,ecdc$Country,max,na.rm=T)
peak_daily<-data.frame(Country=row.names(temp), peak_daily_deaths=as.numeric(temp))
ecdc<-full_join(ecdc, peak_daily,by="Country")
ecdc$date_peak<-NA
for(i in 1:length(countries_ecdc)) {
  inds<-which(ecdc$Country==countries_ecdc[i] & (ecdc$deaths>=(ecdc$peak_daily_deaths)))
  ecdc$date_peak[inds]<-ecdc$dateRep[inds]
}
temp<-tapply(ecdc$date_peak,ecdc$Country,mean,na.rm=T)
ecdc_peak_date<-data.frame(Country=row.names(temp), date_peak_deaths=as.numeric(temp))


## total deaths
temp<-tapply(ecdc$deaths,ecdc$Country,sum,na.rm=T)
total_cumu<-data.frame(Country=row.names(temp), deaths_total=as.numeric(temp))
ecdc<-full_join(ecdc, total_cumu,by="Country")


dat<-full_join(dat, ecdc_deaths_after,by="Country")
dat<-full_join(dat, ecdc_deaths_before,by="Country")
dat<-full_join(dat, peak_daily,by="Country")
dat<-full_join(dat, ecdc_peak_date,by="Country")
dat<-full_join(dat, total_cumu,by="Country")
dat<-left_join(dat, pop,by="Country")
ecdc<-left_join(ecdc, pop,by="Country")

inds<-which(dat$deaths_total>=10)
dat<-dat[inds,]


ecdc$cumu_deaths<-NA
ecdc<-ecdc[order(ecdc$Country,ecdc$dateRep),]
for(i in 1:length(countries_ecdc)) {
  inds<-which(ecdc$Country==countries_ecdc[i])
  ecdc$cumu_deaths[inds]<-cumsum(ecdc$deaths[inds])
}
ecdc$cumu_deaths_per_1000000<-1000*ecdc$cumu_deaths/ecdc$PopTotal

dat$deaths_after_suppression_perm<-dat$deaths_after_suppression/(dat$PopTotal/1000)
dat$deaths_before_suppression_perm<-dat$deaths_before_suppression/(dat$PopTotal/1000)
dat$peak_daily_deaths_perm<-dat$peak_daily_deaths/(dat$PopTotal/1000)
dat$deaths_total_perm<-dat$deaths_total/(dat$PopTotal/1000)


### USA
usdeaths<-filter(usdeaths,State=="NY")
usdeaths$tot<-rowSums(select(usdeaths,X1.22.20:X5.26.20))
#usdeaths2<-select(usdeaths,County.Name,State,tot)
uspop<-filter(uspop,State=="NY")
usmerge<-full_join(uspop,usdeaths,by="County.Name")
usmerge$deathspercap<-usmerge$tot/usmerge$population
usmerge<-filter(usmerge,deathspercap>0.05 & !is.infinite(deathspercap))
usmerge<-filter(usmerge, County.Name %in% c("Queens County","Bronx County", "New York County", "Kings County", 
                                            "Richmond County"))
nyc_deaths<-usmerge %>%
  select(X1.22.20:X5.26.20)
nyc_deaths<-colSums(nyc_deaths)

##### absolute
plot(dat$deaths_before_suppression,dat$deaths_after_suppression , log="xy", xlab="deaths before lockdown",
     ylab="deaths after lockdown",pch=19)
inds<-which(dat$deaths_before_suppression>1)
text(dat$deaths_before_suppression[inds],dat$deaths_after_suppression[inds],dat$Country[inds],adj=c(1,1),cex=0.8)

plot(dat$deaths_before_suppression,dat$peak_daily_deaths , log="xy", xlab="deaths before lockdown",
     ylab="peak daily deaths",pch=19)

plot(dat$deaths_before_suppression,dat$deaths_total , log="xy", xlab="deaths before lockdown",
     ylab="overall cumulative deaths",pch=19)

dat$date_peak_deaths<-as.Date(dat$date_peak_deaths,origin="1970-01-01")
plot(dat$date_lockdown,dat$date_peak,xlab="date lockdown",
     ylab="date peak daily deaths",pch=19)


### per capita
options(scipen=5)
plot(dat$deaths_before_suppression_perm,dat$deaths_after_suppression_perm , log="xy", xlab="deaths per million before lockdown",
     ylab="deaths per million after lockdown",pch=19)
inds<-which(dat$deaths_before_suppression>1)
text(dat$deaths_before_suppression_perm[inds],dat$deaths_after_suppression_perm[inds],dat$Country[inds],adj=c(1,1),cex=0.8)

plot(dat$deaths_before_suppression_perm,dat$peak_daily_deaths_perm , log="xy", xlab="deaths per million before lockdown",
     ylab="peak daily deaths per million",pch=19)

plot(dat$deaths_before_suppression_perm,dat$deaths_total_perm , log="xy", xlab="deaths per million before lockdown",
     ylab="overall cumulative deaths per million",pch=19)

dat$date_peak_deaths<-as.Date(dat$date_peak_deaths,origin="1970-01-01")
plot(dat$date_lockdown,dat$date_peak,xlab="date lockdown",
     ylab="date peak daily deaths",pch=19)


ecdc<-subset(ecdc,deaths_total>=100)
#long_palette<-rep(palette,5)
brks<-seq(min(as.numeric(ecdc$date_lockdown-1)), max(as.numeric(ecdc$date_lockdown)+1),by=7)
ecdc$date_lock_cat<-cut(as.numeric(ecdc$date_lockdown),breaks=brks,  )
levels(ecdc$date_lock_cat)<-c("dodgerblue","orange" ,"red","black")
table(ecdc$date_lock_cat,useNA="a")
par(mar=c(5,4,4,2))
plot(ecdc$dateRep,ecdc$cumu_deaths_per_1000000,col="white",ylab="cumulative deaths per million", ylim=c(0,2500),
     xlab="date",xlim=c(min(ecdc$dateRep),(max(ecdc$dateRep)+21)))
for(i in 1:length(countries_ecdc)) {
  inds<-which(ecdc$Country==countries_ecdc[i])
  lines(ecdc$dateRep[inds],ecdc$cumu_deaths_per_1000000[inds],col=as.character(ecdc$date_lock_cat[inds[1]]))
}
lines(as.Date.character("2020-01-22"):as.Date.character("2020-05-26"),as.numeric(nyc_deaths/(sum(usmerge$population)/1000000)),col="purple")
temp<-tapply(ecdc$cumu_deaths_per_1000000,ecdc$Country,max)
temp<-temp[which(as.numeric(temp)>400 & row.names(temp)!="Italy")]
text(rep(max(ecdc$dateRep),length(temp)),as.numeric(temp),row.names(temp),pos=4,cex=0.8)
text(max(ecdc$dateRep),2466,"New York City",pos=4,cex=0.8)
