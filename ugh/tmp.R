setwd('~/Dropbox/Research/icc/data')

library(dplyr)
library(magrittr)
library(countrycode)

load('baseData.rda')

data %>% group_by(year) %>%
  summarize(
    stat=median(pts, na.rm=TRUE)
  )

list.files('Voeten/')
load('panel.rda')
# vdem = read.csv('~/Desktop/Country_Year_V-Dem_CSV_v7.1/V-Dem-DS-CY-v7.1.csv')

ids = names(vdem)[1:10]
voi = match(c('v2juhcind', 'v2juncind'), names(vdem))

vdemSub = vdem[vdem$year>=2000,c(1:10, voi)]
vdemSub$cname = countrycode(vdemSub$country_name, 'country.name', 'country.name')
vdemSub$cname[is.na(vdemSub$cname)] = 'SAO TOME AND PRINCIPE'
vdemSub$ccode = panel$ccode[match(vdemSub$cname, panel$cname)]
unique(vdemSub[is.na(vdemSub$ccode),c('cname','country_name')])
vdemSub = vdemSub[!is.na(vdemSub$ccode),]
vdemSub$ccodeYear = paste(vdemSub$ccode, vdemSub$year, sep='_')

tmp=vdemSub %>% group_by(ccodeYear) %>% summarize(val=mean(v2juhcind)) %>% data.frame()
tmp=vdemSub %>% group_by(year) %>% summarize(val=mean(v2juhcind)) %>% data.frame()

data$v2juhcind = vdemSub$v2juhcind[match(data$ccodeYear,vdemSub$ccodeYear)]
data$v2juncind = vdemSub$v2juncind[match(data$ccodeYear,vdemSub$ccodeYear)]

summary(data[,c('v2juhcind', 'v2juncind')])

x = setdiff(unique(data$cname), unique(vdemSub$cname))

test0 = data[which(data$cname %in% x),]
summary(test0[,names(test0)[4:9]])

test = data[which(!data$cname %in% x),]

summary(test[,c('v2juhcind', 'v2juncind')])

load('~/Desktop/PTS-2017.rdata')
pts = PTS_2017[PTS_2017$Year>2000,]
pts$pts = apply(pts[,c('PTS_A','PTS_H','PTS_S')], 1, function(x){
  ifelse(sum(is.na(x))==3, NA, max(x,na.rm=TRUE))
  })

pts$cname = countrycode(pts$Country, 'country.name','country.name')
pts$cname[pts$cname=='Czechoslovakia'] = 'CZECH REPUBLIC'
pts$cname[pts$cname=='Yugoslavia'] = 'SERBIA'
pts$cname[pts$cname=='Yugoslavia'] = 'SERBIA'

pts$ccode = panel$ccode[match(pts$cname, panel$cname)]
unique(pts[is.na(pts$ccode),c('Country','cname')])
pts = pts[!is.na(pts$ccode),]
pts$ccodeYear = paste(pts$ccode, pts$Year, sep='_')

data$pts = pts$pts[match(data$ccodeYear, pts$ccodeYear)]

summary(data$pts)