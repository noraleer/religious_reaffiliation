library(survey)
data(api)
mean(apipop$api00)
sum(apipop$enroll, na.rm=TRUE)
#stratified sample
dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
summary(dstrat)
svymean(~api00, dstrat)


weights <- read.table("customweight_nlsy97_6989725f19c40e6c583.dat")

weights <- weights %>% rename(ID = V1)
weights <- weights %>% rename(Weight = V2)

weights <- weights %>% mutate(Weight = Weight/100)

dat <- read_csv("FinalCleanedData_long.csv")

final <- left_join(dat, weights, by = "ID")