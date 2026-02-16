library(survey)
data(api)
mean(apipop$api00)
sum(apipop$enroll, na.rm=TRUE)
#stratified sample
dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
summary(dstrat)
svymean(~api00, dstrat)