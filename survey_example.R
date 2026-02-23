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

final$ID <- as.factor(final$ID)

final$Time <- as.factor(final$Time)

final$GrossHHIncome <- as.factor(final$GrossHHIncome)

final$IncomeRatio <- as.factor(final$IncomeRatio)

final$Time_year <- as.factor(final$Time_year)

final$race <- as.factor(final$race)

final$birth_month <- as.factor(final$birth_month)

final_clean <- na.omit(final)

nlsy_design <- svydesign(
  id = ~ID,        
  weights = ~Weight, 
  data = final_clean
)

final_clean$ID <- as.numeric(final_clean$ID)

final_clean1 <- final_clean %>% filter(Time == 1)

final_clean2 <- final_clean %>% filter(Time == 2)

final_clean3 <- final_clean %>% filter(Time == 3)

final_clean4 <- final_clean %>% filter(Time == 4)

nlsy_design1 <- svydesign(
  id = ~ID,        
  weights = ~Weight, 
  data = final_clean1
)

nlsy_design2 <- svydesign(
  id = ~ID,        
  weights = ~Weight, 
  data = final_clean2
)

nlsy_design3 <- svydesign(
  id = ~ID,        
  weights = ~Weight, 
  data = final_clean3
)

nlsy_design4 <- svydesign(
  id = ~ID,        
  weights = ~Weight, 
  data = final_clean4
)

svymean(final_clean, nlsy_design)

svymean(final_clean1, nlsy_design1)

svymean(final_clean2, nlsy_design2)

svymean(final_clean3, nlsy_design3)

svymean(final_clean4, nlsy_design4)



