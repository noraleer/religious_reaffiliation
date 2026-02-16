library(foreign)
dat <- read.spss("FinalCleanedData.sav")
dat <- as.data.frame(dat)
olddat <- read.spss("FinalCleanedDataold.sav")
olddat <- as.data.frame(olddat)


names(dat)[1] <- "ID"
dat_long <- dat %>% select(ID, CatholicT1:NoAnswerResidenceT4) %>% 
  pivot_longer(
    cols = -ID,
    names_to = c(".value", "Time"),
    names_pattern = "([A-Za-z]+)(T[0-9]+)"
  ) %>%  mutate(Time = str_remove(Time, "T")) %>% 
  mutate(Time_year = case_when(Time == 1 ~ 1997,
                               Time == 2 ~ 2005,
                               Time == 3 ~ 2008,
                               Time == 4 ~ 2011))


#T1 being the original survey year (1997), T2 being 2005, T3 being 2008, and T4 being 2011.

dat_long <- dat_long %>% left_join(dat %>% select(ID,
                                                  sex = R0536300, 
                                                  race = R1482600, 
                                                  birth_month = R0536401, 
                                                  birth_year = R0536402, 
                                                  #YCIC = R000100, 
                                                  sample_type = R1235800), by = "ID")  %>% mutate(Age = Time_year - birth_year)

dat_long %>% View()

write.csv(dat_long , file = "FinalCleanedData_long.csv", row.names = FALSE)
#Person 8 has weird education order 54 too
 

# R0536300 (gender/sex)
# R1482600 (race/ethnicity)
# R0536401 (Rs Birthday - Month)
# R0536402 (Rs Birthday - Year)
# R000100 (Youth Case Identification Code)


# datlong <- dat %>% select("ID", contains("Catholic")) %>%
#   pivot_longer(cols = CatholicT1:CatholicT4,
#                names_to = "Time",
#                values_to = "Catholic") %>% 
#   mutate(Time = substring(Time,nchar(Time),nchar(Time))) %>% 
#   left_join(dat %>% select("ID", contains("Protestant")) %>%
#   pivot_longer(cols = ProtestantT1:ProtestantT4,
#                names_to = "Time",
#                values_to = "Protestant") %>% 
#   mutate(Time = substring(Time,nchar(Time),nchar(Time))),by = c("ID", "Time"))
# 







