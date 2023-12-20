library(stringr)
library(dplyr)
library(tidyverse)

CPI <- read.csv("Consumer Price Index 1913-2023 Edited.csv")
HHI <- read.csv("Median household income US and by state 1984-2022 - Edited.csv")
SPH <- read.csv("Sale price of homes by state.csv")
HLEChange <- read.csv("Change-Table 1.csv")
HLE2012 <- read.csv("2012-Table 1.csv")
HLE2013 <- read.csv("2013-Table 1.csv")
HLE2014 <- read.csv("2014-Table 1.csv")
HLE2015 <- read.csv("2015-Table 1.csv")
HLE2016 <- read.csv("2016-Table 1.csv")
HLE2017 <- read.csv("2017-Table 1.csv")
HLE2018 <- read.csv("2018-Table 1.csv")
HLE2019 <- read.csv("2019-Table 1.csv")
HLE2020 <- read.csv("2020-Table 1.csv")
HLE2021 <- read.csv("2021-Table 1.csv")
HLE2022 <- read.csv("2022-Table 1.csv")

subsetHL <- function(H, yr){
  return(select(H, one_of(c("State", paste(c("Overall.Homeless..", "Overall.Homeless...Under.18..", "Overall.Homeless...Age.18.to.24..", "Overall.Homeless...Over.24.."), yr, sep = "")))))
}

HLE2012 <- subsetHL(HLE2012, "2012")
HLE2013 <- subsetHL(HLE2013, "2013")
HLE2014 <- subsetHL(HLE2014, "2014")
HLE2015 <- subsetHL(HLE2015, "2015")
HLE2016 <- subsetHL(HLE2016, "2016")
HLE2017 <- subsetHL(HLE2017, "2017")
HLE2018 <- subsetHL(HLE2018, "2018")
HLE2019 <- subsetHL(HLE2019, "2019")
HLE2020 <- subsetHL(HLE2020, "2020")
HLE2021 <- subsetHL(HLE2021, "2021")
HLE2022 <- subsetHL(HLE2022, "2022")

HLE <- left_join(HLE2012, HLE2013, by = "State")
HLE <- left_join(HLE, HLE2014, by = "State")
HLE <- left_join(HLE, HLE2015, by = "State")
HLE <- left_join(HLE, HLE2016, by = "State")
HLE <- left_join(HLE, HLE2017, by = "State")
HLE <- left_join(HLE, HLE2018, by = "State")
HLE <- left_join(HLE, HLE2019, by = "State")
HLE <- left_join(HLE, HLE2020, by = "State")
HLE <- left_join(HLE, HLE2021, by = "State")
HLE <- left_join(HLE, HLE2022, by = "State")

SPH <- SPH[order(SPH$period_begin),]
SPH <- subset(SPH, select = c(period_begin,
                              state,
                              state_code,
                              property_type,
                              median_sale_price))
SPH <- SPH[str_detect(SPH$period_begin,"01-01") & !str_detect(SPH$period_begin,"2023"),]
SPH <- SPH[order(SPH$property_type),]
SPH <- SPH[order(SPH$state_code),]
SPH <- SPH[order(SPH$period_begin),]
SPH <- rename(
  SPH,
  DATE = period_begin,
)

HLEChange <- subset(HLEChange, select = c("State", paste(c("Change.in.Total.Homelessness.."), c(2021:2012), c(".2022"), sep = "")))

HLEChange <- rename(
  HLEChange,
  state_code = State,
)

HLE <- rename(
  HLE,
  state_code = State,
)

new_df <- left_join(HLEChange, SPH, by = c("state_code"))
new_df <- mutate(new_df, 
                 DATE = substring(DATE, 1, 4)
                 )


new_df <- left_join(new_df, HLE, by = c("state_code"))

CPI <- subset(CPI, select = c(Year,
                              Jan.CPI))
CPI <- CPI[str_detect(CPI$Year,paste(c(2012:2022), collapse = "|")),]
CPI <- rename(
  CPI,
  DATE = Year,
)
CPI <- mutate(
  CPI,
  DATE = substring(DATE, 1, 4)
)

new_df <- left_join(CPI, new_df, by = c("DATE"))

new_df$state.income <- apply(X = new_df[,c(1, 3)], MARGIN = 1, FUN = function(data){
                                 HHI[substring(HHI$DATE, 1, 4) == data[1],][[paste(data[2], ".Household.Income", sep = "")]]})

increasedOrDecreased <- function(number) {
  number_value <- as.numeric(sub("%","",number))
  if(number_value > 0) {
    return("increased")
  } else if(number_value < 0) {
    return("decreased")
  } else {
    return("stayed the same")
  }
}

new_df <- mutate(
  new_df,
  homelessness_final_change = mapply(increasedOrDecreased,new_df$Change.in.Total.Homelessness..2012.2022)
)

newPrice <- function(oldCost, oldCPI, newCPI) {
  return(round(oldCost * (newCPI / oldCPI)))
}

new_df <- mutate(
  new_df,
  inflation_adjusted_price = mapply(newPrice, new_df$median_sale_price, new_df$Jan.CPI, 281.148)
)

new_df <- mutate(
  new_df,
  numeric_homelessness_change = as.numeric(sub("%","",new_df$Change.in.Total.Homelessness..2012.2022))
)

new_df$region <- setNames(state.region, state.abb)[new_df$state_code]

summarized_data <- t(sapply(new_df[, c("numeric_homelessness_change","median_sale_price","state.income","inflation_adjusted_price")], summary))

new_df <- select(new_df, -numeric_homelessness_change)

#View(summarized_data)

write.csv(new_df, "Final_Data.csv", row.names =TRUE)
