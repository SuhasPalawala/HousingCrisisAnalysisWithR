library(dplyr)
library(plotly)
library(ggplot2)
library(gganimate)

df <- read.csv("Final_Data.csv")
USIncome <- read.csv("USHIncome.csv")
USIncome <- mutate(USIncome,
                DATE = as.numeric(substr(DATE, 1, 4))
               )

#Scientific notation starts at 99 digits 
options(scipen = 99)

#Country home price by type and inflation adjustment
c_HPrice <- function(type, inflat){
  temp <- select(df, c("DATE", "property_type", "median_sale_price", "inflation_adjusted_price"))
  temp <- temp[temp$property_type == type,]
  temp <- group_by(temp, DATE)
  if(inflat){
    temp <- summarize(temp, average = mean(inflation_adjusted_price))
  }else{
    temp <- summarize(temp, average = mean(median_sale_price))
  }
  plot <- ggplot(temp, aes(DATE, average)) + geom_line() + 
    geom_point() + 
    scale_x_continuous(n.breaks = 10) + 
    scale_y_continuous(n.breaks = 10) + 
    labs(x = "Year", y = "Median Property Price (Dollars)")
  return(plot)
}


#Country income over time
c_Income <- ggplot(USIncome, aes(DATE, MEHOINUSA672N)) + 
  geom_line() + 
  geom_point() + 
  scale_x_continuous(n.breaks = 10) + 
  scale_y_continuous(n.breaks = 10) +
  labs(x = "Year", y = "Median Income (Dollars)")

#Country Homeless count plot
c_HLPlot <- function(stat){
  final <- c_HLTable(stat)
  colnames(final) <- c("Year", "count")
  plot <- ggplot(final, aes(Year, count)) + 
    geom_line() + 
    geom_point() + 
    scale_x_continuous(n.breaks = 10) + 
    scale_y_continuous(n.breaks = 10) +
    labs(x = "Year", y = "Number of Homeless Individuals")
  return(plot)
}

#Country Homeless count table
c_HLTable <- function(stat){
  if(stat == "Overall homelessness"){
    stat <- "Overall.Homeless.."
    range <- paste(stat, c(2012:2022), sep = "")
  }else if(stat == "Under 18"){
    stat <- "Overall.Homeless...Under.18.."
    range <- paste(stat, c(2014:2022), sep = "")
  }else if(stat == "18-24"){
    stat <- "Overall.Homeless...Age.18.to.24.."
    range <- paste(stat, c(2014:2022), sep = "")
  }else{
    stat <- "Overall.Homeless...Over.24.."
    range <- paste(stat, c(2014:2022), sep = "")
  }
  
  data <- data.frame(lapply(df[,range], function(x) as.numeric(gsub(",","", x))))
  final <- summarise_all(data, sum)
  final <- data.frame(lapply(final, function(x) as.integer(x/4.0)))
  final2 = data.frame(t(final))
  colnames(final2) <- c("Number of Homeless Individuals")
  final2 <- tibble::rownames_to_column(final2, "Year")
  final2 <- mutate(final2,
                   Year = as.integer(substr(Year, nchar(Year)-3, nchar(Year))))
  return(final2)
}


#State home price by type and inflation adjustment
s_HPrice <- function(state, type, inflat){
  state <- state.abb[match(state,state.name)]
  temp <- select(df, c("DATE", "state_code", "property_type", "median_sale_price", "inflation_adjusted_price"))
  temp <- temp[temp$property_type == type,]
  temp <- temp[temp$state_code == state,]
  if(inflat){
    temp <- mutate(temp, 
                    Price = inflation_adjusted_price)
  }else{
    temp <- mutate(temp, 
                   Price = median_sale_price)
  }
  plot <- ggplot(temp, aes(DATE, Price)) + 
    geom_line() + 
    geom_point() + 
    scale_x_continuous(n.breaks = 10) + 
    scale_y_continuous(n.breaks = 10) +
    labs(x = "Year", y = "Median Property Price (Dollars)")
  return(plot)
}

#State Household income
s_Income <- function(state){
  state <- state.abb[match(state,state.name)]
  temp <- df[df$state_code == state,]
  temp <- temp[c(TRUE, FALSE, FALSE, FALSE),]
  plot = ggplot(temp, aes(DATE, state.income)) + 
    geom_line() + 
    geom_point() + 
    scale_x_continuous(n.breaks = 10) + 
    scale_y_continuous(n.breaks = 10) +
    labs(x = "Year", y = "Median Income (Dollars)")
  return(plot)
}

#State Homeless count plot
s_HLPlot <- function(state, stat){
  final <- s_HLTable(state, stat)
  colnames(final) <- c("Year", "count")
  plot <- ggplot(final, aes(Year, count)) + 
    geom_line() + 
    geom_point() + 
    scale_x_continuous(n.breaks = 10) + 
    scale_y_continuous(n.breaks = 10) +
    labs(x = "Year", y = "Number of Homeless Individuals")
  return(plot)
}

#State Homeless count table
s_HLTable <- function(state, stat){
  state <- state.abb[match(state,state.name)]
  if(stat == "Overall homelessness"){
    stat <- "Overall.Homeless.."
    range <- paste(stat, c(2012:2022), sep = "")
  }else if(stat == "Under 18"){
    stat <- "Overall.Homeless...Under.18.."
    range <- paste(stat, c(2014:2022), sep = "")
  }else if(stat == "18-24"){
    stat <- "Overall.Homeless...Age.18.to.24.."
    range <- paste(stat, c(2014:2022), sep = "")
  }else{
    stat <- "Overall.Homeless...Over.24.."
    range <- paste(stat, c(2014:2022), sep = "")
  }
  temp <- df[df$state_code == state,]
  temp <- data.frame(temp[1,])
  
  data <- data.frame(lapply(temp[,range], function(x) as.integer(gsub(",","", x))))
  final = data.frame(t(data))
  colnames(final) <- c("Number of Homeless Individuals")
  final <- tibble::rownames_to_column(final, "Year")
  final <- mutate(final,
                   Year = as.integer(substr(Year, nchar(Year)-3, nchar(Year))))
  return(final)
}

tran_HPrice <- function(inflat, type){
  temp <- select(df, c("DATE", "state_code", "property_type", "median_sale_price", "inflation_adjusted_price", "state.income", "region"))
  temp <- temp[temp$property_type == type,]
  if(inflat){
    temp <- mutate(temp, 
                   Price = inflation_adjusted_price,
                   DATE = as.integer(DATE))
  }else{
    temp <- mutate(temp, 
                   Price = median_sale_price,
                   DATE = as.integer(DATE))
  }
  temp$region <- sapply(temp$region, function(x){
    if(is.na(x)){
      return("Other")
    }
    return(x)
  })
  plot <- ggplot(temp, aes(state.income, Price, color = region)) + 
    geom_point() + 
    labs(title = "Year: {frame_time}", x = "Median Income (Dollars)", y = "Median Property Price (Dollars)", color = "Region") +
    transition_time(DATE)
  return(plot)
}  

tran_HL <- function(inflat, type, stat, ...){
  if(stat == "Overall homelessness"){
    stat <- "Overall.Homeless.."
    range <- paste(stat, c(2012:2022), sep = "")
  }else if(stat == "Under 18"){
    stat <- "Overall.Homeless...Under.18.."
    range <- paste(stat, c(2014:2022), sep = "")
  }else if(stat == "18-24"){
    stat <- "Overall.Homeless...Age.18.to.24.."
    range <- paste(stat, c(2014:2022), sep = "")
  }else{
    stat <- "Overall.Homeless...Over.24.."
    range <- paste(stat, c(2014:2022), sep = "")
  }
  temp <- df[df$property_type == type,]
  if(stat == "Overall.Homeless.."){
    temp <- temp[as.integer(temp$DATE) > 2011,]
    adjust <- 2007
  }else{
    temp <- temp[as.integer(temp$DATE) > 2013,]
    adjust <- 2009
  }
  if(inflat){
    temp <- mutate(temp, 
                   Price = inflation_adjusted_price)
  }else{
    temp <- mutate(temp, 
                   Price = median_sale_price)
  }
  temp[,range] <- lapply(temp[,range], function(x) as.integer(gsub(",","", x)))
  temp <- temp[,c("DATE", "state", "region", "Price", range)]
  
  temp <- data.frame(t(apply(temp, 1, function(x){
    x[2023-adjust] <- x[as.integer(x[1])-adjust]
    return(x)
    })))

  temp <- select(temp, -range)
  colnames(temp)[5] <- "Homeless"
  
  temp$DATE <- as.integer(temp$DATE)
  temp$Homeless <- as.integer(temp$Homeless)
  temp$Price <- as.integer(temp$Price)

  temp$region <- sapply(temp$region, function(x){
    if(is.na(x)){
      return("Other")
    }
    return(x)
  })
  
  plot <- ggplot(temp, aes(Price, Homeless, color = region)) + 
    geom_point() +
    labs(title = "Year: {frame_time}", x = "Median Property Price (Dollars)", y = "Number of Homeless Individuals", color = "Region") +
    transition_time(DATE)
  return(plot)
}
