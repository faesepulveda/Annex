library(rio);
library(purrr);
library(tidyr);
library(tidyverse);
require(ggplot2);
require(reshape2);
library(plotly);
library(naniar);
library(dplyr);
library(lubridate);
library(knitr)


source("data_loading.r")

###########################

##########################

day <-mutate(annex, hour = hour(annex$V1), minute = minute(V1)) %>%
  filter((hour %in% seq(from = 7, to = 23, by= 1)))
night <-mutate(annex, hour = hour(annex$V1), minute = minute(V1)) %>%
  filter((hour %in% seq(from = 0, to = 6, by= 1)))


summer_m <- 12           
summer_d <- 21     
fall_m   <- 20
fall_d   <- 3
winter_m <- 6
winter_d <- 21
spring_m <- 9
spring_d <- 21

seasons <- sort(c("summer" = (summer_m*100 + summer_d),"fall" = (fall_m*100 + fall_d), 
                  "winter" = (winter_m*100 + winter_d), "spring" = (spring_m*100 + spring_d)))




by_season <- function(seasons, data, time){
  filter_season <- c()
  for (i in 1:length(seasons)){
    
    print(names(seasons[i]))
    print(seasons[[i]])
    season_name <- names(seasons[i])
    #crear carpeta season_name
    if (i != length(seasons)){
      
      season_start <- seasons[[i]]
      season_end <- seasons[[i + 1]] -1
      season_data <- mutate(data, dummy = month(data$V1)*100 + day(V1)) %>%
        filter(between(dummy,season_start, season_end))%>%select(-c(dummy))
      #guardar el filtro en carpeta season_name
      write.csv(season_data, file=paste0(season_name, "_",time, ".txt"))
      filter_season <- rbind(filter_season, season_data)
    }
    else{
      season_data <- setdiff(data, filter_season)
      #guardar el filtro en carpeta season_name
      write.csv(season_data, file=paste0(season_name, "_",time, ".txt"))
    }
  }
}


#season_day_nigth <- by_season(seasons = seasons, data = annex, time = "day&night")
#season_day <- by_season(seasons = seasons, data = day, time = "day")
#season_night <- by_season(seasons = seasons, data = night, time = "night")


################################################
#### statistical metrics by period level #######

files <- c("summer_day", "summer_night", "summer_day&night",
           "fall_day", "fall_night", "fall_day&night",
           "winter_day", "winter_night", "winter_day&night",
           "spring_day", "spring_night", "spring_day&night")




#########Descriptive Statistics File############
### maybe is needed to get this file for each period level???
statistical_metrics <- function(file_name){
  file = paste0(file_name, ".txt")
  data <- rio::import(file)
  data_ <- select(data, -c(V1))
  pollutants <- names(data_)
  
  descriptive <- c()
  j = 0
  for (i in pollutants){
    print(i)
    pol <- data[i]
    na <- sum(is.na(pol))
    pol <- na.omit(pol)
    min <- min(pol)
    max <- max(pol)
    qt <- quantile(pol, p = c(0.025,0.25,0.5,0.75,0.975),na.rm = T)
    stats_pol <- cbind(min,max,qt[[1]],qt[[2]],qt[[3]],qt[[4]],qt[[5]])
    colnames(stats_pol) <- c("min", "max", "Percentile 2.5", "Percentile 25", "Percentile 50", "Percentile 75", "Percentile 97.5")
    
    descriptive <- rbind(descriptive, stats_pol)
    
  }
  
  rownames(descriptive) <- pollutants
  descriptive <- data.frame(descriptive)
  descriptive <- rownames_to_column(descriptive)
  colnames(descriptive) <- c("pollutant","min", "max", "Percentile 2.5", "Percentile 25", "Percentile 50", "Percentile 75", "Percentile 97.5")
  
  write.csv(descriptive, file=paste0(file_name, "_descriptive.txt"))
}

stat_metrics <- function(){
  for (file_name in files){
    statistical_metrics(file_name)
  }
}





###### eCDF ##############

pollutant_eCDF <- function(file_name){
  file = paste0(file_name, ".txt")
  data <- rio::import(file)
  data_ <- select(data, -c(V1))
  pollutants <- names(data_)
  for (i in pollutants){
    
    pol <- data[i]
    cdf_pol <- ecdf(pol[,1])
    
    value_axis <- cbind(pol[,1])
    colnames(value_axis) <- "value"
    probability_axis <- cdf_pol(pol[,1])
    
    write.csv(cbind(value_axis, probability_axis), file=paste0(file_name,"_", names(pol), "_eCDF.txt"))
    
  }
}

pol_eCDF <- function(){
  for (file_name in files){
    pollutant_eCDF(file_name)
  }
  
}

plot_eCDF <- function(file){
  data <- rio::import(file) %>% select(-c(V1))
  plot(data)
}



######  Data resolution & Data quality indicators for the whole data set#####


data_quality <- function(){
  index = annex["V1"][[1]]
  dif <- diff(index)
  quantile(dif)
  median(dif)
  write.csv(dif, file=paste0(file_name, "_time_distribution.txt"))
  interval <- max(index) - min(index)
  message_1 <- paste0(" Analyzed time period is: ", 
                      round(interval, digits = 2)," ", units(interval))
  message_2 <- paste0("The data span from: ", min(index), " to: ", max(index))
  message_3 <- paste0("Typical sample interval is: ", median(dif), " ",units(median(dif)))
  message_4 <- paste0("Total number of samples: ", length(annex[[1]]))
  message_5 <- paste0("Total numbers of NA's: ", sum(colSums(is.na(annex))))
  msg_6 <- paste0("Population mean is: ", round(mean(na.omit(annex[[2]])), digits = 2))
  msg_7 <- paste0("Population standard deviation is: ", round(sd(na.omit(annex[[2]]))), digits = 2)
  cat(message_1, "\n",message_2, "\n", message_3, "\n",message_4, "\n",message_5, "\n",msg_6,  "\n",msg_7)
  capture.output(cat(message_1, "\n",message_2, "\n", message_3, "\n",message_4, "\n",message_5, "\n",msg_6,  "\n",msg_7), file = "quality.txt")
  
}

