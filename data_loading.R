library(rio)
#first set your working directory where you have your data
setwd("C:/Users/faese/Documents/Universidad/2022-01/Annex")

#add the filename
file_name <- "bedroom.txt"

#and load the data
annex  <- rio::import(file_name)