source("annex_draft.r")

#filter data by season of the year & time of the day
by_season(seasons = seasons, data = annex, time = "day&night")
by_season(seasons = seasons, data = day, time = "day")
by_season(seasons = seasons, data = night, time = "night")

#get statistical metrics for each file generated in the preovious step
# and saved in .txt
stat_metrics()


#generate eCDF for each level segregation

pol_eCDF()

# suposse you want to check eCDF for Co2 levels in summer 
# in day & nigth time day you just need to do:


plot_eCDF("summer_day&night_co2_eCDF.txt")

# in general the syntax is:
# season_daytime_pollutant_eCDF.txt


# Finally you can get a general report in the quality of the data
# with data_quality function, this wil print and save a report for your data.

data_quality()
