Raw files are datasets which were received in raw form (ie unwrangled and uncleaned datasets)
Interim files are datasets which are temporarily stored in a file prior to cleaning and merging as part of the data wrangling process
Processed files are datasets which have been wrangled and are used to run the regression models. 

Note: Due to SafeGraph's terms of service, census-level and POI-level visitation information could not be displayed. Only broadly aggregated data could be displayed. Please contact the main author (me) if you wish to obtain any relevant interim data that was used as part of the data wrangling or analysis process. 

Please note that SafeGraph provides one with the option of using "visitor_daytime_cbg" vs "visitor_nighttime_cbg". We opted to use "visitor_daytime_cbg" because the computation of “visitor_nighttime_cbg” was changed on May 2020 , which coincided very closely to the start of COVID-19. So any changes detected from COVID-19 could have very well been confounded by changes resulting from SafeGraphs methodological changes. For more info, please refer to SafeGraphs documentation https://docs.safegraph.com/docs/monthly-patterns#section-algorithms. 
