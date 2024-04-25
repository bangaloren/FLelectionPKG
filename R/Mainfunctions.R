
#Factor Analysis of Florida Election Data


#This first section explores the importing and setup of the data.

clean_and_set_data<-function(){
  #data imports
  library("readxl")
  library(stats)
  library(ggplot2)

  full_set_path<-"fullVRreport.xlsx"
  sheetnames<-excel_sheets(full_set_path)
  tab_sets<- list()
  for(sheetname in sheetnames){
    data<-read_excel(full_set_path, sheet=sheetname)
    tab_sets[[sheetname]]<-data
  }
  #make each tab list into a df
  df_list <- list()
  # Loop through each year from 2017 to 2023
  for (year in 2017:2023) {
    var_name <- paste0("set_", year)
    assign(var_name, tab_sets[[as.character(year)]], envir = .GlobalEnv)
    df_list[[var_name]] <- get(var_name)
    year_data <- get(var_name)


  }
  #had a little check here
  for(year in 2017:2023){
    var_name <- get(paste0("set_", year))
    var_name$TotalVotes <- rowSums(var_name[, c("Florida Democratic Party", "Republican Party Of Florida")])
    assign(paste0("set_", year), var_name, envir = .GlobalEnv)

  }
  View(set_2018)
  #noticed that set_2022 has N/A rows. Drop all N/A rows
  rows_to_drop <- c(68, 69, 70)
  set_2022 <- set_2022[-rows_to_drop, ]

}


#Trend analysis code (per county)
#to predictions

county_trend_analysis_and_prediction <- function(county_name) {
  trends_and_predictions <- list()

  for (year in 2017:2023) {
    county_data <- subset(get(paste0("set_", year)), County == county_name)

    dem_percentage <- sum(county_data$"Florida Democratic Party") / sum(county_data$TotalVotes) * 100
    rep_percentage <- sum(county_data$"Republican Party Of Florida") / sum(county_data$TotalVotes) * 100

    trends_and_predictions[[as.character(year)]] <- c(Democratic_Percentage = dem_percentage,
                                                      Republican_Percentage = rep_percentage)
  }
  years <- as.numeric(names(trends_and_predictions))
  dem_percentages <- sapply(trends_and_predictions, "[[", "Democratic_Percentage")
  rep_percentages <- sapply(trends_and_predictions, "[[", "Republican_Percentage")
  dem_lm <- lm(dem_percentages ~ years)
  rep_lm <- lm(rep_percentages ~ years)

  dem_percentage_2024 <- predict(dem_lm, newdata = data.frame(years = 2024))
  rep_percentage_2024 <- predict(rep_lm, newdata = data.frame(years = 2024))
  trends_and_predictions[["2024"]] <- c(Democratic_Percentage = dem_percentage_2024,
                                        Republican_Percentage = rep_percentage_2024)
  plot_county_trend <- ggplot(data = data.frame(Year = years,
                                                Democratic_Percentage = dem_percentages,
                                                Republican_Percentage = rep_percentages),
                              aes(x = Year)) +
    geom_line(aes(y = Democratic_Percentage, color = "Democratic"), size = 1) +
    geom_line(aes(y = Republican_Percentage, color = "Republican"), size = 1) +
    scale_color_manual(values = c(Democratic = "blue", Republican = "red")) +
    labs(title = paste("Trend Analysis for", county_name),
         x = "Year",
         y = "Percentage",
         color = "Party") +
    theme_minimal()
  print(plot_county_trend)
  return(trends_and_predictions)
}


#Here, I am going to set up some summary stat code
#first 2 are pie chats as described:
#One function will be for individual county in a certain year stats
#another function will be total year stats


county_year_pie<-function(county_name, year){
  county_data <- subset(get(paste0("set_", year)), County == county_name)
  dem_votes <- sum(county_data$"Florida Democratic Party")
  rep_votes <- sum(county_data$"Republican Party Of Florida")
  summarydf<-data.frame(Party=c("Democratic","Republican"),Votes=c(dem_votes,rep_votes))
  pieYear <- ggplot(summarydf, aes(x = "", y = Votes, fill = Party)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(round(Votes/sum(Votes)*100), "%")),
              position = position_stack(vjust = 0.5)) +
    labs(title = paste("County Year Summary for", county_name, "in", year),
         fill = "Party") +
    scale_fill_manual(values = c(Democratic = "blue", Republican = "red")) +

    theme_void() +
    theme(legend.position = "bottom")

  print(pieYear)


}

total_year_summary_pie <- function(year) {
  county_data <-get(paste0("set_", year))

  dem_votes <- sum(year_data$"Florida Democratic Party")
  rep_votes <- sum(year_data$"Republican Party Of Florida")

  summary_df <- data.frame(Party = c("Democratic", "Republican"),
                           Votes = c(dem_votes, rep_votes))

  pie_chart <- ggplot(summary_df, aes(x = "", y = Votes, fill = Party)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(round(Votes/sum(Votes)*100), "%")),
              position = position_stack(vjust = 0.5)) +
    labs(title = paste("Total Year Summary for", year),
         fill = "Party") +
    scale_fill_manual(values = c(Democratic = "blue", Republican = "red")) +
    theme_void() +
    theme(legend.position = "bottom")

  print(pie_chart)
}


