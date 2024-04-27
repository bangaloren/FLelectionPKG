#' County Trends and Predictions
#'
#' @param county_name enter the FL county name you want to test in all caps in quotes
#'
#' @return all trends for each year and the prediction for the upcoming year based on the county.
#' @export
#'
#' @examples county_trend_analysis_and_prediction("ALACHUA")
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
