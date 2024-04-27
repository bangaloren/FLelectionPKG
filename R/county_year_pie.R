#' County Year Summary
#'
#' @param county_name name of the county in all caps and quotes
#' @param year year in format YYYY
#'
#' @return prints the pie chart for the year summary of the county.
#' @export
#'
#' @examples county_year_pie("ALACHUA", 2019)
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
