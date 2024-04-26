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
