#' Clean and Set all Data
#'
#' @return nothing. just sets the data and the sata is broken up into years. set_2017 up to set_2023
#' @export
#'
#' @examples clean_and_set_data()
clean_and_set_data<-function(user_dir){

  #data imports
  library("readxl")
  library(stats)
  library(ggplot2)

  full_set_path<-user_dir
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
  #noticed that set_2022 has N/A rows. Drop all N/A rows
  rows_to_drop <- c(68, 69, 70)
  set_2022 <- set_2022[-rows_to_drop, ]

}
