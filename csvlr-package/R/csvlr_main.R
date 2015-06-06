###########################################################################################
# FILE: SHERYS_R_SCRIPTS_LIBRARY.R
# VERSION: 1.2
# AUTHOR: SHERY CHEONG 
# CREATED: 6/1/15
# LAST UPDATED: 6/5/15

########################################################################################################################

########################################################################################################################
#' @title merge_csv
#'
#' Takes multiple CSV files in a folder and merge them into a combined dataframe

#' @param directory: A string, file path containing the CSV files
#' @param replace: {TRUE/FALSE}, whether to replace NA with 0s. Default is FALSE
#' @param subdir: {TRUE/FALSE}, whether to include files in sub=directories. Default is FALSE
#' @return A single dataframe, containing the merged CSV data
#' @keywords csv
#' @export

merge_csv <- function(directory,replace_NA=FALSE,subdir=FALSE)
{ 
  # Validate inputs
  if(class(try(setwd(directory)))=="try-error")
  {
    stop("Invalid directory path.")
  }
  
  # Set working dir
  setwd(directory)
  
  # Get the files names
  files = list.files(pattern="*.csv",recursive=subdir) #whether to include CSVs in sub-folders
  
  # First apply read.csv, then rbind
  my_dataframe = do.call("rbind", lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
  
  # Replace NA with 0
  if (replace_NA == TRUE)
  {
    myfiles[is.na(myfiles)] <- 0
  }
  
  return (my_dataframe)
}
########################################################################################################################

########################################################################################################################
#' @title merge_txt
#'
#' Takes multiple TXT files in a folder and merge them into a combined dataframe

#' @param directory: A string, file path containing the CSV files
#' @param replace: {TRUE/FALSE}, whether to replace NA with 0s. Default is FALSE
#' @param subdir: {TRUE/FALSE}, whether to include files in sub=directories. Default is FALSE
#' @return A single dataframe, containing the merged TXT data
#' @keywords txt
#' @export

merge_txt <- function(directory,replace_NA=FALSE,subdir=FALSE)
{ 
  # Validate inputs
  if(class(try(setwd(directory)))=="try-error")
  {
    stop("Invalid directory path.")
  }
  
  # Set working dir
  setwd(directory)
  
  # Get the files names
  files = list.files(pattern="*.txt",recursive=subdir) #whether to include TXTs in sub-folders
  
  # First apply read.csv, then rbind
  my_dataframe = do.call("rbind", lapply(files, function(x) read.delim(x, stringsAsFactors = FALSE)))
  
  # Replace NA with 0
  if (replace_NA == TRUE)
  {
    my_dataframe[is.na(my_dataframe)] <- 0
  }
  
  return (my_dataframe)
}
########################################################################################################################

########################################################################################################################
#' @title csv_to_list
#'
#' Function Description:
#' Adds a folder of CSV files and creates a list containing each CSV as an element

#' @param directory: A string, file path containing the CSV files
#' @param subdir: {TRUE/FALSE}, whether to include files in sub=directories. Default is FALSE
#' @return A list, containing each CSV as an element
#' @keywords csv
#' @export

csv_to_list <- function(directory,subdir=FALSE)
{
  # Validate inputs
  if(class(try(setwd(directory)))=="try-error")
  {
    stop("Invalid directory path.")
  }
  
  # Set working dir
  setwd(directory)
  
  # Get the files names
  files = list.files(pattern="*.csv",recursive=subdir) #whether to include TXTs in sub-folders
  
  if(length(files)==0)
  {
    stop("No CSV files are found")
  }
  
  list_of_databases = list()
  for (i in files)
  {
    y = read.csv(i, stringsAsFactors = FALSE)
    list_of_databases[[length(list_of_databases)+1]] <- y
  }  
  return (list_of_databases)
}

########################################################################################################################

########################################################################################################################
#' @title bind_common
#'
#' Binds 2 dataframes based on common variables only. Standalone function which can be operated outside of bind_all.

#' @param dfr1: The first dataframe to bind
#' @param dfr2: The second dataframe to bind
#' @param print_dropped: {TRUE/FALSE}, whether to print the list of dropped variables. Default is FALSE.
#' @return A dataframe, containing the merged data
#' @keywords csv
#' @export


bind_common <- function(dfr1,dfr2,print_dropped=FALSE)
{
  # Validate inputs
  if(class(dfr1) !="data.frame" | class(dfr2) !="data.frame")
  {
    stop("Invalid dataframe.")
  }
  
  # Find the common variables between the 2 datadrames
  common_cols <- intersect(colnames(dfr1), colnames(dfr2))
  
  # Creates the merged dataframe by merging columns with common variables
  dfr_new <- rbind(subset(dfr1, select = common_cols), subset(dfr2, select = common_cols))
  
  # Creates vector containing the dropped variables
  dropped_cols = setdiff(union(colnames(dfr1),colnames(dfr2)),intersect(colnames(dfr1),colnames(dfr2)))
  
  # Flips the print dropped variables switch on/off
  if(print_dropped==TRUE)
  {
    # Prints the dropped variables separated by commons, if any.
    if(length(dropped_cols) != 0)
    {
      cat(length(unique(dropped_cols)),"Vars dropped:",paste(shQuote(unique(dropped_cols),type="cmd"),collapse=" , "),sep=" ")
    }
    else
    {
      print("No variables were dropped.")
    }
  }
  return (dfr_new)
}

########################################################################################################################

########################################################################################################################
#' @title bind_all
#'
#' Loops through all the dataframes in a list, and binds them all based on common variables.

#' @param df_list: List of dataframes(can be obtained from add_to_list function)
#' @param print_dropped: {TRUE/FALSE}, whether to print the list of dropped variables. Default is FALSE.
#' @return A dataframe, containing the merged data
#' @keywords csv
#' @export

bind_all <- function(db_list,print_dropped=FALSE)
{
  # Validate input
  if(df_list <2)
  {
    stop("Need 2 or more dataframes in the list.")
  }
  
  # Create the full database, with the base being the first dataframe
  db_full = db_list[[1]]
  
  # Create a vector to keep track of the dropped variables
  dropped_cols = vector()
  
  # Merge in rest of the dataframes by looping the bind_common function
  for (i in 2:length(db_list))
  {
    if (class(db_list[[i]]) != "data.frame")
    {
      stop("Invalid dataframe detected. Please check the class type of all elements in your list.")
    }
    db_full = bind_common(db_full,db_list[[i]])
    dropped_cols = c(dropped_cols,setdiff(union(colnames(db_full),colnames(db_list[[i]])),intersect(colnames(db_full),colnames(db_list[[i]]))))
  }
  
  # If any variables were dropped during the merge, it will print them
  if(print_dropped==TRUE)
  {
    if(length(dropped_cols) != 0)
    {
      cat(length(unique(dropped_cols)),"Vars dropped:",paste(shQuote(unique(dropped_cols),type="cmd"),collapse=" , "),sep=" ")
    }
    else
    {
      print("No variables were dropped.")
    }
  }
  return (db_full)
}
########################################################################################################################