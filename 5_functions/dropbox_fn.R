### get path to dropbox folder

path_get <- function( file = file, proj = proj){
  
  path_proj_data <- paste("C://Users//W10039893//Dropbox//data", 
                          proj, file, sep = "//")
  
  return(path_proj_data)
  
}


### Load data Function

read_csv_fn <- function( folder_data = folder_data, proj = proj, na.strings = "NA"){
  
  
  path_proj_data <- paste("C:/Users/W10039893/Dropbox/data", 
                          proj, sep = "/")
  
  df <- read.csv(paste(path_proj_data, folder_data, sep = "/"), na.strings = na.strings)
  
  return(df)
  
}


### Save data function

write_csv_fn <- function( object = object, file = file, proj = proj, row.names = FALSE){
  
  path_proj_data <- paste("C:/Users/W10039893/Dropbox/data", 
                          proj, sep = "/")
  
  write.csv(object, file = paste(path_proj_data, file = file, sep = "/"),
            row.names = row.names)
  
}

### save .rda

save_fn <- function( object = object, file = file, proj = proj){
  
  path_proj_data <- paste("C:/Users/W10039893/Dropbox/data", 
                          proj, sep = "/")
  
  save(object, file = paste(path_proj_data, file, sep = "/"))
  
}

### Load .rda ### FAILS ### Use getpath with base load

load_fn <- function( file = file, proj = proj){
  
  path_proj_data <- paste("C:/Users/W10039893/Dropbox/data", 
                          proj, sep = "/")
  
  return( load( file = paste(path_proj_data, file, sep = "/") )
  )
  
}



