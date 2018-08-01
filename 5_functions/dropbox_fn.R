### get path to dropbox folder

path_get <- function(proj = proj, folder_data = folder_data){
  
  path_proj_data <- paste("C://Users//W10039893//Dropbox//data", 
                          proj, folder_data, sep = "//")
  
  return(path_proj_data)
  
}


### Load data Function

read_csv_fn <- function(proj = proj, folder_data = folder_data){
  
  
  path_proj_data <- paste("C:/Users/W10039893/Dropbox/data", 
                          proj, sep = "/")
  
  df <- read.csv(paste(path_proj_data, folder_data, sep = "/"))
  
  return(df)
  
}


### Save data function

write_csv_fn <- function(proj = proj, object = object, file = file, row.names = FALSE){
  
  path_proj_data <- paste("C:/Users/W10039893/Dropbox/data", 
                          proj, sep = "/")
  
  write.csv(object, file = paste(path_proj_data, file = file, sep = "/"),
            row.names = row.names)
  
}

### save .rda

save_fn <- function(proj = proj, object = object, folder_data = folder_data){
  
  path_proj_data <- paste("C:/Users/W10039893/Dropbox/data", 
                          proj, sep = "/")
  
  save(object, file = paste(path_proj_data, folder_data, sep = "/"))
  
}

### Load .rda ### FAILS ### Use getpath with base load

load_fn <- function(proj = proj, folder_data = folder_data){
  
  path_proj_data <- paste("C:/Users/W10039893/Dropbox/data", 
                          proj, sep = "/")
  
  return( load( file = paste(path_proj_data, folder_data, sep = "/") )
  )
  
}



