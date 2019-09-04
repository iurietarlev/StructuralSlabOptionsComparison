
#rm(list=ls())

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#getwd()


#-------INTERPOLATION FUNCTION FOR EXCEL --------
interp_bh <- function(excel_file_path,x_i,y_i,method_i){
  
  original_df <- read_excel(excel_file_path)
  
  z <- original_df[2:ncol(original_df)] #remove column with the name
  original_df_matrix <- as.matrix(z) #transform from df into a matrix
  
  
  original_df <- as.matrix(original_df)
  x <- as.vector(as.numeric(colnames(original_df)))[2:length(colnames(original_df))]
  y <- as.vector(original_df[,1])
  
  if (isTRUE(x_i==max(x))){
    x_i <- x_i - 1e-10
  }
  
  z <- interp2(x, y, original_df_matrix, 
               x_i, y_i, method_i)
  return(z)
}

#-------INTERPOLATION FUNCTION FOR DEFAULT VALUES --------
interp_bh_dflt <- function(original_df,x_i,y_i,method_i){
  
  z <- original_df[2:ncol(original_df)] #remove column with the name
  original_df_matrix <- data.matrix(z) #transform from df into a matrix
  
  original_df <- as.matrix(original_df)
  x1 <- as.vector(as.numeric(colnames(original_df)))[2:length(colnames(original_df))]
  y1 <- as.numeric(as.vector(original_df[,1]))
  
  if (isTRUE(x_i==max(x1))){
    
    x_i <- x_i - 1e-10
  }
  
  z <- interp2(x = x1, y = y1, Z = original_df_matrix, 
               x_i, y_i, method_i)
  #print(original_df_matrix)
  return(z)
}

# select the table for a single parameter only
masking_rc_slab <- function(dfi, parameteri){
  mask = dfi$parameter==parameteri
  df_selection <- dfi[which(mask), ]
  df_selection <- df_selection[,2:length(df_selection[1,])]
  return(df_selection)
}


  
# getting RC parameters
get_depth <- function(dataframe, span_length, imposed_loading){
  depth_df <- masking_rc_slab(dataframe, "depth")
  depth <- interp_bh_dflt(depth_df, x_i = span_length, y_i = imposed_loading, method_i = "linear")
  return(depth)
  }
  
get_reinf_pm2 <- function(dataframe, span_length, imposed_loading){
  reinf_pm2_df <- masking_rc_slab(dataframe, "reinf_pm2")
  reinf_pm2 <- interp_bh_dflt(reinf_pm2_df, x_i = span_length, y_i = imposed_loading, method_i = "linear")
  return(reinf_pm2)
}

get_reinf_pm3 <- function(dataframe,  span_length, imposed_loading){
  reinf_pm3_df <- masking_rc_slab(dataframe, "reinf_pm3")
  reinf_pm3 <- interp_bh_dflt(reinf_pm3_df, x_i = span_length, y_i = imposed_loading, method_i = "linear")
  return(reinf_pm3)
}

# getting SPAN parameters
get_span <- function(dataframe,  depth_slab, imposed_loading){
  df <- masking_rc_slab(dataframe, "span")
  df_span <- interp_bh_dflt(df, x_i = imposed_loading, y_i = depth_slab , method_i = "linear")
  return(df_span)
}

# getting SPAN parameters
get_il <- function(dataframe,  sdll, ill){
  df <- masking_rc_slab(dataframe, "il")
  df_span <- interp_bh_dflt(df, x_i = sdll, y_i = ill  , method_i = "linear")
  return(df_span)
}


color.picker <- function(x,y,z){
  if(isTRUE(x <= y & x <= z ) ){return("green")}
  else if( isTRUE(x <= y & x >= z ) ){return("yellow")}
  else if( isTRUE(x <= z & x >= y ) ){return("yellow")}
  else {return("red")}
}


