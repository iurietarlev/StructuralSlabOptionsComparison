
#rm(list=ls())

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd()





# Function for different output depending on the selected radio_button
# it shows csv input when csv button is selected and mean and sd input when other type of data is selected
input_type_selector_excel_dflt <- function(input_choice, excel, dflt, rc_table_tag,
                                           hollowcore_table_tag, clt_table_tag)
  {
  if (input_choice == excel) {
    list(
      fileInput(inputId = rc_table_tag, label = "RC Slab - load spab table", accept = ".xlsx"),
      fileInput(inputId = hollowcore_table_tag, label = "Hollowcore - looad span table", accept = ".xlsx"),
      fileInput(inputId = clt_table_tag, label = "CLT - load span table", accept = ".xlsx")
      )
    
  }
  else {
    
  }
  }


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()




#install.packages("readxl")
#install.packages("pracma")



#-------INTERPOLATION FUNCTION--------
interp_bh <- function(excel_file_path,x_i,y_i,method_i){
  
  original_df <- read_excel(excel_file_path)
  
  z <- original_df[2:ncol(original_df)] #remove column with the name
  original_df_matrix <- as.matrix(z) #transform from df into a matrix
  
  
  original_df <- as.matrix(original_df)
  x <- as.vector(as.numeric(colnames(original_df)))[2:length(colnames(original_df))]
  y <- as.vector(original_df[,1])
  z <- interp2(x, y, original_df_matrix, 
               x_i, y_i, method_i)
  return(z)
}

answer <- interp_bh(excel_file_path = "./single-span-one-way-slab.xlsx",x_i = 9, y_i = 6.5, method_i = "linear")
answer

    
excel_file_path = "./single-span-one-way-slab.xlsx"
x_i = 9
y_i = 10
method_i = "linear"
original_df <- read_excel(excel_file_path)
original_df


z <- original_df[2:ncol(original_df)] #remove column with the name
z
original_df_matrix <- as.matrix(z) #transform from df into a matrix
original_df_matrix


original_df <- as.matrix(original_df)
x <- as.vector(as.numeric(colnames(original_df)))[2:length(colnames(original_df))]
x
y <- as.vector(original_df[,1])
y
#x_i <- 4.67
#y_i <- 400
z <- interp2(x, y, original_df_matrix, 
             x_i, y_i, method_i)
z  
  





