
library(tibble) # to add column to dataframe
# function to create a dataframe containing load span information
library("readxl") #library for reading excel files
library("pracma") #library for bi-linear interpolation fucntion



load_span_table_df <- function(location, parameteri){
  original_df <- read_excel(location)
  original_df <- as.data.frame(original_df)
  original_df <- add_column(original_df, parameter = parameteri, .before = 1)
}


#creating a table for RC one-way single-span slab

rc_depth_table <- load_span_table_df("./single-span-one-way-slab.xlsx", "depth")
rc_depth_table
rc_reinf_pm2_table <- load_span_table_df("./single-span-one-way-slab reinforcement_pm2.xlsx", "reinf_pm2")
rc_reinf_pm2_table
rc_reinf_pm3_table <- load_span_table_df("./single-span-one-way-slab reinforcement_pm3.xlsx", "reinf_pm3")

rc_tables <- rbind(rc_depth_table,rc_reinf_pm2_table,rc_reinf_pm3_table)
rc_tables

saveRDS(rc_tables, file = "rc_single_span_one_way_slab_tables.rds")
readRDS(file = "rc_single_span_one_way_slab_tables.rds" )

#creating a table for RC one-way multiple-span slab

rc_depth_table <- load_span_table_df("./multiple-span-one-way-slab.xlsx", "depth")
rc_depth_table
rc_reinf_pm2_table <- load_span_table_df("./multiple-span-one-way-slab reinforcement_pm2.xlsx", "reinf_pm2")
rc_reinf_pm2_table
rc_reinf_pm3_table <- load_span_table_df("./multiple-span-one-way-slab reinforcement_pm3.xlsx", "reinf_pm3")

rc_tables <- rbind(rc_depth_table,rc_reinf_pm2_table,rc_reinf_pm3_table)
rc_tables

saveRDS(rc_tables, file = "rc_multiple_span_one_way_slab_tables.rds")
readRDS(file = "rc_multiple_span_one_way_slab_tables.rds" )

#creating a table for RC multiple span flat slab

rc_depth_table <- load_span_table_df("./multiple-span-flat-slab.xlsx", "depth")
rc_depth_table
rc_reinf_pm2_table <- load_span_table_df("./multiple-span-flat-slab reinforcement_pm2.xlsx", "reinf_pm2")
rc_reinf_pm2_table
rc_reinf_pm3_table <- load_span_table_df("./multiple-span-flat-slab reinforcement_pm3.xlsx", "reinf_pm3")

rc_tables <- rbind(rc_depth_table,rc_reinf_pm2_table,rc_reinf_pm3_table)
rc_tables

saveRDS(rc_tables, file = "rc_multiple_span_flat_slab_tables.rds")
readRDS(file = "rc_multiple_span_flat_slab_tables.rds" )





#creating a table for HOLLOWCORE

table <- load_span_table_df("./hollowcore-load-span-table.xlsx", "span")
table

saveRDS(table, file = "hollowcore_table.rds")
readRDS(file = "hollowcore_table.rds" )


#creating a table for CLT
table <- load_span_table_df("./CLT-load-span-table.xlsx", "span")
table

saveRDS(table, file = "clt_table.rds")
readRDS(file = "clt_table.rds" )


#creating a table for SDl + IL = IL
table <- load_span_table_df("./sdl_to_il.xlsx", "il")
table

saveRDS(table, file = "sdl_to_il_table.rds")
readRDS(file = "sdl_to_il_table.rds" )

library(plotly)

# Get Manufacturer
mtcars$manuf <- sapply(strsplit(rownames(mtcars), " "), "[[", 1)

mtcars$manuf
str(mtcars)

x <- data.frame("depth" = c(150,200,250,260,300,350,400,450), 
                "selfweight_kN_pm2" = c(2.36,2.98,3.62,3.47,3.99,4.53,5.15,5.46))
x

mask = x$depth==250
df_selection <- x[which(mask), ]
as.numeric(df_selection[2])

library("ggplot2")
df <- data.frame(a = c(4, 3, 3, 8, 1, 1, 10),
                 b = c("x", "x", "x", "y", "y", "y", "z"),
                 c = c("x1", "x2", "x3", "y1", "y2", "y3", "z1"))

ggplot(df, aes(x = b, y = a, fill = c))+
  geom_bar(stat = "identity")+
  coord_polar(theta="y")

