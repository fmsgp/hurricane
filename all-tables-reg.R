library(tidyverse)
library(fixest)
library(strucchange)

options(digits = 4)


# Table 1 ----------------------------------------------------------------

### reading pre-processed data

## Column 1 / 2
df_data_proc <- read_delim('merchant_processed_data_for_tbl_1.csv')


### creating features for regression
df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)


df_data_proc$pre <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_prep <- ifelse((df_data_proc$dist %in% c(-5,-4,-3,-2)), 1, 0)
df_data_proc$post_during <- ifelse((df_data_proc$dist %in% c(-1,0,1,2)), 1, 0)
df_data_proc$post_recovery <- ifelse(df_data_proc$dist %in% c(3,4,5,6,7,8,9,10), 1, 0)

### a list that will contain the results for different columns
pre_post_results <- list()
pre_during_post_results <- list()

i <- 'affected'

### filtering zip codes to contain control (null), and affected
df_temp <- df_data_proc %>%
  filter(inside_polygon %in% c('NULL',i))

### Panel A
model_pre_post <- feglm(
  log_tot_spend ~ pre_base + post_base,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_post_results[['merchant_log_volume']] <- model_pre_post

model_pre_post <- feglm(
  log_txn_cnt ~ pre_base + post_base,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_post_results[['merchant_log_transaction']] <- model_pre_post


### Panel B
model_pre_during_post <- feglm(
  log_tot_spend ~ pre + post_prep + post_during + post_recovery,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_during_post_results[['merchant_log_volume']] <- model_pre_during_post

model_pre_during_post <- feglm(
  log_txn_cnt ~ pre + post_prep + post_during + post_recovery,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_during_post_results[['merchant_log_transaction']] <- model_pre_during_post

## Column 3 
df_data_proc <- read_delim('customer_processed_data_for_tbl_1.csv')


df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)


df_data_proc$pre <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_prep <- ifelse((df_data_proc$dist %in% c(-5,-4,-3,-2)), 1, 0)
df_data_proc$post_during <- ifelse((df_data_proc$dist %in% c(-1,0,1,2)), 1, 0)
df_data_proc$post_recovery <- ifelse(df_data_proc$dist %in% c(3,4,5,6,7,8,9,10), 1, 0)

i <- 'affected'

df_temp <- df_data_proc %>%
  filter(inside_polygon %in% c('NULL',i))

model_pre_post <- feglm(
  log_tot_spend ~ pre_base + post_base,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_post_results[['cust_all_txn_volume']] <- model_pre_post

model_pre_during_post <- feglm(
  log_tot_spend ~ pre + post_prep + post_during + post_recovery,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_during_post_results[['cust_all_txn_volume']] <- model_pre_during_post

## Column 4
df_data_proc <- read_delim('cust_merch_100km_processed_data_for_tbl_1.csv')
df_data_fix$pre_base <- ifelse((df_data_fix$dist %in% c(-7,-6)) , 1, 0)
df_data_fix$post_base <- ifelse(df_data_fix$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)


df_data_fix$pre <- ifelse((df_data_fix$dist %in% c(-7,-6)) , 1, 0)
df_data_fix$post_prep <- ifelse((df_data_fix$dist %in% c(-5,-4,-3,-2)), 1, 0)
df_data_fix$post_during <- ifelse((df_data_fix$dist %in% c(-1,0,1,2)), 1, 0)
df_data_fix$post_recovery <- ifelse(df_data_fix$dist %in% c(3,4,5,6,7,8,9,10), 1, 0)

i <- 'affected'

df_temp <- df_data_proc %>%
  filter(inside_polygon %in% c('NULL',i))

model_pre_post <- feglm(
  log_tot_spend ~ pre_base + post_base,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_post_results[['0-100_txn_volume']] <- model_pre_post

rm(model_pre_post)
gc()


model_pre_during_post <- feglm(
  log_tot_spend ~ pre + post_prep + post_during + post_recovery,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_during_post_results[['0-100_txn_volume']] <- model_pre_during_post
rm(model_pre_during_post)
gc()


## Column 5

df_data_proc <- read_delim('customer_online_processed_data_for_tbl_1.csv')


df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)


df_data_proc$pre <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_prep <- ifelse((df_data_proc$dist %in% c(-5,-4,-3,-2)), 1, 0)
df_data_proc$post_during <- ifelse((df_data_proc$dist %in% c(-1,0,1,2)), 1, 0)
df_data_proc$post_recovery <- ifelse(df_data_proc$dist %in% c(3,4,5,6,7,8,9,10), 1, 0)

i <- 'affected'

df_temp <- df_data_proc %>%
  filter(inside_polygon %in% c('NULL',i))

model_pre_post <- feglm(
  log_tot_spend ~ pre_base + post_base,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_post_results[['top_online_volume']] <- model_pre_post

model_pre_during_post <- feglm(
  log_tot_spend ~ pre + post_prep + post_during + post_recovery,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_during_post_results[['top_online_volume']] <- model_pre_during_post


## Column 6 

df_data_proc <- read_delim('customer_atm_processed_data_for_tbl_1.csv')


df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)


df_data_proc$pre <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_prep <- ifelse((df_data_proc$dist %in% c(-5,-4,-3,-2)), 1, 0)
df_data_proc$post_during <- ifelse((df_data_proc$dist %in% c(-1,0,1,2)), 1, 0)
df_data_proc$post_recovery <- ifelse(df_data_proc$dist %in% c(3,4,5,6,7,8,9,10), 1, 0)

i <- 'affected'

df_temp <- df_data_proc %>%
  filter(inside_polygon %in% c('NULL',i))

model_pre_post <- feglm(
  log_tot_spend ~ pre_base + post_base,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_post_results[['atm_volume']] <- model_pre_post

model_pre_during_post <- feglm(
  log_tot_spend ~ pre + post_prep + post_during + post_recovery,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_during_post_results[['atm_volume']] <- model_pre_during_post

## Consolidation 


etable(
  pre_post_results, 
  tex=F,
  se.below = TRUE,
  fitstat = c("n"),
  dict = c(pre_base = "Pre-hurricane",
           post_base = 'Post-hurricane',
           new_id = 'Zip-Code FE',
           local_txn_date = 'Time FE',
           zip_code = 'Merchant Zip'),
  file = 'results//table1.txt'
)




etable(
  pre_during_post_results, 
  tex=F,
  se.below = TRUE,
  fitstat = c("n"),
  dict = c(pre = "Pre-hurricane",
           post_prep = 'Post-hurricane (prep)',
           post_during = "Post-hurricane (during)",
           post_recovery = 'Post-hurricane (recovery)',
           new_id = 'Zip-Code FE',
           local_txn_date = 'Time FE',
           zip_code = 'Merchant Zip'),
  file = 'results//table1.txt'
)




# Table A3 ----------------------------------------------------------------

### Baseline
df_data_proc <- read_delim('merchant_processed_data_for_tbl_1.csv')

df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)

df_data_proc$pre <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_prep <- ifelse((df_data_proc$dist %in% c(-5,-4,-3,-2)), 1, 0)
df_data_proc$post_during <- ifelse((df_data_proc$dist %in% c(-1,0,1,2)), 1, 0)
df_data_proc$post_recovery <- ifelse(df_data_proc$dist %in% c(3,4,5,6,7,8,9,10), 1, 0)

i <- 'affected'

df_temp <- df_data_proc %>%
  filter(inside_polygon %in% c('NULL',i))

model_pre_post <- feglm(
  log_tot_spend ~ pre_base + post_base,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_post_results[['Baseline']] <- model_pre_post


model_pre_during_post <- feglm(
  log_tot_spend ~ pre + post_prep + post_during + post_recovery,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_during_post_results[['Baseline']] <- model_pre_during_post

## Levels (column 1)

df_data_proc <- read_delim('merchant_levels_processed_data_for_tbl_1.csv')

df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)

df_data_proc$pre <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_prep <- ifelse((df_data_proc$dist %in% c(-5,-4,-3,-2)), 1, 0)
df_data_proc$post_during <- ifelse((df_data_proc$dist %in% c(-1,0,1,2)), 1, 0)
df_data_proc$post_recovery <- ifelse(df_data_proc$dist %in% c(3,4,5,6,7,8,9,10), 1, 0)


i <- 'affected'

df_temp <- df_data_proc %>%
  filter(inside_polygon %in% c('NULL',i))

model_pre_post <- feglm(
  tot_spend ~ pre_base + post_base,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_post_results[['Volume']] <- model_pre_post


model_pre_during_post <- feglm(
  tot_spend ~ pre + post_prep + post_during + post_recovery,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_during_post_results[['Volume']] <- model_pre_during_post

## Cents (Column 2)

df_data_proc <- read_delim('merchant_levels_cents_processed_data_for_tbl_1.csv')

df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)

df_data_proc$pre <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_prep <- ifelse((df_data_proc$dist %in% c(-5,-4,-3,-2)), 1, 0)
df_data_proc$post_during <- ifelse((df_data_proc$dist %in% c(-1,0,1,2)), 1, 0)
df_data_proc$post_recovery <- ifelse(df_data_proc$dist %in% c(3,4,5,6,7,8,9,10), 1, 0)


i <- 'affected'

df_temp <- df_data_proc %>%
  filter(inside_polygon %in% c('NULL',i))

model_pre_post <- feglm(
  log_tot_spend ~ pre_base + post_base,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_post_results[['cents']] <- model_pre_post

rm(model_pre_post)
gc()



model_pre_during_post <- feglm(
  log_tot_spend ~ pre + post_prep + post_during + post_recovery,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_during_post_results[['cents']] <- model_pre_during_post




## Alternate Treatment-timing (Column 3)
df_data_proc <- read_delim('alternate_sample_processed_data_for_tbl_1.csv')

df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)

df_data_proc$pre <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_prep <- ifelse((df_data_proc$dist %in% c(-5,-4,-3,-2)), 1, 0)
df_data_proc$post_during <- ifelse((df_data_proc$dist %in% c(-1,0,1,2)), 1, 0)
df_data_proc$post_recovery <- ifelse(df_data_proc$dist %in% c(3,4,5,6,7,8,9,10), 1, 0)


i <- 'affected'

df_temp <- df_data_proc %>%
  filter(inside_polygon %in% c('NULL',i))

model_pre_post <- feglm(
  log_tot_spend ~ pre_base + post_base,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_post_results[['Alternate Sample']] <- model_pre_post


model_pre_during_post <- feglm(
  log_tot_spend ~ pre + post_prep + post_during + post_recovery,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_during_post_results[['Alternate Sample']] <- model_pre_during_post

## Conditional on having transaction (Column 7)

df_data_proc <- read_delim('merchant_nofill_processed_data_for_tbl_1.csv')

df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)

df_data_proc$pre <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_prep <- ifelse((df_data_proc$dist %in% c(-5,-4,-3,-2)), 1, 0)
df_data_proc$post_during <- ifelse((df_data_proc$dist %in% c(-1,0,1,2)), 1, 0)
df_data_proc$post_recovery <- ifelse(df_data_proc$dist %in% c(3,4,5,6,7,8,9,10), 1, 0)


i <- 'affected'

df_temp <- df_data_proc %>%
  filter(inside_polygon %in% c('NULL',i))

model_pre_post <- feglm(
  log_tot_spend ~ pre_base + post_base,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_post_results[['Nofill']] <- model_pre_post


model_pre_during_post <- feglm(
  log_tot_spend ~ pre + post_prep + post_during + post_recovery,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_during_post_results[['Nofill']] <- model_pre_during_post

## All unaffected (Column 4)

df_temp <- df_data_proc %>%
  filter(inside_polygon %in% c('NULL',i))

model_pre_post <- feglm(
  log_tot_spend ~ pre_base + post_base,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_post_results[['No-buffer']] <- model_pre_post


model_pre_during_post <- feglm(
  log_tot_spend ~ pre + post_prep + post_during + post_recovery,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_during_post_results[['No-buffer']] <- model_pre_during_post


## Excluding 0-100km buffer (Column 5)

i <- 'affected'

df_temp <- df_data_proc %>%
  filter(inside_polygon %in% c('NULL',i))



model_pre_post <- feglm(
  log_tot_spend ~ pre_base + post_base,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_post_results[['100km-buffer']] <- model_pre_post


model_pre_during_post <- feglm(
  log_tot_spend ~ pre + post_prep + post_during + post_recovery,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_during_post_results[['100km-buffer']] <- model_pre_during_post

## Early Onsite (Column 6)

i <- 'affected'

df_temp <- df_data_proc %>%
  filter(inside_polygon %in% c('NULL',i))

model_pre_post <- feglm(
  log_tot_spend ~ pre_base + post_base,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_post_results[['Only-first-day']] <- model_pre_post


model_pre_during_post <- feglm(
  log_tot_spend ~ pre + post_prep + post_during + post_recovery,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_during_post_results[['Only-first-day']] <- model_pre_during_post



## Consolidate

etable(
  pre_post_results, 
  tex=F,
  se.below = TRUE,
  fitstat = c("n"),
  dict = c(pre_base = "Pre-hurricane",
           post_base = 'Post-hurricane',
           new_id = 'Zip-Code FE',
           local_txn_date = 'Time FE',
           zip_code = 'Merchant Zip'),
  file = 'results//tableA3.txt'
)




etable(
  pre_during_post_results, 
  tex=F,
  se.below = TRUE,
  fitstat = c("n"),
  dict = c(pre = "Pre-hurricane",
           post_prep = 'Post-hurricane (prep)',
           post_during = "Post-hurricane (during)",
           post_recovery = 'Post-hurricane (recovery)',
           new_id = 'Zip-Code FE',
           local_txn_date = 'Time FE',
           zip_code = 'Merchant Zip'),
  file = 'results//tableA3.txt'
)








# Table A4 ----------------------------------------------------------------

df_data_proc <- read_delim('merchant_processed_data_for_tbl_4_high_low.csv')

hurricane_data <- readxl::read_excel('hurricane_category.xlsx') %>%
  janitor::clean_names()


hurricane_low <- hurricane_data %>%
  filter(hurricane_category_approach_1=='Low') %>%
  pull(storm_name) %>%
  unique()

hurricane_high <- hurricane_data %>%
  filter(hurricane_category_approach_1=='High') %>%
  pull(storm_name) %>%
  unique()

df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)


df_data_proc$pre <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_prep <- ifelse((df_data_proc$dist %in% c(-5,-4,-3,-2)), 1, 0)
df_data_proc$post_during <- ifelse((df_data_proc$dist %in% c(-1,0,1,2)), 1, 0)
df_data_proc$post_recovery <- ifelse(df_data_proc$dist %in% c(3,4,5,6,7,8,9,10), 1, 0)

pre_post_results <- list()
pre_during_post_results <- list()

i <- 'affected'

df_temp <- df_data_proc %>%
  filter(inside_polygon %in% c('NULL',i)) 

model_pre_post <- feglm(
  log_tot_spend ~ pre_base + post_base,
  data = df_temp %>%
    filter(storm_name_fx %in% hurricane_low),
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_post_results[['category_low']] <- model_pre_post

rm(model_pre_post)
gc()

model_pre_post <- feglm(
  log_tot_spend ~ pre_base + post_base,
  data = df_temp %>%
    filter(storm_name_fx %in% hurricane_high),
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_post_results[['category_high']] <- model_pre_post

rm(model_pre_post)
gc()

model_pre_during_post <- feglm(
  log_tot_spend ~ pre + post_prep + post_during + post_recovery,
  data = df_temp %>%
    filter(storm_name_fx %in% hurricane_low),
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_during_post_results[['category_low']] <- model_pre_during_post
rm(model_pre_during_post)
gc()

model_pre_during_post <- feglm(
  log_tot_spend ~ pre + post_prep + post_during + post_recovery,
  data = df_temp %>%
    filter(storm_name_fx %in% hurricane_high),
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_during_post_results[['category_high']] <- model_pre_during_post
rm(model_pre_during_post)



# Table A5 ----------------------------------------------------------------

df_data_proc <- read_delim('all_hurricanes_industry_processed_data.csv')

df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)


df_data_proc$pre <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_prep <- ifelse((df_data_proc$dist %in% c(-5,-4,-3,-2)), 1, 0)
df_data_proc$post_during <- ifelse((df_data_proc$dist %in% c(-1,0,1,2)), 1, 0)
df_data_proc$post_recovery <- ifelse(df_data_proc$dist %in% c(3,4,5,6,7,8,9,10), 1, 0)

pre_post_results <- list()
pre_during_post_results <- list()

i <- 'affected'


for (i in unique(df_data_proc$merchant_category_groups)){
  
  print (i)
  
  name <- paste(i)
  
  model_pre_post <- feglm(
    log_tot_spend ~ pre_base + post_base,
    data = df_data_proc %>% filter(merchant_category_groups==i),
    fixef = c("new_id", "local_txn_date"),
    cluster = c('zip_code','local_txn_date'),
    mem.clean=TRUE
  )
  
  pre_post_results[[name]] <- model_pre_post
  
  rm(model_pre_post)
  gc()
  
  
  model_pre_during_post <- feglm(
    log_tot_spend ~ pre + post_prep + post_during + post_recovery,
    data = df_data_proc %>% filter(merchant_category_groups==i),
    fixef = c("new_id", "local_txn_date"),
    cluster = c('zip_code','local_txn_date'),
    mem.clean=TRUE
  )
  
  pre_during_post_results[[name]] <- model_pre_during_post
  rm(model_pre_during_post)
  gc()
}


etable(
  pre_post_results, 
  tex=F,
  se.below = TRUE,
  fitstat = c("n"),
  dict = c(pre_base = "Pre-hurricane",
           post_base = 'Post-hurricane',
           new_id = 'Zip-Code FE',
           local_txn_date = 'Time FE',
           zip_code = 'Merchant Zip'),
  file = 'results//tableA6.txt'
)




etable(
  pre_during_post_results, 
  tex=F,
  se.below = TRUE,
  fitstat = c("n"),
  dict = c(pre = "Pre-hurricane",
           post_prep = 'Post-hurricane (prep)',
           post_during = "Post-hurricane (during)",
           post_recovery = 'Post-hurricane (recovery)',
           new_id = 'Zip-Code FE',
           local_txn_date = 'Time FE',
           zip_code = 'Merchant Zip'),
  file = 'results//tableA6.txt'
)



# Table A7 ----------------------------------------------------------------

### Independent

df_data_proc <- read_delim('all_hurricanes_industry_independent_full_data_processed.csv')

df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)


df_data_proc$pre <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_prep <- ifelse((df_data_proc$dist %in% c(-5,-4,-3,-2)), 1, 0)
df_data_proc$post_during <- ifelse((df_data_proc$dist %in% c(-1,0,1,2)), 1, 0)
df_data_proc$post_recovery <- ifelse(df_data_proc$dist %in% c(3,4,5,6,7,8,9,10), 1, 0)

pre_post_results <- list()
pre_during_post_results <- list()

i <- 'affected'

pre_post_results <- list()
pre_during_post_results <- list()

i <- 'affected'


for (i in unique(df_data_proc$merchant_category_groups)){
  
  print (i)
  
  name <- paste(i,'-independent')
  
  model_pre_post <- feglm(
    log_tot_spend ~ pre_base + post_base,
    data = df_data_proc %>% filter(merchant_category_groups==i),
    fixef = c("new_id", "local_txn_date"),
    cluster = c('zip_code','local_txn_date'),
    mem.clean=TRUE
  )
  
  pre_post_results[[name]] <- model_pre_post
  
  rm(model_pre_post)
  gc()
  
  
  model_pre_during_post <- feglm(
    log_tot_spend ~ pre + post_prep + post_during + post_recovery,
    data = df_data_proc %>% filter(merchant_category_groups==i),
    fixef = c("new_id", "local_txn_date"),
    cluster = c('zip_code','local_txn_date'),
    mem.clean=TRUE
  )
  
  pre_during_post_results[[name]] <- model_pre_during_post
  rm(model_pre_during_post)
  gc()
}


etable(
  pre_post_results, 
  tex=F,
  se.below = TRUE,
  fitstat = c("n"),
  dict = c(pre_base = "Pre-hurricane",
           post_base = 'Post-hurricane',
           new_id = 'Zip-Code FE',
           local_txn_date = 'Time FE',
           zip_code = 'Merchant Zip'),
  file = 'results//tableA7-independent.txt'
)




etable(
  pre_during_post_results, 
  tex=F,
  se.below = TRUE,
  fitstat = c("n"),
  dict = c(pre = "Pre-hurricane",
           post_prep = 'Post-hurricane (prep)',
           post_during = "Post-hurricane (during)",
           post_recovery = 'Post-hurricane (recovery)',
           new_id = 'Zip-Code FE',
           local_txn_date = 'Time FE',
           zip_code = 'Merchant Zip'),
  file = 'results//tableA7-independent.txt'
)


### Block 

pre_post_results <- list()
pre_during_post_results <- list()

i <- 'affected'


for (i in unique(df_data_proc$merchant_category_groups)){
  
  print (i)
  
  name <- paste(i,'-block')
  
  model_pre_post <- feglm(
    log_tot_spend ~ pre_base + post_base,
    data = df_data_proc %>% filter(merchant_category_groups==i),
    fixef = c("new_id", "local_txn_date"),
    cluster = c('zip_code','local_txn_date'),
    mem.clean=TRUE
  )
  
  pre_post_results[[name]] <- model_pre_post
  
  rm(model_pre_post)
  gc()
  
  
  model_pre_during_post <- feglm(
    log_tot_spend ~ pre + post_prep + post_during + post_recovery,
    data = df_data_proc %>% filter(merchant_category_groups==i),
    fixef = c("new_id", "local_txn_date"),
    cluster = c('zip_code','local_txn_date'),
    mem.clean=TRUE
  )
  
  pre_during_post_results[[name]] <- model_pre_during_post
  rm(model_pre_during_post)
  gc()
}


etable(
  pre_post_results, 
  tex=F,
  se.below = TRUE,
  fitstat = c("n"),
  dict = c(pre_base = "Pre-hurricane",
           post_base = 'Post-hurricane',
           new_id = 'Zip-Code FE',
           local_txn_date = 'Time FE',
           zip_code = 'Merchant Zip'),
  file = 'results//tableA7-block.txt'
)




etable(
  pre_during_post_results, 
  tex=F,
  se.below = TRUE,
  fitstat = c("n"),
  dict = c(pre = "Pre-hurricane",
           post_prep = 'Post-hurricane (prep)',
           post_during = "Post-hurricane (during)",
           post_recovery = 'Post-hurricane (recovery)',
           new_id = 'Zip-Code FE',
           local_txn_date = 'Time FE',
           zip_code = 'Merchant Zip'),
  file = 'results//tableA7-blocktxt'
)



# Table A8 ----------------------------------------------------------------

### Independent

df_data_proc <- read_delim('all_hurricanes_industry_independent_common_data_processed.csv')

df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)


df_data_proc$pre <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_prep <- ifelse((df_data_proc$dist %in% c(-5,-4,-3,-2)), 1, 0)
df_data_proc$post_during <- ifelse((df_data_proc$dist %in% c(-1,0,1,2)), 1, 0)
df_data_proc$post_recovery <- ifelse(df_data_proc$dist %in% c(3,4,5,6,7,8,9,10), 1, 0)

pre_post_results <- list()
pre_during_post_results <- list()



i <- 'affected'


for (i in unique(df_data_proc$merchant_category_groups)){
  
  print (i)
  
  name <- paste(i,'-independent')
  
  model_pre_post <- feglm(
    log_tot_spend ~ pre_base + post_base,
    data = df_data_proc %>% filter(merchant_category_groups==i),
    fixef = c("new_id", "local_txn_date"),
    cluster = c('zip_code','local_txn_date'),
    mem.clean=TRUE
  )
  
  pre_post_results[[name]] <- model_pre_post
  
  rm(model_pre_post)
  gc()
  
  
  model_pre_during_post <- feglm(
    log_tot_spend ~ pre + post_prep + post_during + post_recovery,
    data = df_data_proc %>% filter(merchant_category_groups==i),
    fixef = c("new_id", "local_txn_date"),
    cluster = c('zip_code','local_txn_date'),
    mem.clean=TRUE
  )
  
  pre_during_post_results[[name]] <- model_pre_during_post
  rm(model_pre_during_post)
  gc()
}


etable(
  pre_post_results, 
  tex=F,
  se.below = TRUE,
  fitstat = c("n"),
  dict = c(pre_base = "Pre-hurricane",
           post_base = 'Post-hurricane',
           new_id = 'Zip-Code FE',
           local_txn_date = 'Time FE',
           zip_code = 'Merchant Zip'),
  file = 'results//tableA8-independent.txt'
)




etable(
  pre_during_post_results, 
  tex=F,
  se.below = TRUE,
  fitstat = c("n"),
  dict = c(pre = "Pre-hurricane",
           post_prep = 'Post-hurricane (prep)',
           post_during = "Post-hurricane (during)",
           post_recovery = 'Post-hurricane (recovery)',
           new_id = 'Zip-Code FE',
           local_txn_date = 'Time FE',
           zip_code = 'Merchant Zip'),
  file = 'results//tableA8-independent.txt'
)


### Block 

pre_post_results <- list()
pre_during_post_results <- list()

i <- 'affected'


for (i in unique(df_data_proc$merchant_category_groups)){
  
  print (i)
  
  name <- paste(i,'-block')
  
  model_pre_post <- feglm(
    log_tot_spend ~ pre_base + post_base,
    data = df_data_proc %>% filter(merchant_category_groups==i),
    fixef = c("new_id", "local_txn_date"),
    cluster = c('zip_code','local_txn_date'),
    mem.clean=TRUE
  )
  
  pre_post_results[[name]] <- model_pre_post
  
  rm(model_pre_post)
  gc()
  
  
  model_pre_during_post <- feglm(
    log_tot_spend ~ pre + post_prep + post_during + post_recovery,
    data = df_data_proc %>% filter(merchant_category_groups==i),
    fixef = c("new_id", "local_txn_date"),
    cluster = c('zip_code','local_txn_date'),
    mem.clean=TRUE
  )
  
  pre_during_post_results[[name]] <- model_pre_during_post
  rm(model_pre_during_post)
  gc()
}


etable(
  pre_post_results, 
  tex=F,
  se.below = TRUE,
  fitstat = c("n"),
  dict = c(pre_base = "Pre-hurricane",
           post_base = 'Post-hurricane',
           new_id = 'Zip-Code FE',
           local_txn_date = 'Time FE',
           zip_code = 'Merchant Zip'),
  file = 'results//tableA8-block.txt'
)




etable(
  pre_during_post_results, 
  tex=F,
  se.below = TRUE,
  fitstat = c("n"),
  dict = c(pre = "Pre-hurricane",
           post_prep = 'Post-hurricane (prep)',
           post_during = "Post-hurricane (during)",
           post_recovery = 'Post-hurricane (recovery)',
           new_id = 'Zip-Code FE',
           local_txn_date = 'Time FE',
           zip_code = 'Merchant Zip'),
  file = 'results//tableA8-blocktxt'
)




# Table A10 ---------------------------------------------------------------

### Offline

df_data_proc <- read_delim('all_hurricanes_industry_offline_data_processed.csv')

df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)


df_data_proc$pre <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_prep <- ifelse((df_data_proc$dist %in% c(-5,-4,-3,-2)), 1, 0)
df_data_proc$post_during <- ifelse((df_data_proc$dist %in% c(-1,0,1,2)), 1, 0)
df_data_proc$post_recovery <- ifelse(df_data_proc$dist %in% c(3,4,5,6,7,8,9,10), 1, 0)

pre_post_results <- list()
pre_during_post_results <- list()

for (i in unique(df_data_proc$merchant_category_groups)){
  
  print (i)
  
  name <- paste(i,'_offline')
  
  model_pre_post <- feglm(
    log_tot_spend ~ pre_base + post_base,
    data = df_data_proc %>% filter(merchant_category_groups==i),
    fixef = c("new_id", "local_txn_date"),
    cluster = c('zip_code','local_txn_date'),
    mem.clean=TRUE
  )
  
  pre_post_results[[name]] <- model_pre_post
  
  rm(model_pre_post)
  gc()
  
  
  model_pre_during_post <- feglm(
    log_tot_spend ~ pre + post_prep + post_during + post_recovery,
    data = df_data_proc %>% filter(merchant_category_groups==i),
    fixef = c("new_id", "local_txn_date"),
    cluster = c('zip_code','local_txn_date'),
    mem.clean=TRUE
  )
  
  pre_during_post_results[[name]] <- model_pre_during_post
  rm(model_pre_during_post)
  gc()
}


### Online

df_data_proc <- read_delim('all_hurricanes_industry_online_data_processed.csv')

df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)


df_data_proc$pre <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_prep <- ifelse((df_data_proc$dist %in% c(-5,-4,-3,-2)), 1, 0)
df_data_proc$post_during <- ifelse((df_data_proc$dist %in% c(-1,0,1,2)), 1, 0)
df_data_proc$post_recovery <- ifelse(df_data_proc$dist %in% c(3,4,5,6,7,8,9,10), 1, 0)

pre_post_results <- list()
pre_during_post_results <- list()

for (i in unique(df_data_proc$merchant_category_groups)){
  
  print (i)
  
  name <- paste(i,'_offline')
  
  model_pre_post <- feglm(
    log_tot_spend ~ pre_base + post_base,
    data = df_data_proc %>% filter(merchant_category_groups==i),
    fixef = c("new_id", "local_txn_date"),
    cluster = c('zip_code','local_txn_date'),
    mem.clean=TRUE
  )
  
  pre_post_results[[name]] <- model_pre_post
  
  rm(model_pre_post)
  gc()
  
  
  model_pre_during_post <- feglm(
    log_tot_spend ~ pre + post_prep + post_during + post_recovery,
    data = df_data_proc %>% filter(merchant_category_groups==i),
    fixef = c("new_id", "local_txn_date"),
    cluster = c('zip_code','local_txn_date'),
    mem.clean=TRUE
  )
  
  pre_during_post_results[[name]] <- model_pre_during_post
  rm(model_pre_during_post)
  gc()
}
