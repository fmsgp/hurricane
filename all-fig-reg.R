library(tidyverse)
library(fixest)
library(strucchange)

options(digits = 4)


# Fig 2 -------------------------------------------------------------------

#### a 


df_data_proc <- read_delim('merchant_processed_data_for_tbl_1.csv')

df_data_proc <- df_data_proc %>%
  mutate(
    e_n10=ifelse(dist==-10,1,0),
    e_n9=ifelse(dist==-9,0,0),
    e_n8=ifelse(dist==-8,1,0),
    e_n7=ifelse(dist==-7,1,0),
    e_n6=ifelse(dist==-6,1,0),
    e_n5=ifelse(dist==-5,1,0),
    e_n4=ifelse(dist==-4,1,0),
    e_n3=ifelse(dist==-3,1,0),
    e_n2=ifelse(dist==-2,1,0),
    e_n1=ifelse(dist==-1,1,0),
    e_0=ifelse(dist==0,1,0),
    e_1=ifelse(dist==1,1,0),
    e_2=ifelse(dist==2,1,0),
    e_3=ifelse(dist==3,1,0),
    e_4=ifelse(dist==4,1,0),
    e_5=ifelse(dist==5,1,0),
    e_6=ifelse(dist==6,1,0),
    e_7=ifelse(dist==7,1,0),
    e_8=ifelse(dist==8,1,0),
    e_9=ifelse(dist==9,1,0),
    e_10=ifelse(dist==10,1,0)
  )

event_results <- list()

model_event <- feglm(
  log_tot_spend ~ e_n7+e_n6+e_n5+e_n4+e_n3+e_n2+e_n1+e_0+e_1+e_2+e_3+e_4+e_5+e_6+e_7+e_8+e_9+e_10,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

event_results[['filled_cutoff']] <- model_event
rm(model_event)
gc()

df_out_log <- data.frame()
for (j in names(event_results)){
  
  out_feglm <- summary(event_results[[j]])
  
  df_temp_log <- as.data.frame(
    coef(event_results[[j]])) %>%
    rownames_to_column() %>%
    rename(day = rowname) %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    mutate(
      std_error = out_feglm$se
    ) %>%
    rename(
      estimate = coef_event_results_j
    ) %>%
    mutate(
      lower = estimate-1.96*std_error,
      higher = estimate+1.96*std_error,
      day = str_replace(day,'e_n','-'),
      day = as.numeric(str_replace(day,'e_',''))
    ) %>%
    rbind(
      data.frame(
        day = -8,
        estimate = 0,
        std_error = 0,
        lower = 0,
        higher = 0
      )
    ) %>%
    arrange(day) %>%
    mutate(
      industry = i,
      cluster=j
    )
  
  rm(out_feglm)
  gc()
  
  df_out_log <- rbind(df_out_log,df_temp_log)
}


write.csv(df_out_log,'results//fig2a.csv',row.names = F)

df_out_log <- read_csv('results//fig2a.csv')

plt_temp <- ggplot(df_out_log, aes(x=day, y=estimate)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="solid", color = "gray") +
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4,
                position=position_dodge(0.05)) +
  geom_line(aes(x=day, y=estimate)) +
  geom_point() +
  theme_bw() +
  ylim(-1,0.5) +
  xlab('event day') + 
  ylab('log of daily transaction volume') + 
  scale_x_continuous(breaks = seq(-10,20,by=1)) +
  #ggtitle(paste0('impact of hurricanes on log-spending at local merchants')) +
  #ggplot2::theme_minimal() +
  theme(
    legend.position = 'top',
    axis.text.y = element_text(size=25),
    axis.title.y = element_text(size=25),
    axis.text.x = element_text(size=25),
    axis.title.x = element_text(size=25),
    text = element_text(size=25)
  ) 

file_name <- paste0('results//fig2a.png')
ggsave(file_name,plt_temp,dpi=600,width=15,height=8) 


### 2B

df_data_proc <- read_delim('merchant_processed_data_for_tbl_4_high_low.csv')

df_data_proc <- df_data_proc %>%
  mutate(
    e_n10=ifelse(dist==-10,1,0),
    e_n9=ifelse(dist==-9,0,0),
    e_n8=ifelse(dist==-8,1,0),
    e_n7=ifelse(dist==-7,1,0),
    e_n6=ifelse(dist==-6,1,0),
    e_n5=ifelse(dist==-5,1,0),
    e_n4=ifelse(dist==-4,1,0),
    e_n3=ifelse(dist==-3,1,0),
    e_n2=ifelse(dist==-2,1,0),
    e_n1=ifelse(dist==-1,1,0),
    e_0=ifelse(dist==0,1,0),
    e_1=ifelse(dist==1,1,0),
    e_2=ifelse(dist==2,1,0),
    e_3=ifelse(dist==3,1,0),
    e_4=ifelse(dist==4,1,0),
    e_5=ifelse(dist==5,1,0),
    e_6=ifelse(dist==6,1,0),
    e_7=ifelse(dist==7,1,0),
    e_8=ifelse(dist==8,1,0),
    e_9=ifelse(dist==9,1,0),
    e_10=ifelse(dist==10,1,0)
  )



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

event_results <- list()

model_event <- feglm(
  log_tot_spend ~ e_n7+e_n6+e_n5+e_n4+e_n3+e_n2+e_n1+e_0+e_1+e_2+e_3+e_4+e_5+e_6+e_7+e_8+e_9+e_10,
  data = df_temp %>%
    filter(storm_name_fx %in% hurricane_low),
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

event_results[['category_low']] <- model_event
rm(model_event)
gc()

model_event <- feglm(
  log_tot_spend ~ e_n7+e_n6+e_n5+e_n4+e_n3+e_n2+e_n1+e_0+e_1+e_2+e_3+e_4+e_5+e_6+e_7+e_8+e_9+e_10,
  data = df_temp %>%
    filter(storm_name_fx %in% hurricane_high),
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

event_results[['category_high']] <- model_event



df_out_log <- data.frame()
for (j in names(event_results)){
  
  out_feglm <- summary(event_results[[j]])
  
  df_temp_log <- as.data.frame(
    coef(event_results[[j]])) %>%
    rownames_to_column() %>%
    rename(day = rowname) %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    mutate(
      std_error = out_feglm$se
    ) %>%
    rename(
      estimate = coef_event_results_j
    ) %>%
    mutate(
      lower = estimate-1.96*std_error,
      higher = estimate+1.96*std_error,
      day = str_replace(day,'e_n','-'),
      day = as.numeric(str_replace(day,'e_',''))
    ) %>%
    rbind(
      data.frame(
        day = -8,
        estimate = 0,
        std_error = 0,
        lower = 0,
        higher = 0
      )
    ) %>%
    arrange(day) %>%
    mutate(
      intensity=j
    )
  
  rm(out_feglm)
  gc()
  
  df_out_log <- rbind(df_out_log,df_temp_log)
}


write.csv(df_out_log,'results//fig2b.csv',row.names = F)

df_out_log <- read_csv('results//fig2b.csv') %>%
  mutate(
    intensity = case_when(
      intensity == 'category_high' ~ 'High',
      TRUE ~ 'Low'
    )
  )

plt_temp <- ggplot(df_out_log, aes(x=day, y=estimate,color=intensity)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="solid", color = "gray") +
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4,
                position=position_dodge(0.05)) +
  geom_line(aes(x=day, y=estimate)) +
  geom_point() +
  theme_bw() +
  ylim(-1,0.5) +
  xlab('event day') + 
  ylab('log of daily transaction volume') + 
  scale_x_continuous(breaks = seq(-10,20,by=1)) +
  #ggtitle(paste0('impact of hurricanes on log-spending at local merchants')) +
  #ggplot2::theme_minimal() +
  theme(
    legend.position = 'top',
    axis.text.y = element_text(size=25),
    axis.title.y = element_text(size=25),
    axis.text.x = element_text(size=25),
    axis.title.x = element_text(size=25),
    text = element_text(size=25)
  ) 

file_name <- paste0('results//fig2b.png')
ggsave(file_name,plt_temp,dpi=600,width=15,height=8) 



# Fig 3 -------------------------------------------------------------------

df_data_proc <- read_delim('all_hurricanes_industry_processed_data.csv')

df_data_fix <- df_data_fix %>%
  mutate(
    e_n10=ifelse(dist==-10,1,0),
    e_n9=ifelse(dist==-9,0,0),
    e_n8=ifelse(dist==-8,1,0),
    e_n7=ifelse(dist==-7,1,0),
    e_n6=ifelse(dist==-6,1,0),
    e_n5=ifelse(dist==-5,1,0),
    e_n4=ifelse(dist==-4,1,0),
    e_n3=ifelse(dist==-3,1,0),
    e_n2=ifelse(dist==-2,1,0),
    e_n1=ifelse(dist==-1,1,0),
    e_0=ifelse(dist==0,1,0),
    e_1=ifelse(dist==1,1,0),
    e_2=ifelse(dist==2,1,0),
    e_3=ifelse(dist==3,1,0),
    e_4=ifelse(dist==4,1,0),
    e_5=ifelse(dist==5,1,0),
    e_6=ifelse(dist==6,1,0),
    e_7=ifelse(dist==7,1,0),
    e_8=ifelse(dist==8,1,0),
    e_9=ifelse(dist==9,1,0),
    e_10=ifelse(dist==10,1,0)
  )

event_results <- list()

for (i in unique(df_data_proc$merchant_category_groups)){
  
  print (i)
  
  name <- paste(i)
  
  model_event <- feglm(
    log_tot_spend ~ e_n7+e_n6+e_n5+e_n4+e_n3+e_n2+e_n1+e_0+e_1+e_2+e_3+e_4+e_5+e_6+e_7+e_8+e_9+e_10,
    data = df_data_proc %>% filter(merchant_category_groups==i),
    fixef = c("new_id", "local_txn_date"),
    cluster = c('zip_code','local_txn_date'),
    mem.clean=TRUE
  )
  
  event_results[[name]] <- model_event
  rm(model_event)
  gc()
}


df_out <- data.frame()
for (j in names(event_results)){
  
  out_feglm <- summary(event_results[[j]])
  
  df_temp_log <- as.data.frame(
    coef(event_results[[j]])) %>%
    rownames_to_column() %>%
    rename(day = rowname) %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    mutate(
      std_error = out_feglm$se
    ) %>%
    rename(
      estimate = coef_event_results_j
    ) %>%
    mutate(
      lower = estimate-1.96*std_error,
      higher = estimate+1.96*std_error,
      day = str_replace(day,'e_n','-'),
      day = as.numeric(str_replace(day,'e_',''))
    ) %>%
    rbind(
      data.frame(
        day = -8,
        estimate = 0,
        std_error = 0,
        lower = 0,
        higher = 0
      )
    ) %>%
    arrange(day) %>%
    mutate(
      cluster=j
    )
  
  rm(out_feglm)
  gc()
  
  df_out <- rbind(df_out,df_temp_log)
}


write.csv(df_out,'results//industry-analysis.csv',row.names = F)

df_out_log2 <- read_csv('results//industry-analysis.csv')

for (i in unique(df_out_log2$cluster)){
  name <- paste0('results//industry_whole',i,'.png')
  
  plt_temp <- ggplot(
    df_out_log2 %>% filter(cluster==i) , 
    aes(x=day, y=estimate)
  ) +
    geom_hline(yintercept=0, linetype="dashed", color = "black") +
    geom_vline(xintercept=0, linetype="solid", color = "gray") +
    geom_errorbar(aes(ymin=lower, ymax=higher), width=.4,
                  position=position_dodge(0.05)) +
    geom_line(aes(x=day, y=estimate)) +
    geom_point() +
    theme_bw() +
    ylim(-1.5,0.6) +
    xlab('event day') + 
    ylab('log of daily transaction volume') + 
    #scale_x_continuous(breaks = seq(-10,20,by=1)) +
    #ggtitle(paste0('impact of hurricanes on log-spending at local merchants')) +
    #ggplot2::theme_minimal() +
    theme(
      legend.position = 'top',
      axis.text.y = element_text(size=25),
      axis.title.y = element_text(size=25),
      axis.text.x = element_text(size=25),
      axis.title.x = element_text(size=25),
      text = element_text(size=25)
    ) 
  
  ggsave(name,plt_temp,dpi=600,width=15,height=8)
  
  
}



# Fig 4 -------------------------------------------------------------------

### Q1

df_data_proc <- read_delim('merchant_processed_data_q1.csv')
df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)


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

pre_post_results[['0-20']] <- model_pre_post




### Q2

df_data_proc <- read_delim('merchant_processed_data_q2.csv')
df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)


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

pre_post_results[['20-40']] <- model_pre_post




### Q3

df_data_proc <- read_delim('merchant_processed_data_q3.csv')
df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)


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

pre_post_results[['40-60']] <- model_pre_post



### Q4

df_data_proc <- read_delim('merchant_processed_data_q4.csv')
df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)


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

pre_post_results[['60-80']] <- model_pre_post




### Q5

df_data_proc <- read_delim('merchant_processed_data_q5.csv')
df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)



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

pre_post_results[['80-100']] <- model_pre_post

### Consolidate
event_results <- pre_post_results
df_out_log <- data.frame()
for (j in names(event_results)){
  
  out_feglm <- summary(event_results[[j]])
  
  df_temp_log <- as.data.frame(
    coef(event_results[[j]])) %>%
    rownames_to_column() %>%
    rename(day = rowname) %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    mutate(
      std_error = out_feglm$se
    ) %>%
    rename(
      estimate = coef_event_results_j
    ) %>%
    mutate(
      lower = estimate-1.96*std_error,
      higher = estimate+1.96*std_error
    ) %>%
    mutate(
      type=j
    )
  
  gc()
  
  df_out_log <- rbind(df_out_log,df_temp_log)
}


df_out_log2 <- df_out_log %>%
  mutate(
    type = factor(
      type,
      levels = c(
        '0-20','20-40','40-60','60-80','80-100'
      )
    )
  )


write.csv(df_out_log2,'results//instore_online_results.csv',row.names = F)

df_out_log2 <- read_csv('results//instore_online_results.csv') %>%
  mutate(
    type = case_when(
      type == '0-20' ~ "<20%",
      type =='20-40' ~ '20%-40%',
      type == '40-60' ~ '40%-60%',
      type == '60-80' ~ '60%-80%',
      type == '80-100' ~ '>80%'
    )
  ) %>%
  mutate(
    type = factor(type,levels = c('<20%','20%-40%','40%-60%','60%-80%','>80%'))
  )


plt_temp <- ggplot(
  df_out_log2 %>% filter(day=='post_base'), 
  aes(x=type, y=estimate)
) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="solid", color = "gray") +
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4,
                position=position_dodge(0.05)) +
  geom_line(aes(x=type, y=estimate)) +
  geom_point() +
  theme_bw() +
  #ylim(-1,0.5) +
  xlab('share of online spending') + 
  ylab('log of daily transaction volume') + 
  #scale_x_continuous(breaks = seq(-10,20,by=1)) +
  #ggtitle(paste0('impact of hurricanes on log-spending at local merchants')) +
  #ggplot2::theme_minimal() +
  theme(
    legend.position = 'top',
    axis.text.y = element_text(size=25),
    axis.title.y = element_text(size=25),
    axis.text.x = element_text(size=25),
    axis.title.x = element_text(size=25),
    text = element_text(size=25)
  ) 

ggsave('results//instore_online_results.png',plt_temp,dpi=600,width=15,height=8) 


# Fig A1 ------------------------------------------------------------------

df_data_proc <-data.table::fread(
  'all_hurricanes_merch_50_level_overall.csv'
) 

df_data_proc <- df_data_proc %>%
  mutate(
    e_n10=ifelse(dist==-10,1,0),
    e_n9=ifelse(dist==-9,0,0),
    e_n8=ifelse(dist==-8,1,0),
    e_n7=ifelse(dist==-7,1,0),
    e_n6=ifelse(dist==-6,1,0),
    e_n5=ifelse(dist==-5,1,0),
    e_n4=ifelse(dist==-4,1,0),
    e_n3=ifelse(dist==-3,1,0),
    e_n2=ifelse(dist==-2,1,0),
    e_n1=ifelse(dist==-1,1,0),
    e_0=ifelse(dist==0,1,0),
    e_1=ifelse(dist==1,1,0),
    e_2=ifelse(dist==2,1,0),
    e_3=ifelse(dist==3,1,0),
    e_4=ifelse(dist==4,1,0),
    e_5=ifelse(dist==5,1,0),
    e_6=ifelse(dist==6,1,0),
    e_7=ifelse(dist==7,1,0),
    e_8=ifelse(dist==8,1,0),
    e_9=ifelse(dist==9,1,0),
    e_10=ifelse(dist==10,1,0)
  )


event_results <- list()

uniq_distance <- c('affected','100km','200km','300km','400km','500km')

df_out_log <- data.frame()
for (i in uniq_distance){
  
  print (i)
  
  name <- paste0('volume_',i)
  
  df_temp <- df_data_proc %>%
    filter(inside_polygon %in% c('NULL',i))
  
  model_pre_post <- feglm(
    log_tot_spend ~ pre_base + post_base,
    data = df_temp,
    fixef = c("new_id", "local_txn_date"),
    cluster = c('zip_code','local_txn_date'),
    mem.clean=TRUE
  )
  
  event_results <- list()
  event_results[[i]] <- model_pre_post
  
  for (j in names(event_results)){
    
    out_feglm <- summary(event_results[[j]])
    
    df_temp_log <- as.data.frame(
      coef(event_results[[j]])) %>%
      rownames_to_column() %>%
      rename(day = rowname) %>%
      janitor::clean_names() %>%
      as_tibble() %>%
      mutate(
        std_error = out_feglm$se
      ) %>%
      rename(
        estimate = coef_event_results_j
      ) %>%
      mutate(
        lower = estimate-1.96*std_error,
        higher = estimate+1.96*std_error
      ) %>%
      mutate(
        distance=i
      )
    
    gc()
    
    df_out_log <- rbind(df_out_log,df_temp_log)
  }
  
  
  
  
  rm(out_feglm)
  rm(event_results)
  rm(df_temp)
  rm(model_event)
  gc()
  
}


df_out_log <- df_out_log %>%
  mutate(
    distance = factor(
      distance,
      levels = c(
        'affected','100km','200km',
        '300km','400km','500km'
      )
    )
  )


write.csv(df_out_log,'results//figA1.csv',row.names = F)
df_out_log <- read_csv('results//figA1.csv') %>%
  mutate(
    distance = factor(
      distance,
      levels = c(
        'affected','100km','200km',
        '300km','400km','500km'
      )
    )
  )

plt_temp <- ggplot(
  df_out_log %>% filter(day=='post_base'), 
  aes(x=distance, y=estimate)
) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="solid", color = "gray") +
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4,
                position=position_dodge(0.05)) +
  geom_line(aes(x=distance, y=estimate)) +
  geom_point() +
  theme_bw() +
  #ylim(-1,0.5) +
  xlab('distance') + 
  ylab('log of daily transaction volume') + 
  #scale_x_continuous(breaks = seq(-10,20,by=1)) +
  #ggtitle(paste0('impact of hurricanes on log-spending at local merchants')) +
  #ggplot2::theme_minimal() +
  theme(
    legend.position = 'top',
    axis.text.y = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.text.x = element_text(size=20),
    axis.title.x = element_text(size=20),
    text = element_text(size=20)
  ) 

ggsave('results//figA1.png',plt_temp,dpi=600,width=15,height=8) 




# Fig A2 ------------------------------------------------------------------

### A

df_data_proc <- read_delim('all_hurricanes_90d_processed_data.csv')

df_data_proc <- df_data_proc %>%
  mutate(
    e_n4=ifelse(dist_week==-4,0,0),
    e_n3=ifelse(dist_week==-3,0,0),
    e_n2=ifelse(dist_week==-2,1,0),
    e_n1=ifelse(dist_week==-1,1,0),
    e_0=ifelse(dist_week==0,1,0),
    e_1=ifelse(dist_week==1,1,0),
    e_2=ifelse(dist_week==2,1,0),
    e_3=ifelse(dist_week==3,1,0),
    e_4=ifelse(dist_week==4,1,0),
    e_5=ifelse(dist_week==5,1,0),
    e_6=ifelse(dist_week==6,1,0),
    e_7=ifelse(dist_week==7,1,0),
    e_8=ifelse(dist_week==8,1,0),
    e_9=ifelse(dist_week==9,1,0),
    e_10=ifelse(dist_week==10,1,0),
    e_11=ifelse(dist_week==11,1,0),
    e_12=ifelse(dist_week==12,1,0)
  )




i <- 'affected'

df_temp <- df_data_proc %>%
  filter(inside_polygon %in% c('NULL',i)) %>%
  mutate(
    year_month = paste(year,month,sep='-'),
    year_week = paste(year,week,sep='-')
  )


event_results <- list()

model_event <- feglm(
  log_tot_spend ~e_n3 + e_n2+e_n1+e_0+e_1+e_2+e_3+e_4+e_5+e_6+e_7+e_8+e_9+e_10+e_11,
  data = df_temp,
  fixef = c("new_id", "year_week"),
  cluster = c('zip_code','year_week'),
  mem.clean=TRUE
)


event_results[['filled_cutoff']] <- model_event
rm(model_event)
gc()

df_out_log <- data.frame()
for (j in names(event_results)){
  
  out_feglm <- summary(event_results[[j]])
  
  df_temp_log <- as.data.frame(
    coef(event_results[[j]])) %>%
    rownames_to_column() %>%
    rename(day = rowname) %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    mutate(
      std_error = out_feglm$se
    ) %>%
    rename(
      estimate = coef_event_results_j
    ) %>%
    mutate(
      lower = estimate-1.96*std_error,
      higher = estimate+1.96*std_error,
      day = str_replace(day,'e_n','-'),
      day = as.numeric(str_replace(day,'e_',''))
    ) %>%
    rbind(
      data.frame(
        day = -3,
        estimate = 0,
        std_error = 0,
        lower = 0,
        higher = 0
      )
    ) %>%
    arrange(day) %>%
    mutate(
      industry = i,
      cluster=j
    )
  
  rm(out_feglm)
  gc()
  
  df_out_log <- rbind(df_out_log,df_temp_log)
}


write.csv(df_out_log,'results//90_day_spend_volume.csv')
df_out_log <- read_csv('results//90_day_spend_volume.csv')

plt_temp <- ggplot(df_out_log, aes(x=day, y=estimate)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="solid", color = "gray") +
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4,
                position=position_dodge(0.05)) +
  geom_line(aes(x=day, y=estimate)) +
  geom_point() +
  theme_bw() +
  ylim(-1,0.5) +
  xlab('week around event day') + 
  ylab('log of daily transaction volume') + 
  scale_x_continuous(breaks = seq(-10,20,by=1)) +
  #ggtitle(paste0('impact of hurricanes on log-spending at local merchants')) +
  #ggplot2::theme_minimal() +
  theme(
    legend.position = 'top',
    axis.text.y = element_text(size=25),
    axis.title.y = element_text(size=25),
    axis.text.x = element_text(size=25),
    axis.title.x = element_text(size=25),
    text = element_text(size=25)
  ) 

file_name <- paste0('results//90_day_spend_volume.png')
ggsave(file_name,plt_temp,dpi=600,width=15,height=8) 

### B

df_data_proc <- read_delim('all_hurricanes_90d_processed_data_cnt.csv')

df_data_proc <- df_data_proc %>%
  mutate(
    e_n4=ifelse(dist_week==-4,0,0),
    e_n3=ifelse(dist_week==-3,0,0),
    e_n2=ifelse(dist_week==-2,1,0),
    e_n1=ifelse(dist_week==-1,1,0),
    e_0=ifelse(dist_week==0,1,0),
    e_1=ifelse(dist_week==1,1,0),
    e_2=ifelse(dist_week==2,1,0),
    e_3=ifelse(dist_week==3,1,0),
    e_4=ifelse(dist_week==4,1,0),
    e_5=ifelse(dist_week==5,1,0),
    e_6=ifelse(dist_week==6,1,0),
    e_7=ifelse(dist_week==7,1,0),
    e_8=ifelse(dist_week==8,1,0),
    e_9=ifelse(dist_week==9,1,0),
    e_10=ifelse(dist_week==10,1,0),
    e_11=ifelse(dist_week==11,1,0),
    e_12=ifelse(dist_week==12,1,0)
  )



i <- 'affected'

df_temp <- df_data_proc %>%
  filter(inside_polygon %in% c('NULL',i)) %>%
  mutate(
    year_month = paste(year,month,sep='-'),
    year_week = paste(year,week,sep='-')
  )


event_results <- list()

model_event <- feglm(
  mmh_cnt ~ e_n2+e_n1+e_0+e_1+e_2+e_3+e_4+e_5+e_6+e_7+e_8+e_9+e_10+e_11,
  data = df_temp,
  fixef = c("new_id", "year_week"),
  cluster = c('zip_code','year_week'),
  mem.clean=TRUE
)


event_results[['filled_cutoff']] <- model_event
rm(model_event)
gc()

df_out_log <- data.frame()
for (j in names(event_results)){
  
  out_feglm <- summary(event_results[[j]])
  
  df_temp_log <- as.data.frame(
    coef(event_results[[j]])) %>%
    rownames_to_column() %>%
    rename(day = rowname) %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    mutate(
      std_error = out_feglm$se
    ) %>%
    rename(
      estimate = coef_event_results_j
    ) %>%
    mutate(
      lower = estimate-1.96*std_error,
      higher = estimate+1.96*std_error,
      day = str_replace(day,'e_n','-'),
      day = as.numeric(str_replace(day,'e_',''))
    ) %>%
    rbind(
      data.frame(
        day = -3,
        estimate = 0,
        std_error = 0,
        lower = 0,
        higher = 0
      )
    ) %>%
    arrange(day) %>%
    mutate(
      industry = i,
      cluster=j
    )
  
  rm(out_feglm)
  gc()
  
  df_out_log <- rbind(df_out_log,df_temp_log)
}


write.csv(df_out_log,'results//90d_merchant_count_base_preexisting.csv')
df_out_log <- read_csv('results//90d_merchant_count_base_preexisting.csv')

plt_temp <- ggplot(df_out_log, aes(x=day, y=estimate)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="solid", color = "gray") +
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4,
                position=position_dodge(0.05)) +
  geom_line(aes(x=day, y=estimate)) +
  geom_point() +
  theme_bw() +
  #ylim(-1,0.5) +
  xlab('event week') + 
  ylab('count of merchants that were active') + 
  scale_x_continuous(breaks = seq(-10,20,by=1)) +
  #ggtitle(paste0('impact of hurricanes on log-spending at local merchants')) +
  #ggplot2::theme_minimal() +
  theme(
    legend.position = 'top',
    axis.text.y = element_text(size=25),
    axis.title.y = element_text(size=25),
    axis.text.x = element_text(size=25),
    axis.title.x = element_text(size=25),
    text = element_text(size=25)
  ) 

file_name <- paste0('results//90d_merchant_count_base_preexisting.png')
ggsave(file_name,plt_temp,dpi=600,width=15,height=8) 


# Fig A3 ------------------------------------------------------------------

### A

df_data_proc <- read_delim('customer_processed_data_for_tbl_1.csv')

df_data_proc <- df_data_proc %>%
  mutate(
    e_n10=ifelse(dist==-10,1,0),
    e_n9=ifelse(dist==-9,0,0),
    e_n8=ifelse(dist==-8,1,0),
    e_n7=ifelse(dist==-7,1,0),
    e_n6=ifelse(dist==-6,1,0),
    e_n5=ifelse(dist==-5,1,0),
    e_n4=ifelse(dist==-4,1,0),
    e_n3=ifelse(dist==-3,1,0),
    e_n2=ifelse(dist==-2,1,0),
    e_n1=ifelse(dist==-1,1,0),
    e_0=ifelse(dist==0,1,0),
    e_1=ifelse(dist==1,1,0),
    e_2=ifelse(dist==2,1,0),
    e_3=ifelse(dist==3,1,0),
    e_4=ifelse(dist==4,1,0),
    e_5=ifelse(dist==5,1,0),
    e_6=ifelse(dist==6,1,0),
    e_7=ifelse(dist==7,1,0),
    e_8=ifelse(dist==8,1,0),
    e_9=ifelse(dist==9,1,0),
    e_10=ifelse(dist==10,1,0)
  )


i <- 'affected'

df_temp <- df_data_proc %>%
  filter(inside_polygon %in% c('NULL',i))

event_results <- list()

model_event <- feglm(
  log_tot_spend ~ e_n7+e_n6+e_n5+e_n4+e_n3+e_n2+e_n1+e_0+e_1+e_2+e_3+e_4+e_5+e_6+e_7+e_8+e_9+e_10,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

event_results[['cust_all_txn_volume']] <- model_event
rm(model_event)
gc()

df_out_log <- data.frame()
for (j in names(event_results)){
  
  out_feglm <- summary(event_results[[j]])
  
  df_temp_log <- as.data.frame(
    coef(event_results[[j]])) %>%
    rownames_to_column() %>%
    rename(day = rowname) %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    mutate(
      std_error = out_feglm$se
    ) %>%
    rename(
      estimate = coef_event_results_j
    ) %>%
    mutate(
      lower = estimate-1.96*std_error,
      higher = estimate+1.96*std_error,
      day = str_replace(day,'e_n','-'),
      day = as.numeric(str_replace(day,'e_',''))
    ) %>%
    rbind(
      data.frame(
        day = -8,
        estimate = 0,
        std_error = 0,
        lower = 0,
        higher = 0
      )
    ) %>%
    arrange(day) %>%
    mutate(
      industry = i,
      cluster=j
    )
  
  rm(out_feglm)
  gc()
  
  df_out_log <- rbind(df_out_log,df_temp_log)
}


write.csv(df_out_log,'results//figA3A.csv',row.names = F)

df_out_log <- read_csv('results//figA3A.csv')

plt_temp <- ggplot(df_out_log, aes(x=day, y=estimate)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="solid", color = "gray") +
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4,
                position=position_dodge(0.05)) +
  geom_line(aes(x=day, y=estimate)) +
  geom_point() +
  theme_bw() +
  ylim(-1,0.5) +
  xlab('event day') + 
  ylab('log of daily transaction volume') + 
  scale_x_continuous(breaks = seq(-10,20,by=1)) +
  #ggtitle(paste0('impact of hurricanes on log-spending at local merchants')) +
  #ggplot2::theme_minimal() +
  theme(
    legend.position = 'top',
    axis.text.y = element_text(size=25),
    axis.title.y = element_text(size=25),
    axis.text.x = element_text(size=25),
    axis.title.x = element_text(size=25),
    text = element_text(size=25)
  ) 

file_name <- paste0('results//figA3A.png')
ggsave(file_name,plt_temp,dpi=600,width=15,height=8) 


### B

df_data_proc <- read_delim('cust_merch_100km_processed_data_for_tbl_1.csv')
df_data_proc <- df_data_proc %>%
  mutate(
    e_n10=ifelse(dist==-10,1,0),
    e_n9=ifelse(dist==-9,0,0),
    e_n8=ifelse(dist==-8,1,0),
    e_n7=ifelse(dist==-7,1,0),
    e_n6=ifelse(dist==-6,1,0),
    e_n5=ifelse(dist==-5,1,0),
    e_n4=ifelse(dist==-4,1,0),
    e_n3=ifelse(dist==-3,1,0),
    e_n2=ifelse(dist==-2,1,0),
    e_n1=ifelse(dist==-1,1,0),
    e_0=ifelse(dist==0,1,0),
    e_1=ifelse(dist==1,1,0),
    e_2=ifelse(dist==2,1,0),
    e_3=ifelse(dist==3,1,0),
    e_4=ifelse(dist==4,1,0),
    e_5=ifelse(dist==5,1,0),
    e_6=ifelse(dist==6,1,0),
    e_7=ifelse(dist==7,1,0),
    e_8=ifelse(dist==8,1,0),
    e_9=ifelse(dist==9,1,0),
    e_10=ifelse(dist==10,1,0)
  )


event_results <- list()

model_event <- feglm(
  log_tot_spend ~ e_n7+e_n6+e_n5+e_n4+e_n3+e_n2+e_n1+e_0+e_1+e_2+e_3+e_4+e_5+e_6+e_7+e_8+e_9+e_10,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

event_results[['all_txn_volume']] <- model_event
rm(model_event)
gc()

df_out_log <- data.frame()
for (j in names(event_results)){
  
  out_feglm <- summary(event_results[[j]])
  
  df_temp_log <- as.data.frame(
    coef(event_results[[j]])) %>%
    rownames_to_column() %>%
    rename(day = rowname) %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    mutate(
      std_error = out_feglm$se
    ) %>%
    rename(
      estimate = coef_event_results_j
    ) %>%
    mutate(
      lower = estimate-1.96*std_error,
      higher = estimate+1.96*std_error,
      day = str_replace(day,'e_n','-'),
      day = as.numeric(str_replace(day,'e_',''))
    ) %>%
    rbind(
      data.frame(
        day = -8,
        estimate = 0,
        std_error = 0,
        lower = 0,
        higher = 0
      )
    ) %>%
    arrange(day) %>%
    mutate(
      industry = i,
      cluster=j
    )
  
  rm(out_feglm)
  gc()
  
  df_out_log <- rbind(df_out_log,df_temp_log)
}


write.csv(df_out_log,'results//figA3B.csv',row.names = F)

df_out_log <- read_csv('results//figA3B.csv')

plt_temp <- ggplot(df_out_log, aes(x=day, y=estimate)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="solid", color = "gray") +
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4,
                position=position_dodge(0.05)) +
  geom_line(aes(x=day, y=estimate)) +
  geom_point() +
  theme_bw() +
  ylim(-1,0.5) +
  xlab('event day') + 
  ylab('log of daily transaction volume') + 
  scale_x_continuous(breaks = seq(-10,20,by=1)) +
  #ggtitle(paste0('impact of hurricanes on log-spending at local merchants')) +
  #ggplot2::theme_minimal() +
  theme(
    legend.position = 'top',
    axis.text.y = element_text(size=25),
    axis.title.y = element_text(size=25),
    axis.text.x = element_text(size=25),
    axis.title.x = element_text(size=25),
    text = element_text(size=25)
  ) 

file_name <- paste0('results//figA3B.png')
ggsave(file_name,plt_temp,dpi=600,width=15,height=8) 

### C

df_data_proc <- read_delim('customer_online_processed_data_for_tbl_1.csv')
df_data_proc <- df_data_proc %>%
  mutate(
    e_n10=ifelse(dist==-10,1,0),
    e_n9=ifelse(dist==-9,0,0),
    e_n8=ifelse(dist==-8,1,0),
    e_n7=ifelse(dist==-7,1,0),
    e_n6=ifelse(dist==-6,1,0),
    e_n5=ifelse(dist==-5,1,0),
    e_n4=ifelse(dist==-4,1,0),
    e_n3=ifelse(dist==-3,1,0),
    e_n2=ifelse(dist==-2,1,0),
    e_n1=ifelse(dist==-1,1,0),
    e_0=ifelse(dist==0,1,0),
    e_1=ifelse(dist==1,1,0),
    e_2=ifelse(dist==2,1,0),
    e_3=ifelse(dist==3,1,0),
    e_4=ifelse(dist==4,1,0),
    e_5=ifelse(dist==5,1,0),
    e_6=ifelse(dist==6,1,0),
    e_7=ifelse(dist==7,1,0),
    e_8=ifelse(dist==8,1,0),
    e_9=ifelse(dist==9,1,0),
    e_10=ifelse(dist==10,1,0)
  )


event_results <- list()

model_event <- feglm(
  log_tot_spend ~ e_n7+e_n6+e_n5+e_n4+e_n3+e_n2+e_n1+e_0+e_1+e_2+e_3+e_4+e_5+e_6+e_7+e_8+e_9+e_10,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

event_results[['top_online_volume']] <- model_event
rm(model_event)
gc()

df_out_log <- data.frame()
for (j in names(event_results)){
  
  out_feglm <- summary(event_results[[j]])
  
  df_temp_log <- as.data.frame(
    coef(event_results[[j]])) %>%
    rownames_to_column() %>%
    rename(day = rowname) %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    mutate(
      std_error = out_feglm$se
    ) %>%
    rename(
      estimate = coef_event_results_j
    ) %>%
    mutate(
      lower = estimate-1.96*std_error,
      higher = estimate+1.96*std_error,
      day = str_replace(day,'e_n','-'),
      day = as.numeric(str_replace(day,'e_',''))
    ) %>%
    rbind(
      data.frame(
        day = -8,
        estimate = 0,
        std_error = 0,
        lower = 0,
        higher = 0
      )
    ) %>%
    arrange(day) %>%
    mutate(
      industry = i,
      cluster=j
    )
  
  rm(out_feglm)
  gc()
  
  df_out_log <- rbind(df_out_log,df_temp_log)
}


write.csv(df_out_log,'results//figA3C.csv',row.names = F)

df_out_log <- read_csv('results//figA3C.csv')

plt_temp <- ggplot(df_out_log, aes(x=day, y=estimate)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="solid", color = "gray") +
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4,
                position=position_dodge(0.05)) +
  geom_line(aes(x=day, y=estimate)) +
  geom_point() +
  theme_bw() +
  ylim(-1,0.5) +
  xlab('event day') + 
  ylab('log of daily transaction volume') + 
  scale_x_continuous(breaks = seq(-10,20,by=1)) +
  #ggtitle(paste0('impact of hurricanes on log-spending at local merchants')) +
  #ggplot2::theme_minimal() +
  theme(
    legend.position = 'top',
    axis.text.y = element_text(size=25),
    axis.title.y = element_text(size=25),
    axis.text.x = element_text(size=25),
    axis.title.x = element_text(size=25),
    text = element_text(size=25)
  ) 

file_name <- paste0('results//figA3C.png')
ggsave(file_name,plt_temp,dpi=600,width=15,height=8) 

### D
df_data_proc <- read_delim('customer_atm_processed_data_for_tbl_1.csv')

df_data_proc <- df_data_proc %>%
  mutate(
    e_n10=ifelse(dist==-10,1,0),
    e_n9=ifelse(dist==-9,0,0),
    e_n8=ifelse(dist==-8,1,0),
    e_n7=ifelse(dist==-7,1,0),
    e_n6=ifelse(dist==-6,1,0),
    e_n5=ifelse(dist==-5,1,0),
    e_n4=ifelse(dist==-4,1,0),
    e_n3=ifelse(dist==-3,1,0),
    e_n2=ifelse(dist==-2,1,0),
    e_n1=ifelse(dist==-1,1,0),
    e_0=ifelse(dist==0,1,0),
    e_1=ifelse(dist==1,1,0),
    e_2=ifelse(dist==2,1,0),
    e_3=ifelse(dist==3,1,0),
    e_4=ifelse(dist==4,1,0),
    e_5=ifelse(dist==5,1,0),
    e_6=ifelse(dist==6,1,0),
    e_7=ifelse(dist==7,1,0),
    e_8=ifelse(dist==8,1,0),
    e_9=ifelse(dist==9,1,0),
    e_10=ifelse(dist==10,1,0)
  )


event_results <- list()

model_event <- feglm(
  log_tot_spend ~ e_n7+e_n6+e_n5+e_n4+e_n3+e_n2+e_n1+e_0+e_1+e_2+e_3+e_4+e_5+e_6+e_7+e_8+e_9+e_10,
  data = df_temp,
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

event_results[['atm_volume']] <- model_event
rm(model_event)
gc()

df_out_log <- data.frame()
for (j in names(event_results)){
  
  out_feglm <- summary(event_results[[j]])
  
  df_temp_log <- as.data.frame(
    coef(event_results[[j]])) %>%
    rownames_to_column() %>%
    rename(day = rowname) %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    mutate(
      std_error = out_feglm$se
    ) %>%
    rename(
      estimate = coef_event_results_j
    ) %>%
    mutate(
      lower = estimate-1.96*std_error,
      higher = estimate+1.96*std_error,
      day = str_replace(day,'e_n','-'),
      day = as.numeric(str_replace(day,'e_',''))
    ) %>%
    rbind(
      data.frame(
        day = -8,
        estimate = 0,
        std_error = 0,
        lower = 0,
        higher = 0
      )
    ) %>%
    arrange(day) %>%
    mutate(
      industry = i,
      cluster=j
    )
  
  rm(out_feglm)
  gc()
  
  df_out_log <- rbind(df_out_log,df_temp_log)
}

write.csv(df_out_log,'results//figA3D.csv',row.names = F)

df_out_log <- read_csv('results//figA3D.csv')

plt_temp <- ggplot(df_out_log, aes(x=day, y=estimate)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="solid", color = "gray") +
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4,
                position=position_dodge(0.05)) +
  geom_line(aes(x=day, y=estimate)) +
  geom_point() +
  theme_bw() +
  ylim(-1,0.5) +
  xlab('event day') + 
  ylab('log of daily transaction volume') + 
  scale_x_continuous(breaks = seq(-10,20,by=1)) +
  #ggtitle(paste0('impact of hurricanes on log-spending at local merchants')) +
  #ggplot2::theme_minimal() +
  theme(
    legend.position = 'top',
    axis.text.y = element_text(size=25),
    axis.title.y = element_text(size=25),
    axis.text.x = element_text(size=25),
    axis.title.x = element_text(size=25),
    text = element_text(size=25)
  ) 

file_name <- paste0('results//figA3D.png')
ggsave(file_name,plt_temp,dpi=600,width=15,height=8) 


# Fig A4 ------------------------------------------------------------------

### Grocery 

df_proc <-data.table::fread(
  'all_hurricanes_merchant_level_size_food_processed.csv'
)

df_proc <- df_proc %>%
  mutate(
    e_n10=ifelse(dist==-10,1,0),
    e_n9=ifelse(dist==-9,0,0),
    e_n8=ifelse(dist==-8,1,0),
    e_n7=ifelse(dist==-7,1,0),
    e_n6=ifelse(dist==-6,1,0),
    e_n5=ifelse(dist==-5,1,0),
    e_n4=ifelse(dist==-4,1,0),
    e_n3=ifelse(dist==-3,1,0),
    e_n2=ifelse(dist==-2,1,0),
    e_n1=ifelse(dist==-1,1,0),
    e_0=ifelse(dist==0,1,0),
    e_1=ifelse(dist==1,1,0),
    e_2=ifelse(dist==2,1,0),
    e_3=ifelse(dist==3,1,0),
    e_4=ifelse(dist==4,1,0),
    e_5=ifelse(dist==5,1,0),
    e_6=ifelse(dist==6,1,0),
    e_7=ifelse(dist==7,1,0),
    e_8=ifelse(dist==8,1,0),
    e_9=ifelse(dist==9,1,0),
    e_10=ifelse(dist==10,1,0)
  )

i <- 'affected'

event_results <- list()

for (i in unique(df_temp$store_type)){
  
  print (i)
  
  name <- paste(i)
  
  model_event <- feglm(
    log_tot_spend ~ e_n7+e_n6+e_n5+e_n4+e_n3+e_n2+e_n1+e_0+e_1+e_2+e_3+e_4+e_5+e_6+e_7+e_8+e_9+e_10,
    data = df_temp %>% filter(store_type==i),
    fixef = c("new_id", "local_txn_date"),
    cluster = c('zip_code','local_txn_date'),
    mem.clean=TRUE
  )
  
  event_results[[name]] <- model_event
  rm(model_event)
  gc()
}


df_out_food <- data.frame()
for (j in names(event_results)){
  
  out_feglm <- summary(event_results[[j]])
  
  df_temp_log <- as.data.frame(
    coef(event_results[[j]])) %>%
    rownames_to_column() %>%
    rename(day = rowname) %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    mutate(
      std_error = out_feglm$se
    ) %>%
    rename(
      estimate = coef_event_results_j
    ) %>%
    mutate(
      lower = estimate-1.96*std_error,
      higher = estimate+1.96*std_error,
      day = str_replace(day,'e_n','-'),
      day = as.numeric(str_replace(day,'e_',''))
    ) %>%
    rbind(
      data.frame(
        day = -8,
        estimate = 0,
        std_error = 0,
        lower = 0,
        higher = 0
      )
    ) %>%
    arrange(day) %>%
    mutate(
      cluster=j
    )
  
  rm(out_feglm)
  gc()
  
  df_out_food <- rbind(df_out_food,df_temp_log)
}


write.csv(df_out_food,'results//figA4-food.csv',row.names = F)

df_out_food <- read_csv('results//figA4-food.csv')

plt_temp <- ggplot(df_out_food, aes(x=day, y=estimate,color=cluster)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="solid", color = "gray") +
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4,
                position=position_dodge(0.05)) +
  geom_line(aes(x=day, y=estimate)) +
  geom_point() +
  theme_bw() +
  ylim(-1.7,0.5) +
  xlab('day') + 
  ylab('log of daily transaction volume') + 
  scale_x_continuous(breaks = seq(-10,10,by=1)) +
  #ggtitle(paste0('impact of hurricanes on log-spending at local merchants')) +
  #ggplot2::theme_minimal() +
  theme(
    legend.position = 'top',
    axis.text.y = element_text(size=25),
    axis.title.y = element_text(size=25),
    axis.text.x = element_text(size=25),
    axis.title.x = element_text(size=25),
    text = element_text(size=25)
  ) 

file_name <- paste0('results//figA4-food.png')
ggsave(file_name,plt_temp,dpi=600,width=15,height=8) 

### Home Improvement

df_proc <-data.table::fread(
  'all_hurricanes_merchant_level_size_home_improvement_processed.csv'
)

df_proc <- df_proc %>%
  mutate(
    e_n10=ifelse(dist==-10,1,0),
    e_n9=ifelse(dist==-9,0,0),
    e_n8=ifelse(dist==-8,1,0),
    e_n7=ifelse(dist==-7,1,0),
    e_n6=ifelse(dist==-6,1,0),
    e_n5=ifelse(dist==-5,1,0),
    e_n4=ifelse(dist==-4,1,0),
    e_n3=ifelse(dist==-3,1,0),
    e_n2=ifelse(dist==-2,1,0),
    e_n1=ifelse(dist==-1,1,0),
    e_0=ifelse(dist==0,1,0),
    e_1=ifelse(dist==1,1,0),
    e_2=ifelse(dist==2,1,0),
    e_3=ifelse(dist==3,1,0),
    e_4=ifelse(dist==4,1,0),
    e_5=ifelse(dist==5,1,0),
    e_6=ifelse(dist==6,1,0),
    e_7=ifelse(dist==7,1,0),
    e_8=ifelse(dist==8,1,0),
    e_9=ifelse(dist==9,1,0),
    e_10=ifelse(dist==10,1,0)
  )

event_results <- list()

for (i in unique(df_temp$store_type)){
  
  print (i)
  
  name <- paste(i)
  
  model_event <- feglm(
    log_tot_spend ~ e_n7+e_n6+e_n5+e_n4+e_n3+e_n2+e_n1+e_0+e_1+e_2+e_3+e_4+e_5+e_6+e_7+e_8+e_9+e_10,
    data = df_temp %>% filter(store_type==i),
    fixef = c("new_id", "local_txn_date"),
    cluster = c('zip_code','local_txn_date'),
    mem.clean=TRUE
  )
  
  event_results[[name]] <- model_event
  rm(model_event)
  gc()
}


df_out_interior <- data.frame()
for (j in names(event_results)){
  
  out_feglm <- summary(event_results[[j]])
  
  df_temp_log <- as.data.frame(
    coef(event_results[[j]])) %>%
    rownames_to_column() %>%
    rename(day = rowname) %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    mutate(
      std_error = out_feglm$se
    ) %>%
    rename(
      estimate = coef_event_results_j
    ) %>%
    mutate(
      lower = estimate-1.96*std_error,
      higher = estimate+1.96*std_error,
      day = str_replace(day,'e_n','-'),
      day = as.numeric(str_replace(day,'e_',''))
    ) %>%
    rbind(
      data.frame(
        day = -8,
        estimate = 0,
        std_error = 0,
        lower = 0,
        higher = 0
      )
    ) %>%
    arrange(day) %>%
    mutate(
      cluster=j
    )
  
  rm(out_feglm)
  gc()
  
  df_out_interior <- rbind(df_out_interior,df_temp_log)
}


write.csv(df_out_interior,'results//figA4-interior.csv',row.names = F)

df_out_interior <- read_csv('results//figA4-interior.csv')

plt_temp <- ggplot(df_out_interior , aes(x=day, y=estimate,color=cluster)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="solid", color = "gray") +
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4,
                position=position_dodge(0.05)) +
  geom_line(aes(x=day, y=estimate)) +
  geom_point() +
  theme_bw() +
  ylim(-1.7,0.5) +
  xlab('day') + 
  ylab('log of daily transaction volume') + 
  scale_x_continuous(breaks = seq(-10,10,by=1)) +
  #ggtitle(paste0('impact of hurricanes on log-spending at local merchants')) +
  #ggplot2::theme_minimal() +
  theme(
    legend.position = 'top',
    axis.text.y = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.text.x = element_text(size=20),
    axis.title.x = element_text(size=20),
    text = element_text(size=20)
  ) 

file_name <- paste0('results//figA4-interior.png')
ggsave(file_name,plt_temp,dpi=600,width=15,height=8) 

### Restaurants
df_proc <-data.table::fread(
  'all_hurricanes_merchant_level_size_restaurants_processed.csv'
)

df_proc <- df_proc %>%
  mutate(
    e_n10=ifelse(dist==-10,1,0),
    e_n9=ifelse(dist==-9,0,0),
    e_n8=ifelse(dist==-8,1,0),
    e_n7=ifelse(dist==-7,1,0),
    e_n6=ifelse(dist==-6,1,0),
    e_n5=ifelse(dist==-5,1,0),
    e_n4=ifelse(dist==-4,1,0),
    e_n3=ifelse(dist==-3,1,0),
    e_n2=ifelse(dist==-2,1,0),
    e_n1=ifelse(dist==-1,1,0),
    e_0=ifelse(dist==0,1,0),
    e_1=ifelse(dist==1,1,0),
    e_2=ifelse(dist==2,1,0),
    e_3=ifelse(dist==3,1,0),
    e_4=ifelse(dist==4,1,0),
    e_5=ifelse(dist==5,1,0),
    e_6=ifelse(dist==6,1,0),
    e_7=ifelse(dist==7,1,0),
    e_8=ifelse(dist==8,1,0),
    e_9=ifelse(dist==9,1,0),
    e_10=ifelse(dist==10,1,0)
  )

event_results <- list()

for (i in unique(df_temp$store_type)){
  
  print (i)
  
  name <- paste(i)
  
  model_event <- feglm(
    log_tot_spend ~ e_n7+e_n6+e_n5+e_n4+e_n3+e_n2+e_n1+e_0+e_1+e_2+e_3+e_4+e_5+e_6+e_7+e_8+e_9+e_10,
    data = df_temp %>% filter(store_type==i),
    fixef = c("new_id", "local_txn_date"),
    cluster = c('zip_code','local_txn_date'),
    mem.clean=TRUE
  )
  
  event_results[[name]] <- model_event
  rm(model_event)
  gc()
}


df_out_rest <- data.frame()
for (j in names(event_results)){
  
  out_feglm <- summary(event_results[[j]])
  
  df_temp_log <- as.data.frame(
    coef(event_results[[j]])) %>%
    rownames_to_column() %>%
    rename(day = rowname) %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    mutate(
      std_error = out_feglm$se
    ) %>%
    rename(
      estimate = coef_event_results_j
    ) %>%
    mutate(
      lower = estimate-1.96*std_error,
      higher = estimate+1.96*std_error,
      day = str_replace(day,'e_n','-'),
      day = as.numeric(str_replace(day,'e_',''))
    ) %>%
    rbind(
      data.frame(
        day = -8,
        estimate = 0,
        std_error = 0,
        lower = 0,
        higher = 0
      )
    ) %>%
    arrange(day) %>%
    mutate(
      cluster=j
    )
  
  rm(out_feglm)
  gc()
  
  df_out_rest <- rbind(df_out_rest,df_temp_log)
}




write.csv(df_out_rest,'results//figA4-rest.csv',row.names = F)

df_out_rest <- read_csv('results//figA4-rest.csv')

plt_temp <- ggplot(df_out_rest, aes(x=day, y=estimate,color=cluster)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="solid", color = "gray") +
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4,
                position=position_dodge(0.05)) +
  geom_line(aes(x=day, y=estimate)) +
  geom_point() +
  theme_bw() +
  ylim(-1.7,0.5) +
  xlab('day') + 
  ylab('log of daily transaction volume') + 
  scale_x_continuous(breaks = seq(-10,10,by=1)) +
  #ggtitle(paste0('impact of hurricanes on log-spending at local merchants')) +
  #ggplot2::theme_minimal() +
  theme(
    legend.position = 'top',
    axis.text.y = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.text.x = element_text(size=20),
    axis.title.x = element_text(size=20),
    text = element_text(size=20)
  ) 

file_name <- paste0('results//figA4-rest.png')
ggsave(file_name,plt_temp,dpi=600,width=15,height=8) 

### Retail 

df_proc <-data.table::fread(
  'all_hurricanes_merchant_level_size_retail_processed.csv'
)

df_proc <- df_proc %>%
  mutate(
    e_n10=ifelse(dist==-10,1,0),
    e_n9=ifelse(dist==-9,0,0),
    e_n8=ifelse(dist==-8,1,0),
    e_n7=ifelse(dist==-7,1,0),
    e_n6=ifelse(dist==-6,1,0),
    e_n5=ifelse(dist==-5,1,0),
    e_n4=ifelse(dist==-4,1,0),
    e_n3=ifelse(dist==-3,1,0),
    e_n2=ifelse(dist==-2,1,0),
    e_n1=ifelse(dist==-1,1,0),
    e_0=ifelse(dist==0,1,0),
    e_1=ifelse(dist==1,1,0),
    e_2=ifelse(dist==2,1,0),
    e_3=ifelse(dist==3,1,0),
    e_4=ifelse(dist==4,1,0),
    e_5=ifelse(dist==5,1,0),
    e_6=ifelse(dist==6,1,0),
    e_7=ifelse(dist==7,1,0),
    e_8=ifelse(dist==8,1,0),
    e_9=ifelse(dist==9,1,0),
    e_10=ifelse(dist==10,1,0)
  )


event_results <- list()

for (i in unique(df_temp$store_type)){
  
  print (i)
  
  name <- paste(i)
  
  model_event <- feglm(
    log_tot_spend ~ e_n7+e_n6+e_n5+e_n4+e_n3+e_n2+e_n1+e_0+e_1+e_2+e_3+e_4+e_5+e_6+e_7+e_8+e_9+e_10,
    data = df_temp %>% filter(store_type==i),
    fixef = c("new_id", "local_txn_date"),
    cluster = c('zip_code','local_txn_date'),
    mem.clean=TRUE
  )
  
  event_results[[name]] <- model_event
  rm(model_event)
  gc()
}


df_out_retail <- data.frame()
for (j in names(event_results)){
  
  out_feglm <- summary(event_results[[j]])
  
  df_temp_log <- as.data.frame(
    coef(event_results[[j]])) %>%
    rownames_to_column() %>%
    rename(day = rowname) %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    mutate(
      std_error = out_feglm$se
    ) %>%
    rename(
      estimate = coef_event_results_j
    ) %>%
    mutate(
      lower = estimate-1.96*std_error,
      higher = estimate+1.96*std_error,
      day = str_replace(day,'e_n','-'),
      day = as.numeric(str_replace(day,'e_',''))
    ) %>%
    rbind(
      data.frame(
        day = -8,
        estimate = 0,
        std_error = 0,
        lower = 0,
        higher = 0
      )
    ) %>%
    arrange(day) %>%
    mutate(
      cluster=j
    )
  
  rm(out_feglm)
  gc()
  
  df_out_retail <- rbind(df_out_retail,df_temp_log)
}


write.csv(df_out_retail,'results//figA4-retail.csv',row.names = F)

df_out_retail <- read_csv('results//figA4-retail.csv')

plt_temp <- ggplot(df_out_retail, aes(x=day, y=estimate,color=cluster)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="solid", color = "gray") +
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4,
                position=position_dodge(0.05)) +
  geom_line(aes(x=day, y=estimate)) +
  geom_point() +
  theme_bw() +
  ylim(-1.7,0.5) +
  xlab('day') + 
  ylab('log of daily transaction volume') + 
  scale_x_continuous(breaks = seq(-10,10,by=1)) +
  #ggtitle(paste0('impact of hurricanes on log-spending at local merchants')) +
  #ggplot2::theme_minimal() +
  theme(
    legend.position = 'top',
    axis.text.y = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.text.x = element_text(size=20),
    axis.title.x = element_text(size=20),
    text = element_text(size=20)
  ) 

file_name <- paste0('results//figA4-retail.png')
ggsave(file_name,plt_temp,dpi=600,width=15,height=8) 


# Fig A5 ------------------------------------------------------------------

### Population 

df_all <-data.table::fread(
  'all_hurricanes_merchant_level_overall_population_proc.csv'
)

df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)
pre_post_results <- list()

i <- 'affected'

df_temp <- df_data_proc %>%
  filter(inside_polygon %in% c('NULL',i))

model_pre_post <- feglm(
  log_tot_spend ~ pre_base + post_base,
  data = df_temp,
  split = 'group',
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_post_results[['Pop-Low']] <- model_pre_post$Low
pre_post_results[['Pop-LowMedium']] <- model_pre_post$`Low-Medium`
pre_post_results[['Pop-HighMedium']] <- model_pre_post$`High-Medium`
pre_post_results[['Pop-High']] <- model_pre_post$High

### Age 

df_all <-data.table::fread(
  'all_hurricanes_merchant_level_overall_age_proc.csv'
)

df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)


i <- 'affected'

df_temp <- df_data_proc %>%
  filter(inside_polygon %in% c('NULL',i))

model_pre_post <- feglm(
  log_tot_spend ~ pre_base + post_base,
  data = df_temp,
  split = 'group',
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_post_results[['Age-Low']] <- model_pre_post$Low
pre_post_results[['Age-LowMedium']] <- model_pre_post$`Low-Medium`
pre_post_results[['Age-HighMedium']] <- model_pre_post$`High-Medium`
pre_post_results[['Age-High']] <- model_pre_post$High

### Income 

df_all <-data.table::fread(
  'all_hurricanes_merchant_level_overall_income_proc.csv'
)

df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)


i <- 'affected'

df_temp <- df_data_proc %>%
  filter(inside_polygon %in% c('NULL',i))

model_pre_post <- feglm(
  log_tot_spend ~ pre_base + post_base,
  data = df_temp,
  split = 'group',
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_post_results[['Income-Low']] <- model_pre_post$Low
pre_post_results[['Income-LowMedium']] <- model_pre_post$`Low-Medium`
pre_post_results[['Income-HighMedium']] <- model_pre_post$`High-Medium`
pre_post_results[['Income-High']] <- model_pre_post$High

### Education 

df_all <-data.table::fread(
  'all_hurricanes_merchant_level_overall_education_proc.csv'
)

df_data_proc$pre_base <- ifelse((df_data_proc$dist %in% c(-7,-6)) , 1, 0)
df_data_proc$post_base <- ifelse(df_data_proc$dist %in% c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10), 1, 0)


i <- 'affected'

df_temp <- df_data_proc %>%
  filter(inside_polygon %in% c('NULL',i))

model_pre_post <- feglm(
  log_tot_spend ~ pre_base + post_base,
  data = df_temp,
  split = 'group',
  fixef = c("new_id", "local_txn_date"),
  cluster = c('zip_code','local_txn_date'),
  mem.clean=TRUE
)

pre_post_results[['Education-Low']] <- model_pre_post$Low
pre_post_results[['Education-LowMedium']] <- model_pre_post$`Low-Medium`
pre_post_results[['Education-HighMedium']] <- model_pre_post$`High-Medium`
pre_post_results[['Education-High']] <- model_pre_post$High

### Consolidating

event_results <- pre_post_results
df_out_log <- data.frame()
for (j in names(event_results)){
  
  out_feglm <- summary(event_results[[j]])
  
  df_temp_log <- as.data.frame(
    coef(event_results[[j]])) %>%
    rownames_to_column() %>%
    rename(day = rowname) %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    mutate(
      std_error = out_feglm$se
    ) %>%
    rename(
      estimate = coef_event_results_j
    ) %>%
    mutate(
      lower = estimate-1.96*std_error,
      higher = estimate+1.96*std_error
    ) %>%
    mutate(
      type=j
    )
  
  gc()
  
  df_out_log <- rbind(df_out_log,df_temp_log)
}


df_out_log2 <- df_out_log %>%
  separate(type,into = c('demographic','cut'),sep = '-') %>%
  mutate(
    cut = case_when(
      cut == 'Low' ~ '1st',
      cut == 'LowMedium' ~ '2nd',
      cut == 'HighMedium' ~ '3rd',
      cut == 'High' ~ '4th'
    ),
    demographic = case_when(
      demographic == 'Pop' ~ 'Population',
      TRUE ~ demographic
    )
  ) %>%
  mutate(
    cut = factor(
      cut,
      levels = c(
        '1st','2nd','3rd','4th'
      )
    )
  )


write.csv(df_out_log2,'results//demographic_results.csv',row.names = F)

df_out_log2 <- read_csv('results//demographic_results.csv')


plt_temp <- ggplot(
  df_out_log2 %>% filter(day=='post_base'), 
  aes(x=cut, y=estimate)
) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="solid", color = "gray") +
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4,
                position=position_dodge(0.05)) +
  geom_line(aes(x=cut, y=estimate)) +
  geom_point() +
  theme_bw() +
  #ylim(-1,0.5) +
  xlab('quartile') + 
  ylab('log of daily transaction volume') + 
  #scale_x_continuous(breaks = seq(-10,20,by=1)) +
  #ggtitle(paste0('impact of hurricanes on log-spending at local merchants')) +
  #ggplot2::theme_minimal() +
  theme(
    legend.position = 'top',
    axis.text.y = element_text(size=25),
    axis.title.y = element_text(size=25),
    axis.text.x = element_text(size=25),
    axis.title.x = element_text(size=25),
    text = element_text(size=25)
  ) +
  facet_wrap(~demographic)

ggsave('results//demographic_results.png',plt_temp,dpi=600,width=15,height=8) 
