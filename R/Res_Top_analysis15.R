
# true_value from simulation file
# path input here line 16 from the simulation file

####------------------------common input -------------------------####

# no group cycle
# awlaws six parameters. if less, will probme

Cat_value <- 9
method_ana <- 'FOCE'     # transfer to the subgroup.r file

# target_group <- 1
time_group_spcified <- 'TM_8'
 

# it could be FOCE_PRO, MATH
# input
# method_ana will use in source file
# code to use methods_ana:   method_ana,'_', str_pad(simu_ID,3,pad=0),'.lst'
# output:  carry method_ana



top_df <-read_excel('..\\top_results_condensed2.xlsx')  %>% 
                 filter(Cat==Cat_value) %>%
                select(-1)	


#> only keep the target group if target group exist
df_target_group <- top_df 

if (exists('target_group')) {
  top_df_Group <- top_df %>% filter(Group %in% target_group) } else {                                                                 target_group <- 1  }   
 
trial_quantity <- 50               # common in both file




####------------------------make empty df ---------------------- ####

para_df <- df_target_group %>% select(-starts_with('TM')) %>% 
                               select( -contains('KA')) %>% 
                               select(-Group) %>% 
                               select(-Cat)  %>% 
                               select(-Results) 

if (para_df$TV_CL[1]==0) { para_df <- para_df %>% select(-one_of(c('TV_CL','IIV_CL'))) }
  
para_names <- para_df %>% select(-starts_with('Time_mark')) %>% names()
theta_name <- para_df %>% select(starts_with('TV')) %>%  names()

pre        <- data.frame(matrix(ncol = length(para_names)+3, nrow = 0))  ## 2 column lableing
names(pre) <- c('Group','TM', 'fileName',para_names)


####---------------------------same Cat analyse every group ---------------------####

for ( Group_No in df_target_group$Group)   {
  
        true_value <- df_target_group %>% slice(Group_No) %>% 
                                           select( one_of(para_names) ) %>% 
                                           unlist(.[1,])
         #> TM_5,TM_6
        time_names <- df_target_group %>%  select(starts_with('TM')) %>% 
                                            select_if(!(is.na(.))) %>% 
                                            select_if(str_detect(.,',')) %>% 
                                            names()
        
        
####------------------------------time group--------------------------------------------#####     
        if (exists('time_group_spcified'))  {
          time_group <- time_group_spcified     
          path <- paste0('NM_run\\Cat_',Cat_value,'\\group_',Group_No,'\\',time_group)
          if ( file.exists(path) ) {   
            source('Res_analysis_subgroup14.R')  # run the code; write IMP_TM_5_full.csv
          }      
          
          } else {  
        
        
        for (time_group in time_names)      {         # save reuslts under the group name
          
             path <- paste0('NM_run\\Cat_',Cat_value,'\\group_',Group_No,'\\',time_group)
     if ( file.exists(path) ) {   
              source('Res_analysis_subgroup15.R')  # run the code; write IMP_TM_5_full.csv
            }                                               # file.exists(path)
        } # for (time_group
    }  # exists('time_group_spcified
  rm(true_value)
}


   
# #####----------------------- summarise all tables to one table------------------------#########
# options(scipen = 5) 

all_file <-   paste0('NM_run\\Cat_',Cat_value,'\\group_1\\*.csv') %>% 
                   Sys.glob(.) %>%        grep('TM', ., value = TRUE)


for (i in all_file )  {
  
 contents <- i %>% read_csv() %>% filter(fileName %in% c('mean_true_bias (%)','RSE (%)'))  
 contents <- contents %>% mutate_at(.,vars(one_of(para_names2)),funs( as.numeric(.))  ) %>% 
             mutate_if(is.numeric,funs( round(.,2)) )
 
 contents$Time_mark <- Sys.time()
 contents[nrow(contents)+1,] <- NA

 file_to_write <- paste0(dirname(path),'/',method_ana,'_all_Time.csv')

  if (file.exists(file_to_write))    {
    
    data.table::fwrite(contents,file_to_write,append = T,na='',dateTimeAs=c("write.csv") )
          } else {  contents[2:3,] <- contents[1:2,]
                contents[1,] <- NA
                contents[4,] <- NA
          data.table::fwrite (contents, file_to_write, na='', dateTimeAs=c("write.csv") )  
                          }
}

shell.exec(paste0('NM_run\\Cat_',Cat_value,'\\group_',target_group) )