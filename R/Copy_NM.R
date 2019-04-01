
# copy_NM()
copy_NM <-  function(template_mod_path,DV_place )  {  
  
  for (Group_No in unique(top_df_Group))   { 
    
    df_target_group0 <- top_df %>% filter(Group==Group_No) 
    temp_file <- readLines( paste0(template_mod_path,'\\FOCE_',DV_place,'xxx.mod'))

    if (nrow(df_target_group0)>1) { 
      warning('multiple group ture values,will select the last row ') } # chech group has multiple rows
    
    # notice to change  
    time_list <-  df_target_group0 %>% filter(row_number()==n()) %>% 
      select(starts_with('TM')) %>% 
      select_if(!(is.na(.))) %>% 
      select_if(is.character)   %>% 
      select_if(str_detect(.,',')) %>% 
      names()
 
    df_target_group   <- top_df %>% filter(Group==Group_No)  %>% 
      filter(row_number()==n())  %>% 
      select(one_of(para_name))  
    
    if (exists('target_TM'))  { time_list2 <-time_list[ time_list %in% target_TM] 
    } else {  time_list2 <-time_list  }
    
    #> Loop TM5, TM6
    for (time_group in time_list2) {
      path <- paste0('NM_run\\Cat_',Cat_value,'\\group_',Group_No,'\\',time_group)
      
      for ( simu_ID in 1:trial_quantity )    {
        Loop_number <- str_pad(simu_ID,3,pad=0)
        new_name <-  paste0(template_mod_path,'_',DV_place,as.character(Loop_number),'.mod')
        new_core_name <-  new_name %>% str_sub(.,1,-5) 
        
        temp_file2 <- temp_file %>% 
          str_replace('dataset.csv', paste0('trial_conc_',Loop_number,'.csv') ) %>% # name match
          str_replace('output_pirana', new_core_name) %>%                           # name
          str_replace('output_csv', paste0(new_core_name,'.csv'))                  # name
        
        #> give random inial value /2 or 2           
      random_VMAX <-round(runif(1,min=df_target_group$TV_VMAX[1]/4,max=df_target_group$TV_VMAX[1]*2),2)
        random_KM <- round(runif(1,min=df_target_group$TV_KM[1]/4,  max=df_target_group$TV_KM[1]*2) ,2)
        random_V2 <- round(runif(1,min=df_target_group$TV_V2[1]*0.5,max=df_target_group$TV_V2[1]*1.5),2)
        random_V3 <- round(runif(1,min=df_target_group$TV_V3[1]*0.5,max=df_target_group$TV_V3[1]*1.5),2)
        random_Q <-  round(runif(1,min=df_target_group$TV_Q[1]*0.5, max=df_target_group$TV_Q[1]*1.5) ,2)
        random_CL <- round(runif(1,min=df_target_group$TV_CL[1]*0.5,max=df_target_group$TV_CL[1]*1.5),2)
        
        temp_file3 <- temp_file2 %>% 
          str_replace('.+VMAX_initial', paste0('(0,',random_VMAX,') ;VMAX_NEW') ) %>% 
          str_replace('.+KM_initial',   paste0('(0,',random_KM,')   ;KM_NEW')  ) %>% 
          str_replace('.+V2_initial',   paste0('(0,',random_V2,')   ;V2_NEW')  ) %>% 
          str_replace('.+V3_initial',   paste0('(0,',random_V3,')   ;V3_NEW')  ) %>% 
          str_replace('.+Q_initial',    paste0('(0,',random_Q,')    ;Q_NEW')   ) %>% 
          str_replace('.+CL_initial',   paste0('(0,',random_CL,')   ;CL_NEW') ) 
        
        if (!file.exists(path)) { warning('Sibo:folder not exist') ; dir.create(path) } 
        
        write(temp_file3, paste0(path,'\\',new_name))
      }   
    }
  }
}

# random_KM <- 0.02

#copy_NM_KM_zero2()

copy_NM_KM_02 <-  function(template_mod_path,DV_place )  {  
  
  for (Group_No in unique(top_df_Group))   { 
    df_target_group0 <- top_df %>% filter(Group==Group_No) 
      temp_file <-readLines(paste0(template_mod_path,'\\FOCE_',DV_place,'xxx.mod')) 
    
    if (nrow(df_target_group0)>1) { 
      warning('multiple group ture values,will select the last row ') } # chech group has multiple rows
  
    # notice to change  
    time_list <-  df_target_group0 %>% filter(row_number()==n()) %>% 
      select(starts_with('TM')) %>% 
      select_if(!(is.na(.))) %>% 
      select_if(is.character)   %>% 
      select_if(str_detect(.,',')) %>% 
      names()
     # 
    df_target_group   <- top_df %>% filter(Group==Group_No)  %>% 
      filter(row_number()==n())  %>% 
      select(one_of(para_name))  
    
    if (exists('target_TM'))  { time_list2 <-time_list[ time_list %in% target_TM] 
    } else {  time_list2 <-time_list  }
    
    #> Loop TM5, TM6
    for (time_group in time_list2) {
      path <- paste0('NM_run\\Cat_',Cat_value,'\\group_',Group_No,'\\',time_group)
      
      for ( simu_ID in 1:trial_quantity )    {
        Loop_number <- str_pad(simu_ID,3,pad=0)
        new_name <-  paste0(template_mod_path,'_',DV_place,as.character(Loop_number),'.mod')
        new_core_name <-  new_name %>% str_sub(.,1,-5) 
        
        temp_file2 <- temp_file %>% 
          str_replace('dataset.csv', paste0('trial_conc_',Loop_number,'.csv') ) %>% # name match
          str_replace('output_pirana', new_core_name) %>%                           # name
          str_replace('output_csv', paste0(new_core_name,'.csv'))                  # name
        
        #> give random inial value /2 or 2           
        random_VMAX <-round(runif(1,min=df_target_group$TV_VMAX[1]/4,max=df_target_group$TV_VMAX[1]*2),2)
        random_KM <- 0.02
        random_V2 <- round(runif(1,min=df_target_group$TV_V2[1]*0.5,max=df_target_group$TV_V2[1]*1.5) ,2)
        random_V3 <- round(runif(1,min=df_target_group$TV_V3[1]*0.5,max=df_target_group$TV_V3[1]*1.5) ,2)
        random_Q <-  round(runif(1,min=df_target_group$TV_Q[1]*0.5, max=df_target_group$TV_Q[1]*1.5)  ,2)
        random_CL <- round(runif(1,min=df_target_group$TV_CL[1]*0.5,max=df_target_group$TV_CL[1]*1.5) ,2)
        
        temp_file3 <- temp_file2 %>% 
          str_replace('.+VMAX_initial', paste0('(0,',random_VMAX,') ;VMAX_NEW') ) %>% 
          str_replace('.+KM_initial',   paste0(  '(',  random_KM,  ') FIX ;KM_NEW')  ) %>% 
          str_replace('.+V2_initial',   paste0('(0,',random_V2,')   ;V2_NEW')  ) %>% 
          str_replace('.+V3_initial',   paste0('(0,',random_V3,')   ;V3_NEW')  ) %>% 
          str_replace('.+Q_initial',    paste0('(0,',random_Q,')    ;Q_NEW')   ) %>% 
          str_replace('.+CL_initial',   paste0('(0,',random_CL,')   ;CL_NEW') ) 
        
        if (!file.exists(path)) { warning('Sibo:folder not exist') ; dir.create(path) } 
        
        write(temp_file3, paste0(path,'\\',new_name))
      }   
    }
  }
}





####--------------------------- Linear CL to zero -----------------------------------------#####

#copy_NM_CL_zero()

copy_NM_CL_zero <-  function(template_mod_path,DV_place )  {  
  
  for (Group_No in unique(top_df_Group))   { 
    df_target_group0 <- top_df %>% filter(Group==Group_No) 
    temp_file <-readLines(paste0(template_mod_path,'\\FOCE_',DV_place,'xxx.mod')) 
    
    if (nrow(df_target_group0)>1) { 
      warning('multiple group ture values,will select the last row ') } # chech group has multiple rows
    
    # notice to change  
    time_list <-  df_target_group0 %>% filter(row_number()==n()) %>% 
      select(starts_with('TM')) %>% 
      select_if(!(is.na(.))) %>% 
      select_if(is.character)   %>% 
      select_if(str_detect(.,',')) %>%     names()

      df_target_group   <- top_df %>% filter(Group==Group_No)  %>% 
                                filter(row_number()==n())  %>% 
                                select(one_of(para_name))  
    
    if (exists('target_TM'))  { time_list2 <-time_list[ time_list %in% target_TM] 
                       } else {  time_list2 <-time_list  }
    
    #> Loop TM5, TM6
    for (time_group in time_list2) {
      path <- paste0('NM_run\\Cat_',Cat_value,'\\group_',Group_No,'\\',time_group)
      
      for ( simu_ID in 1:trial_quantity )    {
        Loop_number <- str_pad(simu_ID,3,pad=0)
        new_name <-  paste0(template_mod_path,'_',DV_place,as.character(Loop_number),'.mod')
        new_core_name <-  new_name %>% str_sub(.,1,-5) 
        
        temp_file2 <- temp_file %>% 
          str_replace('dataset.csv', paste0('trial_conc_',Loop_number,'.csv') ) %>% # name match
          str_replace('output_pirana', new_core_name) %>%                           # name
          str_replace('output_csv', paste0(new_core_name,'.csv'))                  # name
        
        #> give random inial value /2 or 2           
        random_VMAX <-round(runif(1,min=df_target_group$TV_VMAX[1]/4,max=df_target_group$TV_VMAX[1]*2),2)
        random_KM <-  round(runif(1,min=df_target_group$TV_KM[1]/4,  max=df_target_group$TV_KM[1]*2),2)
        random_V2 <-  round(runif(1,min=df_target_group$TV_V2[1]*0.5,max=df_target_group$TV_V2[1]*1.5) ,2)
        random_V3 <-  round(runif(1,min=df_target_group$TV_V3[1]*0.5,max=df_target_group$TV_V3[1]*1.5) ,2)
        random_Q <-   round(runif(1,min=df_target_group$TV_Q[1]*0.5, max=df_target_group$TV_Q[1]*1.5)  ,2)
        random_CL <-  0
        
        temp_file3 <- temp_file2 %>% 
          str_replace('.+VMAX_initial', paste0('(0,',random_VMAX,    ') ;VMAX_NEW') ) %>% 
          str_replace('.+KM_initial',   paste0(  '(0,',  random_KM,     ')   ;KM_NEW')  ) %>% 
          str_replace('.+V2_initial',   paste0('(0,',random_V2,    ')   ;V2_NEW')  ) %>% 
          str_replace('.+V3_initial',   paste0('(0,',random_V3,    ')   ;V3_NEW')  ) %>% 
          str_replace('.+Q_initial',    paste0('(0,',random_Q,   ')    ;Q_NEW')   ) %>% 
          str_replace('.+CL_initial',   paste0('(',random_CL,   ') FIX  ;CL_NEW') ) 
		  
         temp_file4 <- temp_file3 %>% 
          str_replace('.+IIV_CL',   '0.0001 FIX  ; IIV_CL') 
              
        if (!file.exists(path)) { warning('Sibo:folder not exist') ; dir.create(path) } 
        
        write(temp_file4, paste0(path,'\\',new_name))
      }   
    }
  }
}






###-----------------------------------------------------------------#############

# NO MM

copy_NM_VMAX_zero <-  function(template_mod_path,DV_place )  {  
  
  for (Group_No in unique(top_df_Group))   { 
    df_target_group0 <- top_df %>% filter(Group==Group_No) 
    temp_file <- readLines(paste0(template_mod_path,'\\FOCE_',DV_place,'xxx.mod')) 
    
    if (nrow(df_target_group0)>1) { 
      warning('multiple group ture values,will select the last row ') } # chech group has multiple rows
    
    # notice to change  
    time_list <-  df_target_group0 %>% filter(row_number()==n()) %>% 
      select(starts_with('TM')) %>% 
      select_if(!(is.na(.))) %>% 
      select_if(is.character)   %>% 
      select_if(str_detect(.,',')) %>%     names()

      df_target_group   <- top_df %>% filter(Group==Group_No)  %>% 
                                filter(row_number()==n())  %>% 
                                select(one_of(para_name))  
    
    if (exists('target_TM'))  { time_list2 <-time_list[ time_list %in% target_TM] 
                       } else {  time_list2 <-time_list  }
    
    #> Loop TM5, TM6
    for (time_group in time_list2) {
      path <- paste0('NM_run\\Cat_',Cat_value,'\\group_',Group_No,'\\',time_group)
      
      for ( simu_ID in 1:trial_quantity )    {
        Loop_number <- str_pad(simu_ID,3,pad=0)
        new_name <-  paste0(template_mod_path,'_',DV_place,as.character(Loop_number),'.mod')
        new_core_name <-  new_name %>% str_sub(.,1,-5) 
        
        temp_file2 <- temp_file %>% 
          str_replace('dataset.csv', paste0('trial_conc_',Loop_number,'.csv') ) %>% # name match
          str_replace('output_pirana', new_core_name) %>%                           # name
          str_replace('output_csv', paste0(new_core_name,'.csv'))                  # name
        
        #> give random inial value /2 or 2           
        random_VMAX <- 0
        random_KM <-  1
        random_V2 <-  round(runif(1,min=df_target_group$TV_V2[1]*0.5,max=df_target_group$TV_V2[1]*1.5) ,2)
        random_V3 <-  round(runif(1,min=df_target_group$TV_V3[1]*0.5,max=df_target_group$TV_V3[1]*1.5) ,2)
        random_Q <-   round(runif(1,min=df_target_group$TV_Q[1]*0.5, max=df_target_group$TV_Q[1]*1.5)  ,2)
        random_CL <- round(runif(1,min=df_target_group$TV_CL[1]*0.5,max=df_target_group$TV_CL[1]*1.5),2)
		
		
        temp_file3 <- temp_file2 %>% 
          str_replace('.+VMAX_initial', paste0('(',random_VMAX,    ')  FIX ;VMAX_NEW') ) %>% 
          str_replace('.+KM_initial',   paste0('(',  random_KM,     ')  FIX ;KM_NEW')  ) %>% 
          str_replace('.+V2_initial',   paste0('(0,',random_V2,   ')   ;V2_NEW')  ) %>% 
          str_replace('.+V3_initial',   paste0('(0,',random_V3,   ')   ;V3_NEW')  ) %>% 
          str_replace('.+Q_initial',    paste0('(0,',random_Q,    ')    ;Q_NEW')   ) %>% 
          str_replace('.+CL_initial',   paste0('(0,',random_CL,   ')   ;CL_NEW') ) 
        
        temp_file4 <- temp_file3  
        
        if (!file.exists(path)) { warning('Sibo:folder not exist') ; dir.create(path) } 
        
        write(temp_file4, paste0(path,'\\',new_name))
      }   
    }
  }
}



























