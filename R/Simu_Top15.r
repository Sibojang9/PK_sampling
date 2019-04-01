# every cat the same ture valude 

####-------------------------------the sam   Gae-----------------------####

#> read cat itself
# # target_group <- 1  # always lat group

Cat_value <- 9


rm(target_TM) 
 # target_TM <- 'TM_13'


top_df <-read_excel('..\\top_results_condensed2.xlsx') %>% 
                   filter(Cat==Cat_value) %>%  
                   select(-one_of(c('Time_mark','Cat')))	

para_name <- c(str_subset(names(top_df) ,'TV_'), 
               str_subset(names(top_df) ,'IIV_'), 
               str_subset(names(top_df) ,'RES_') 
              )


sub_simu_file <- 'simu_subgroup12.R'  # file this source

source('M6_LLOQ4.R')
source('simu_functions2.R')

trial_quantity    <- 50
subject_quantity  <- 24   
dose_group        <- c(100,500,1000,2000)
duration          <- 1/24        # for RXode,NONMEM dose not need. for every goupd


if (!((subject_quantity/length(dose_group))%%1==0)) {stop("ID number wrong")}# ID number dose times N


# LLOQ definded in the middling
# if (sqrt(res_error[2]) < 0.05)  { LLOQ <- 0.01 } else {  LLOQ <- 0.01  }


top_df_Group <- top_df %>% .$Group

#> only keep the target group if there is a target

    if (  exists('target_group'))   {
       top_df_Group <- top_df$Group[top_df$Group %in% target_group]
       top_df_Group <- unique(top_df_Group)
      }       


##---------model --------------------##
ode <-  '
CP = A2/V2                                                        ;
d/dt(A1)  = 0                                                    ;
d/dt(A2)  =  -Q/V2*A2 + Q/V3*A3 - CL/V2*A2 - VMAX*CP/(KM+CP) ;
d/dt(A3)  =   Q/V2*A2 - Q/V3*A3                              ;
'
mod1 <- RxODE(model = ode, modName = "mod1")  

#  1: loop groups.  2: loop time. purri

dir.create(paste0('NM_run\\Cat_',Cat_value))
#--------------------------------------- OVER -----------------------##



####----------------------------loop group simulation data ------------------------####

#>top_df_Group screen by the target group

for ( Group_No in unique(top_df_Group))   {
  
  # creat different subfolder
  path_group  <- paste0('NM_run\\Cat_',Cat_value,'\\group_',Group_No)
  
  df_targe_group0   <- top_df %>% filter(Group==Group_No) #> multiple group=1
  
  if (nrow(df_targe_group0)>1) { 
    warning('multiple group ture values,will select the last row ') } # chech group has multiple rows
  
  df_targe_group <- df_targe_group0 %>% filter(row_number()==n()) 
  
  rm(true_value)
  
  true_value <- df_targe_group %>% select(one_of(para_name))  
  
  message('True Value')
  print(select(true_value,starts_with('TV_')))
  print(select(true_value,starts_with('IIV_')))
  print(select(true_value,starts_with('RES_')))
  
  dir.create(path_group)
  
  time_list <- df_targe_group %>% filter(Group== Group_No) %>% 
                               select(starts_with('TM')) %>% 
                               select_if(~!all(is.na(.)))   %>% 
                                    as.list()%>% 
                                    map(chat2number)
  
  
  if (exists('target_TM'))  { time_list <-  time_list[ names(time_list) %in% target_TM] }

  # algorithm select all sampling point. then every TM select the subgroup
  all_sampling_points <-  time_list %>% unlist() %>% sort() %>% unique()
  
  
  theta <-     true_value %>%  select( starts_with('TV'))  %>% unlist(.[1,])
  eta   <-     true_value %>%  select( starts_with('IIV')) %>% unlist(.[1,])
  res_error <- true_value %>%  select( starts_with('RES')) %>% unlist(.[1,])
  
  
  if (sqrt(res_error[2]) < 0.05)  { LLOQ <- 0.01 } else {  LLOQ <- 0.05  }
  
  theta_naked_name <- str_replace(names(theta), 'TV_', '') # no prefix
  eta_naked_name <- str_replace(names(eta), 'IIV_', '') # no prefix

  if ( !identical(theta_naked_name,eta_naked_name)  )  {
    print('Input Parameters Missing') ; stop()}
  
####simu_subgroup7.R will read every TM/5/6/...10  in the time_list.(including all TMs)####
  for ( trial_ID in 1:trial_quantity )   {  source(sub_simu_file) }
  

}
 



####----------------------- independent of the loop-----------------#####
 
 source('Copy_NM.R')

# Cat 1.  Km fixed to 0.02
if (Cat_value %in% c(1))        {  print( 'Km fixed to 0.02' )  ; copy_NM_KM_02('FOCE','')    }

# Cat 2/3/6.  CL fixed to 0
if (Cat_value %in% c(2,3,6)  )  { print( 'Clinear CL fixed to 0');  copy_NM_CL_zero('FOCE','')  }

# Cat 4/5/7/8  Nornal
if (Cat_value %in% c(4,5,7,8))  { print( 'Normal');    copy_NM('FOCE','')        }

if (Cat_value %in% c(9))  { print( 'only clearance');    copy_NM_VMAX_zero('FOCE','')        }



 ## complete linear model not here. change the model structure.
