#### ACS Data Settings ####

## ============= Using the default ACS variables list ========================== ####

source('GeocoderACMT.R')

## ===== Select ACS variables from list ======================================= ####

#create list of variables
#default list: 2017
#ACSInputList<-load_variables(2017, 'acs5')

#set ACS variable list
##set up data pull specs:

get_default_acs_var_list<-function(year=year){

  #import ACS list based on the year
  if(year==2010){
      acs_columns<-read.csv('ACMT/ACSColumns2010.csv')
    }
  if(year==2011){
    acs_columns<-read.csv('ACMT/ACSColumns2011.csv')
  }
  if(year>2011 & year<2019){
    acs_columns<-read.csv('ACMT/ACSColumns2012_thru_18.csv')
  }
  if(year==2019){
    acs_columns <- read.csv("ACMT/ACSColumns2019.csv")
  }
  if(year==2020){
    acs_columns<-read.csv('ACMT/ACSColumns2020.csv')
  }

  ##create 'count' versions of each variable name and 'proportion' versions for each #ACS variable where applicable
  acs_count_names<-paste(acs_columns$var_name, "count", sep="_")
  if (length(acs_columns$var_name[acs_columns$universe_col != ""]) == 0) {   # prevent having something that is exactly "_proportion"
    acs_proportion_names <- character(0)
  } else {
    acs_proportion_names <- paste(acs_columns$var_name[acs_columns$universe_col !=''], "proportion", sep="_")   # only non-universal variables have proportions
  }
  
  #save file as ACSColumns file
  write.csv(acs_columns, '~/workspace/ACMT/ACSColumns.csv')
  
  names_of_variables_to_get<-c(acs_count_names, acs_proportion_names)
  return(names_of_variables_to_get)
  
}

#update ACSColumns.csv file with uploaded csv
update_acs_var_list<-function(acs_csv){
  
  acs_columns<-read.csv(acs_csv)
  acs_columns<-acs_columns %>% rename(acs_variable_name_to_interpolate_by_sum_boolean_mapping=interpolation)
  write.csv(acs_columns, 'ACMT/ACSColumns.csv')
  
}

#update ACS variables list
update_acs_columns<-function()

acs_columns<-read.csv('~/workspace/ACMT/ACSColumns.csv')  


#Designate interpolation strategy for variables
acs_variable_name_to_interpolate_by_sum_boolean_mapping<-acs_columns$acs_variable_name_to_interpolate_by_sum_boolean_mapping
names(acs_variable_name_to_interpolate_by_sum_boolean_mapping)<-acs_columns$acs_col

#Set the list of variable codes, the list of variable names, the radius, and the year for the data you want pulled
codes_of_acs_variables_to_get<-acs_columns_inspace$acs_col
names_of_variables_to_get<-c(acs_count_names, acs_proportion_names)

acs_vars<-names_of_variables_to_get

#}

update_acs_variables(acs_columns)

acs_years<-c(2014, 2015, 2016, 2017, 2018, 2019, 2020)
acs_selected_years<-c(2017)

acs_description<-'The American Community Survey (ACS) is a national survey that provides sociodemographic information. In the 5-year estimates, data is pooled across 5 years of surveying to minimize measurement error.
We have generated a list of ACS variables to be pulled for each participant in Inspace across three buffered areas (500m, 1000m, and 5000m). 
ACS variables will be pulled for 2017 (2013-2017 5-year ACS) as well as for the 5-year period centered around your initial year of enrollment.'
