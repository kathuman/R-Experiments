# https://www.r-bloggers.com/2019/03/process-mining-part-1-3-introduction-to-bupar-package/
# https://www.r-bloggers.com/2019/04/process-mining-part-2-3-more-on-bupar-package/
# 

# library 
library(dplyr)
library(tidyverse)
library(bupaR)
library(lubridate)
theme_set(theme_light())
devtools::install_github("gertjanssenswillen/bupaR", ref = "eventlog-ordering")

#####################################################
# Preparing the data
#####################################################
# In bupaR the data needs to be adjusted from only having Case, Activity, resource and timestamp.
# it also needs:
# ACTIVITY_INSTANCE  and
# LIFECYCLE

# Load Data 
patients <- read.csv(url("https://gitlab.com/healthcare2/process-mining-tutorial/-/raw/master/ArtificialPatientTreatment.csv"))

# Create an Activity Instance
patients$ACTIVITY_INSTANCE <- 1:nrow(patients)

# Create a lifecycle as Start from Datetime
patients$LIFECYCLE <- "START"

# Adjust Timestamp
patients$DateTime <- ymd_hms(patients$DateTime)

# Create the eventlog
ptevents <- eventlog(patients,case_id = "patient",
         activity_id = "action",
         resource_id = "org.resource",
         timestamp = "DateTime", 
         activity_instance_id = "ACTIVITY_INSTANCE",
         lifecycle_id = "LIFECYCLE")

########################################################
# Analyzing the data
########################################################

# Analyzing eventlog
mapping(ptevents)
dim(ptevents)
n_activities(ptevents)
activity_labels(ptevents)
activities(ptevents)

processing_time(ptevents, #event log 
                "activity", # level of analysis, in this situation at level of activity
                units="mins") #time units to be used

processing_time(ptevents, level="log", units="days")

patients_df<- data.frame(ptevents)%>%  # convert object
  select(- .order) %>% #remove this col as we don't need it and it messes with the spread function
  spread(LIFECYCLE, DateTime) 

dim(patients_df)

################################# Part 2 ############################################
library(plyr)
library(tidyverse)
library(bupaR)
sepsis

# least common activity 
activity_frequency(sepsis, level = "activity") %>% arrange(relative)

sepsis_subset<-filter_activity_presence(sepsis, "Release E") # cases with least common activity to achieve smaller eventlog

########### Process mapping based on absolute activity instances
sepsis_subset %>% process_map(type = frequency("absolute"))

########### Process mapping based on absolute number of cases for the activity
sepsis_subset %>% process_map(type = frequency("absolute_case"))

# Which activities reoccurred consecutively in a case?
number_of_repetitions(sepsis_subset, level="activity", type="all")

# Which activities reoccurred consecutively in a case where the same resource repeated the activities?
number_of_repetitions(sepsis_subset, level="activity", type="repeat")

############################### Part 3 ##############################################
# Interruption Index

# library 
library(plyr)
library(tidyverse)
library(bupaR)
theme_set(theme_light())
n_resources(sepsis)

################ data adjustment ######################

#derive desired df 
sepsis_df<-sepsis %>% filter_resource(c("A", "B")) %>%  # filter 2 resources for our example 
  data.frame() %>% # convert event log object
  select(case_id, activity, lifecycle, resource, timestamp) %>% # select relevant variables
  drop_na() #drop na observation

# create a day identifier
sepsis_df<-sepsis_df %>% mutate(Date= as.Date(timestamp)) %>% mutate("ID_day" = group_indices_(., .dots = c("resource","Date"))) %>% # add ID_day
  group_by(ID_day) %>% arrange(ID_day, timestamp) %>% select(resource, ID_day, case_id) %>% 
  mutate(caseload = n_distinct(case_id)) %>% # caseload
  ungroup()

sepsis_df %>% head()

# We now calculate the timeblock and then the interuption index.

#remove duplicate rows
ix <- c(TRUE, rowSums(tail(sepsis_df, -1) == head(sepsis_df, -1)) != ncol(sepsis_df))
sepsis_df<-sepsis_df[ix,] 
#transpose case_id column
sepsis_df <- ddply(sepsis_df, .(ID_day), transform, idx = paste("TB", 1:length(case_id), sep = ""))  %>% spread(idx, case_id)
#calculate timeblocks
sepsis_df<-sepsis_df %>%  mutate(timeblock = rowSums(!is.na(select(.,starts_with("TB"))))) %>% select(-starts_with("TB")) # remove reduntanct TB variables 
#calculate index 
sepsis_df$interupt_index<-sepsis_df$timeblock/ sepsis_df$caseload
# sample size of index for each resource
sepsis_df<-sepsis_df %>% add_count(resource, interupt_index) 
head(sepsis_df)

# Visualizing
sepsis_df %>% ggplot(aes (interupt_index, n, size=n)) + geom_point() + facet_grid(~resource)


##############################################################
### Time Series Heatmap ######################################
library(wesanderson)

patients_df<-data.frame(patients)
levels(patients_df$handling)

## relabeling handling labels to resemble what one sees in a hospital
patients_df<-patients_df %>% mutate(handling= fct_relevel(handling, "Registration", "Triage and Assessment", "Blood test", "X-Ray", "MRI SCAN", "Discuss Results", "Check-out"))
levels(patients_df$handling)

patients_df %>% dplyr::mutate(
  time= format(time, format = "%H:%M:%S") %>% as.POSIXct(format = "%H:%M:%S"), #standardized the date for ploting
  hour= lubridate::floor_date(time, "hour")) %>% # round down time to nearest hour
  count(handling, hour) %>% # total instances of each activity at each hour
  add_count(handling, wt=n) %>% # total instances of each activity 
  mutate(percent= ((n/nn)*100)) %>% #relative freq for each activity
  ggplot(aes(hour, handling, fill=percent)) + geom_tile(size=.5, color="white") + scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 20 ,type = "continuous"))+
  theme_classic() +
  labs(x="24hour Clock", y="", title= "Peak and Lull Period of Patient Activities", subtitle= "percentage calculated is the relative frequency for a specific activity", fill="%")  + scale_y_discrete(limits = rev(levels(patients_df$handling)))+ # reverse display of y-axis varaibles 
  scale_x_datetime(date_breaks = ("1 hour"), date_labels = "%H")  #display only 24H clock values 

#########################################################################
# Other visualizations
# https://bupar.net/processmaps.html

library(bupaR)

## process maps
patients %>%
  process_map()

patients %>%
  process_map(type = frequency("relative"))

patients %>%
  process_map(performance(median, "days"))

traffic_fines %>% 
  process_map(type = custom(attribute = "amount", units = "EUR"))

## presedence matrix
## https://bupar.net/precedence.html
patients %>%
  precedence_matrix(type = "absolute",) 

patients %>%
  precedence_matrix(type = "absolute") %>%
  plot

patients %>%
  precedence_matrix(type = "relative") %>%
  plot

patients %>%
  precedence_matrix(type = "relative-antecedent") %>%
  plot

patients %>%
  precedence_matrix(type = "relative-consequent") %>%
  plot

## Social Network Analysus
## https://bupar.net/social_networks.html

patients %>%
  resource_map()

patients %>%
  resource_matrix() %>%
  plot()

## Dashboards
## https://bupar.net/processmonitR.html
library(processmonitR)

performance_dashboard(hospital)
activity_dashboard(patients)
rework_dashboard(traffic_fines)
resource_dashboard(patients)


#########################################################################
## Example 2
## https://medium.com/process-mining-and-analytics/process-mining-in-10-minutes-with-r-1ab28ed74e81

