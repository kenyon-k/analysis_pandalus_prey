################################################################################
################################################################################
#                   Figure 4 Bar Samples per Zone - FINAL
################################################################################
################################################################################

# Author: Krista Kenyon (KAK)
# Date Created: May 10/2024
# Date Last Modified: June 14/2024 by KAK

# Document Purpose: Working document for creating the Figure 4 grouped bar chart.
    # Working in code is simpler and faster here than in the R Markdown document
        # R Markdown places code results below chunks, which can mean a lot of scrolling
        # Not worried about accidentally knitting the whole document when testing out different code chunks
    # Current version is up to date clean code.

# Document Sections:
    # Remaining Tasks
    # Library's to Load
    # Chunk A: Importing and Prepping the Data
        # copy of R Markdown Chunk 2: import data
        # no modifications were made to this chunk
    # Chunk B: Creating the Grouped Bar Figure
        # code transferred into the R Markdown Chunk 4: samples-zone
    # Additional Notes


# To see tutorials, key webpages, and key videos I used to create and format the 
# bar chart, please see the 'Draft_Script_Fig04_Bar-sample-region.R' in the 'Draft Code' folder


################################################################################
######################### Remaining Tasks ######################################
################################################################################

# cleaning up the code


################################################################################
####################### Library's to Load ######################################
################################################################################
library(ggplot2) 
library(tidyverse)
library(dplyr)
library(reshape2) 
# library(ggrepel)

################################################################################
################### Importing and Prepping the Data ########################
###############################################################################

# This code chunk:
    # imports the data
    # manipulates the data it to create the base prey dataframe
    # manipulates the data it to create the base predator dataframe
    # checks & saves the dataframe structures

# The data imported must be structured consistently in order to utilize this coding.

# All code to check dataframe structure are commented out. If not they will produce outputs into the report.


############################ Key Data Structure Assumption #####################

# It is assumed that the imported data will initially have a structure consistent to the example saved immediately below. Particularly the variable names and variable data type (e.g. chr, int, num)

### Example initial imported dataset structure 

# 'data.frame':	1236 obs. of  19 variables:
# $ ContentKey        : chr  "62114003_892_3_1_-14_1733" "62114003_892_3_1_-15_1732" "62114003_892_3_1_-16_1728" "62114003_892_3_1_-17_1729" ...
# $ Prey_OS_ID        : int  6967 9998 6967 6967 6967 6967 8020 8020 4950 8530 ...
# $ PreyWt            : num  0.3 NA 0.286 0.091 0.079 ...
# $ Prey_Count        : int  NA NA NA NA NA NA NA NA NA NA ...
# $ OS_ID             : int  892 892 892 892 892 892 892 892 892 892 ...
# $ Length            : num  24.5 20 20 20.5 17 14 37.5 27 27 33 ...
# $ Trawl_ID          : int  62114003 62114003 62114003 62114003 62114003 62114003 62114003 62114003 62114003 62114003 ...
# $ Year              : int  2019 2019 2019 2019 2019 2019 2019 2019 2019 2019 ...
# $ Start.Depth       : int  269 269 269 269 269 269 269 269 269 269 ...
# $ End.Depth         : int  271 271 271 271 271 271 271 271 271 271 ...
# $ Depth.Range       : chr  "201-300" "201-300" "201-300" "201-300" ...
# $ Study.Area        : chr  "2G" "2G" "2G" "2G" ...
# $ Scientific.Name   : chr  "REINHARDTIUS HIPPOGLOSSOIDES" "REINHARDTIUS HIPPOGLOSSOIDES" "REINHARDTIUS HIPPOGLOSSOIDES" "REINHARDTIUS HIPPOGLOSSOIDES" ...
# $ CommonName        : chr  "GREENLAND HALIBUT, TURBOT" "GREENLAND HALIBUT, TURBOT" "GREENLAND HALIBUT, TURBOT" "GREENLAND HALIBUT, TURBOT" ...
# $ Start.Lat.Degree  : int  57 57 57 57 57 57 57 57 57 57 ...
# $ Start.Lat.Minutes : num  45.1 45.1 45.1 45.1 45.1 ...
# $ Start.Long.Degree : int  60 60 60 60 60 60 60 60 60 60 ...
# $ Start.Long.Minutes: num  24.2 24.2 24.2 24.2 24.2 ...
# $ FishKey           : chr  "62114003_892_3_1_-14" "62114003_892_3_1_-15" "62114003_892_3_1_-16" "62114003_892_3_1_-17" ...


####################### Importing Your Data ####################################


### import data

prey <- read.csv('data/raw/2019_StomachContent_test.csv') 
    # assumes header=TRUE and knows it's separated by commas
# head(prey)

# str(prey)
    # check initial data structure. Does it match example?


########################## Creating Base Prey Dataframe ########################

# 'prey' dataframe: each row is a new prey item per sampled stomachs

# Dataframe transformations:
    # add column with shrimp fishing regions (EAZ, WAZ, SFA4)
    # add column with predator 'category' names (Atlantic code, Greenland halibut, Redfish, Skate)

# Finish with checking and storing dataframe structure


 
### Adding 'region' category (WAZ, EAZ, SFA4) 

prey$region = prey$Study.Area  
    # creates new column that is duplicate of sa.code, and renames to pred. name

prey <- prey %>% 
  mutate(region=recode(region, "RISA"="EAZ", "SFA2EX"="EAZ", "SFA3" ="WAZ","2G"="SFA4"))
    # renaming values in the prey$region column
 


### Adding predator category

prey$pred.name = prey$OS_ID  
    # creates new column that is duplicate of pred.sp, and renames to pred. name

prey <- prey %>%
  mutate(pred.name=recode(pred.name, "892" = "Greenland halibut",
                          "438" = "Atlantic cod",
                          "792" = "Redfish", "793" = "Redfish", "794" = "Redfish",
                          "997" = "Redfish", "998" = "Redfish",
                          "80" = "Skate", "88" = "Skate", "89" = "Skate",
                          "90" = "Skate", "91" = "Skate", "92" = "Skate",
                          "94" = "Skate", "95" = "Skate", "96" = "Skate",
                          "97" = "Skate"))
    # replaces species codes with predatory categories in new column. 
    # See 'Predator Codes and Names.txt' file in 'Scripts' folder for list of species names attached to each code



### Checking our Base Prey Dataframe Modifications

# which(is.na(prey))
    # checking if there are blank values or NA's in the dataframe

str(prey) 
    # checking new dataframe structure to ensure all updates are good


### Saving Structure of Base 2019 Dataframe

# 'data.frame':	1236 obs. of  21 variables:
# $ ContentKey        : chr  "62114003_892_3_1_-14_1733" "62114003_892_3_1_-15_1732" "62114003_892_3_1_-16_1728" "62114003_892_3_1_-17_1729" ...
# $ Prey_OS_ID        : int  6967 9998 6967 6967 6967 6967 8020 8020 4950 8530 ...
# $ PreyWt            : num  0.3 NA 0.286 0.091 0.079 ...
# $ Prey_Count        : int  NA NA NA NA NA NA NA NA NA NA ...
# $ OS_ID             : int  892 892 892 892 892 892 892 892 892 892 ...
# $ Length            : num  24.5 20 20 20.5 17 14 37.5 27 27 33 ...
# $ Trawl_ID          : int  62114003 62114003 62114003 62114003 62114003 62114003 62114003 62114003 62114003 62114003 ...
# $ Year              : int  2019 2019 2019 2019 2019 2019 2019 2019 2019 2019 ...
# $ Start.Depth       : int  269 269 269 269 269 269 269 269 269 269 ...
# $ End.Depth         : int  271 271 271 271 271 271 271 271 271 271 ...
# $ Depth.Range       : chr  "201-300" "201-300" "201-300" "201-300" ...
# $ Study.Area        : chr  "2G" "2G" "2G" "2G" ...
# $ Scientific.Name   : chr  "REINHARDTIUS HIPPOGLOSSOIDES" "REINHARDTIUS HIPPOGLOSSOIDES" "REINHARDTIUS HIPPOGLOSSOIDES" "REINHARDTIUS HIPPOGLOSSOIDES" ...
# $ CommonName        : chr  "GREENLAND HALIBUT, TURBOT" "GREENLAND HALIBUT, TURBOT" "GREENLAND HALIBUT, TURBOT" "GREENLAND HALIBUT, TURBOT" ...
# $ Start.Lat.Degree  : int  57 57 57 57 57 57 57 57 57 57 ...
# $ Start.Lat.Minutes : num  45.1 45.1 45.1 45.1 45.1 ...
# $ Start.Long.Degree : int  60 60 60 60 60 60 60 60 60 60 ...
# $ Start.Long.Minutes: num  24.2 24.2 24.2 24.2 24.2 ...
# $ FishKey           : chr  "62114003_892_3_1_-14" "62114003_892_3_1_-15" "62114003_892_3_1_-16" "62114003_892_3_1_-17" ...
# $ region            : chr  "SFA4" "SFA4" "SFA4" "SFA4" ...
# $ pred.name         : chr  "Greenland halibut" "Greenland halibut" "Greenland halibut" "Greenland halibut" ...



###################### Creating Base Predator Dataframe ########################

# 'pred' dataframe: each row is a new fish that's stomach was sampled 

# Dataframe transformations:
    # check number of fish sampled
    # duplicate prey dataframe
    # remove duplicate fish-id values (from multiple prey found within stomachs)
    # confirm number of sampled fish retained is correct
    # add column for number of stomachs sampled

# Finish with checking and storing dataframe structure



### checking how many unique fish were sampled

length(unique(prey$FishKey))
    # 2019 data: 644 observations


### duplicating base prey dataframe

pred <- prey


### Retain one row per sampled fish

# removing duplicated values for FishKey so that there is one entry per predator
pred <- pred[!duplicated(pred$FishKey),]
    # removes duplicates in FishKey
    # code found here https://stackoverflow.com/questions/52038660/how-to-subset-your-dataframe-to-only-keep-the-first-duplicate

# CHECK NUMBER VARIABLES! Does it match # unique fish sampled above
str(pred) 
    # for 2019 data: 644 observations


### Adding category for the number of stomachs sampled per fish

pred$sampled.stomach <- 1 


### Saving Structure of Base Dataframe
str(pred)

# 'data.frame':	644 obs. of  22 variables:
#   $ ContentKey        : chr  "62114003_892_3_1_-14_1733" "62114003_892_3_1_-15_1732" "62114003_892_3_1_-16_1728" "62114003_892_3_1_-17_1729" ...
# $ Prey_OS_ID        : int  6967 9998 6967 6967 6967 6967 8020 8020 8530 8020 ...
# $ PreyWt            : num  0.3 NA 0.286 0.091 0.079 ...
# $ Prey_Count        : int  NA NA NA NA NA NA NA NA NA NA ...
# $ OS_ID             : int  892 892 892 892 892 892 892 892 892 892 ...
# $ Length            : num  24.5 20 20 20.5 17 14 37.5 27 33 33.5 ...
# $ Trawl_ID          : int  62114003 62114003 62114003 62114003 62114003 62114003 62114003 62114003 62114003 62114003 ...
# $ Year              : int  2019 2019 2019 2019 2019 2019 2019 2019 2019 2019 ...
# $ Start.Depth       : int  269 269 269 269 269 269 269 269 269 269 ...
# $ End.Depth         : int  271 271 271 271 271 271 271 271 271 271 ...
# $ Depth.Range       : chr  "201-300" "201-300" "201-300" "201-300" ...
# $ Study.Area        : chr  "2G" "2G" "2G" "2G" ...
# $ Scientific.Name   : chr  "REINHARDTIUS HIPPOGLOSSOIDES" "REINHARDTIUS HIPPOGLOSSOIDES" "REINHARDTIUS HIPPOGLOSSOIDES" "REINHARDTIUS HIPPOGLOSSOIDES" ...
# $ CommonName        : chr  "GREENLAND HALIBUT, TURBOT" "GREENLAND HALIBUT, TURBOT" "GREENLAND HALIBUT, TURBOT" "GREENLAND HALIBUT, TURBOT" ...
# $ Start.Lat.Degree  : int  57 57 57 57 57 57 57 57 57 57 ...
# $ Start.Lat.Minutes : num  45.1 45.1 45.1 45.1 45.1 ...
# $ Start.Long.Degree : int  60 60 60 60 60 60 60 60 60 60 ...
# $ Start.Long.Minutes: num  24.2 24.2 24.2 24.2 24.2 ...
# $ FishKey           : chr  "62114003_892_3_1_-14" "62114003_892_3_1_-15" "62114003_892_3_1_-16" "62114003_892_3_1_-17" ...
# $ region            : chr  "SFA4" "SFA4" "SFA4" "SFA4" ...
# $ pred.name         : chr  "Greenland halibut" "Greenland halibut" "Greenland halibut" "Greenland halibut" ...
# $ sampled.stomach      : num  1 1 1 1 1 1 1 1 1 1 ...





################################################################################
############## Chunk B: Creating Fig 4 Grouped-Bar Chart #######################
################################################################################

# The purpose of this code chunk is to create Figure 4

# The sections of this code chunk are:
    # Creating a new dataframe for Figure 4
    # Creating and Formatting Figure 4 grouped-bar chart


# If desired, additional notes and links to tutorial youtubes/webpages I used to create Figure 4 are recorded in (scripts/draft code/Draft_Script_Fig4_Bar-sample-region.R)

# Those notes include sites and tutorials that assisted me with:
    # Creating figure dataset
    # GGPLOT2 Code for Grouped Pie Charts
    # additional ggplot2 arguments not used in this chart


################### Creating New Dataframe for Fig 4 ###########################

# This dataframe will detail the total sum of stomachs sampled per predator category in each region

# All code to check dataframe structure are commented out. If not they will produce outputs into the report.

# New Dataframe structure will need:
    # Two categorical variables:
        # x-axis: regions
        # fill: predator category
    # One numerical variable: 
        # y-axis: total samples per predator species per region

# library(reshape2) 
    # included in code chunk 'r setup'



# create new dataframe that sums the stomach samples per species, per region
wide.data <- dcast(pred, region~pred.name, value.var="sampled.stomach", sum)
    # creates a dataframe in a 'wide format' and we will want a 'long format' dataframe
    # found code from https://stackoverflow.com/questions/25366929/manipulating-seperated-species-quantity-data-into-a-species-abundance-matrix

wide.data


# converting 'wide format' to 'long format' dataframe
region.total <- melt(data = wide.data,                       # melt() converts from wide format (species spread across rows) to long format (species compiled into one row)
                     id.vars = "region",                     # id.vars : vector of variables I want to stack
                     measured.vars = c("Atlantic cod",       # measured.vars : identifies the columns that have the data I want to stack
                                       "Greenland halibut", 
                                       "Redfish", "Skate"),
                     variable.name = "pred.name",            # variable.name : what I want to call the stacked column
                     value.name = "stomach.sum")             # value.name : name of the column of my stacked measurements in my output data frame
    # See section 3.4.6 'Reshaping data frames' for melt() coding here https://intro2r.com/wrangling-data-frames.html
    
region.total

str(region.total)

### Saving Final Database Structure 

# 'data.frame':	12 obs. of  3 variables:
# $ region     : chr  "EAZ" "SFA4" "WAZ" "EAZ" ...
# $ pred.name  : Factor w/ 4 levels "Atlantic cod",..: 1 1 1 2 2 2 3 3 3 4 ...
# $ stomach.sum: num  2 1 0 198 92 43 34 28 3 120 ...



############### Building and Formatting the Grouped Bar Chart ##################


# Grouped Bar Chart
ggplot(data = region.total,
       aes(fill=pred.name, y=stomach.sum, x=region)) + 
  
  geom_bar(position = position_dodge(),                          # makes the bars beside each other while being compatible with more additional arguments than: position = "dodge"
           width = 0.75,                                         # defines width of bars
           stat="identity",
           color = "black") +                                    # makes black boarders around bars
  
  geom_text(aes(label = stomach.sum),                            # label bar chats based on y value
            vjust = -0.5,                                        # puts labels above bars
            size = 3,                                            # changes size of labels
            position = position_dodge(width = 0.75)) +           # matches the labels with our grouped bars (match widths in geom_text and geom_bar)
  
  scale_fill_manual(values = c("Atlantic cod" = "#FCFED4",       # above assigns colour to particular predator categories
                               "Greenland halibut" = "#CCEDB1", 
                               "Redfish" = "#41B7C4", 
                               "Skate" ="#FF9999" )) +
  
  theme_minimal() +                                              # minimal theme removes right/left boarders, x-axis directly below bars, removes gray background color
  
  theme(legend.title = element_blank(),                          # removes legend title
        legend.key.size = unit(3, "mm"),                         # adjusts width of legend color symbols
        legend.key.height = unit(3, "mm"),                       # adjusts height of legend color symbols 
        legend.key.spacing.y = unit(1, "mm"),                    # defines space between legend items
        panel.grid.minor = element_blank(),                      # removes all minor grid-lines 
        panel.grid.major.x = element_blank()) +                  # removes grid-lines along the x-axis
  
  labs(x = "Region",                                             # x-axis title
       y = "Number of Stomachs Processed",)                      # y-axis title


