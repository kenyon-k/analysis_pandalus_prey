################################################################################
################################################################################
#                   Figure 5 Stomach by Length & Depth - DRAFT
################################################################################
################################################################################

# Author: Krista Kenyon (KAK)
# Date Created: June 17/2024
# Date Last Modified: June 28/2024 by KAK

# Document Purpose: Working document for creating the Figure 5 paneled bar charts.
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
    # Chunk B: Creating the Paneled Bar Charts examining stomachs sampled per fish length
        # code transferred into the R Markdown Chunk 5: stomach-length
    # Additional Notes


# To see tutorials, key webpages, and key videos I used to create and format the 
# bar chart, please see the 'Draft_Script_Fig05_Stomach-by-Length.R' in the 'Draft Code' folder


################################################################################
######################### Remaining Tasks ######################################
################################################################################

## Step 1: Select which chart style I will go with

## Step 2: Save base prey and pred dataframe outputs for cleaner code

## Step 3: Clean up Document Code

## Step 4: Clean up Draft Script doc
  
## Step 5: Decide if I want to mess around with panel boarders right now

## BACKBURNER: Is there a way to clean up my predator length codes


################################################################################
####################### Library's to Load ######################################
################################################################################
library(ggplot2) 
library(tidyverse)
library(dplyr)
library(reshape2) 
library(viridis)  # Fig 5 - color blind friendly palette
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
############## Chunk B: Creating Fig 5 Paneled-Bar Chart #######################
################################################################################

# The purpose of this code chunk is to create Figure 5

# The sections of this code chunk are:
    # Creating a new dataframe for Figure 5
    # Creating and Formatting Figure 5 paneled-bar chart


# If desired, additional notes and links to tutorial youtubes/webpages I used to create Figure 5 are recorded in (scripts/draft code/Draft_Script_Fig5_Stomach-by-Length.R)

# Those notes include sites and tutorials that assisted me with:
    # ....................
    # ....................
    # ....................


################### Creating New Dataframe for Fig 5 ###########################

# This dataframe will detail the total sum of stomachs sampled per size class within each depth stratum.
    # Different panel for each predator category

# All code to check dataframe structure are commented out. If not they will produce outputs into the report.

# New Dataframe structure will need:
    # Three categorical variables:
        # x-axis: predator length
        # fill: depth range
        # facet: predator category
    # One numerical variable:
        # y-axis: sum stomach samples that falls within categories



### Adding depth range category
    # The database does have a depth-range column with a variety of values.
    # This code needs to be repeatable.
    # I'm not sure if new depth-range categories will be added in the future
    # So I am creating a depth range category based on start and end depths

# creating new column filled with NAs (makes QA easier)
pred$depth <- NA 

# Creating the depth range categories
    # yes multiple categories both use the same 'start/end' points.
    # the combination of requirements within each depth category means there isn't actually overlap
pred$depth[pred$Start.Depth >= 100 & pred$End.Depth >= 100 & 
             pred$Start.Depth <= 200 & pred$End.Depth <= 200] <- "100-200 m"

pred$depth[pred$Start.Depth >= 200 & pred$End.Depth >= 200 & 
             pred$Start.Depth <= 300 & pred$End.Depth <= 300] <- "200-300 m"

pred$depth[pred$Start.Depth >= 300 & pred$End.Depth >= 300 & 
             pred$Start.Depth <= 400 & pred$End.Depth <= 400] <- "300-400 m"

pred$depth[pred$Start.Depth >= 400 & pred$End.Depth >= 400 & 
             pred$Start.Depth <= 500 & pred$End.Depth <= 500] <- "400-500 m"

pred$depth[pred$Start.Depth >= 500 & pred$End.Depth >= 500 & 
             pred$Start.Depth <= 750 & pred$End.Depth <= 750] <- "500-750 m"

# check if there are any NAs remaining (shows # NA's per column)
colSums(is.na(pred))   





### Adding predator length range category

## creating new column filled with NAs (makes QA easier)
pred$length.range <- NA

# Creating the depth range categories
pred$length.range[pred$Length < 6] <- "0-5" 
pred$length.range[pred$Length >= 6 & pred$Length < 11] <- "6-10"
pred$length.range[pred$Length >= 11 & pred$Length < 16] <- "11-15"
pred$length.range[pred$Length >= 16 & pred$Length < 21] <- "16-20"
pred$length.range[pred$Length >= 21 & pred$Length < 26] <- "21-25"
pred$length.range[pred$Length >= 26 & pred$Length < 31] <- "26-30"
pred$length.range[pred$Length >= 31 & pred$Length < 36] <- "31-35"
pred$length.range[pred$Length >= 36 & pred$Length < 41] <- "36-40"
pred$length.range[pred$Length >= 41 & pred$Length < 46] <- "41-45"
pred$length.range[pred$Length >= 46 & pred$Length < 51] <- "46-50"
pred$length.range[pred$Length >= 51 & pred$Length < 56] <- "51-55"
pred$length.range[pred$Length >= 56 & pred$Length < 61] <- "56-60"
pred$length.range[pred$Length >= 61 & pred$Length < 66] <- "61-65"
pred$length.range[pred$Length >= 66 & pred$Length < 71] <- "66-70"
pred$length.range[pred$Length >= 71 & pred$Length < 76] <- "71-75"
pred$length.range[pred$Length >= 76 & pred$Length < 81] <- "76-80"
pred$length.range[pred$Length >= 81 & pred$Length < 85] <- "81-85"
pred$length.range[pred$Length >= 86 & pred$Length < 91] <- "86-90"
pred$length.range[pred$Length >= 91 & pred$Length < 96] <- "91-95"
pred$length.range[pred$Length >= 96 & pred$Length < 101] <- "96-100"
pred$length.range[pred$Length >= 101 & pred$Length < 106] <- "101-105"
pred$length.range[pred$Length >= 106 & pred$Length < 111] <- "106-110"
pred$length.range[pred$Length >= 111 & pred$Length < 116] <- "111-115"
pred$length.range[pred$Length >= 116 & pred$Length < 121] <- "116-120"
pred$length.range[pred$Length >= 121 & pred$Length < 126] <- "121-125"
pred$length.range[pred$Length >= 126 & pred$Length < 131] <- "126-130"
pred$length.range[pred$Length >= 131 & pred$Length < 136] <- "131-135"
pred$length.range[pred$Length >= 136 & pred$Length < 141] <- "136-140"
pred$length.range[pred$Length >= 141 & pred$Length < 146] <- "141-145"
pred$length.range[pred$Length >= 146 & pred$Length < 151] <- "146-150"
pred$length.range[pred$Length >= 151 & pred$Length < 156] <- "151-155"
pred$length.range[pred$Length >= 156 & pred$Length < 161] <- "156-160"
pred$length.range[pred$Length >= 161 & pred$Length < 166] <- "161-165"
pred$length.range[pred$Length >= 166 & pred$Length < 171] <- "166-170"
pred$length.range[pred$Length >= 171 & pred$Length < 176] <- "171-175"
pred$length.range[pred$Length >= 176 & pred$Length < 181] <- "176-180"
pred$length.range[pred$Length >= 181 & pred$Length < 185] <- "181-185"
pred$length.range[pred$Length >= 186 & pred$Length < 191] <- "186-190"
pred$length.range[pred$Length >= 191 & pred$Length < 196] <- "191-195"
pred$length.range[pred$Length >= 196 & pred$Length < 201] <- "196-200"


colSums(is.na(pred)) 


# I still don't like the above code. It's super clunky. BUT it will give the desired effects.
# Any values outside of these defined ranges in future years will appear as NA in the charts.

# For my attempts at cleaner binning code, and rational as to why those methods were not used:
    # See the bottom of scripts/draft code/Draft_Script_Fig5_Stomach-by-Length.R



############### Building and Formatting the Grouped Bar Chart ##################

#### Option 1 ####

# This option has 1 legend and uses the predator category as a title vs a/b/c/d


# look at for panel labels and outlines in following webpage: https://stackoverflow.com/questions/67098543/facet-wrap-labels-as-panel-labels-in-ggplot

# look at for labels per panel https://stackoverflow.com/questions/14840542/place-a-legend-for-each-facet-wrap-grid-in-ggplot2

####### TESTING FACET_GRID
 
# 
# ggplot(pred, aes(reorder(length.range, Length))) +
#   
#   geom_bar(aes(fill = depth),
#            position = "stack",
#            stat="count",
#            width = 0.4) +   
#   
#   facet_grid(depth ~ pred.name) #, 
#              # scale = "free_y",
#              # axes = "all_x")
# 
# theme_minimal() + 
#   
#   theme(axis.text.x = element_text(angle = 45,hjust = 1, size = 8 ),
#         axis.title = element_text(size = 8),
#         axis.title.y = element_text(vjust = +3),                 # pulls y-axis title away from chart
#         axis.title.x = element_text(vjust = -2),                 # pulls x-axis title away from chart
#         legend.title = element_blank(),                          # removes legend title
#         common.legend = TRUE,
#         legend.key.size = unit(3, "mm"),                         # adjusts width of legend color symbols
#         legend.key.height = unit(3, "mm"),                       # adjusts height of legend color symbols
#         legend.key.spacing.y = unit(1, "mm"),                    # defines space between legend items
#         panel.grid.minor = element_blank(),                      # removes all minor grid-lines
#         panel.grid.major.x = element_blank(),
#         panel.spacing = unit(13, 'point'),                         # adds space between panels
#         # panel.border = element_rect(fill = "transparent",        # needed to add the boarder       
#         #                            color = "black", linewidth = 1.0), # boarder color and width
#         strip.text = element_text(face = "bold",      # bolds the panel titles
#                                   hjust = 0)) +       # pulls panel titles to the top left
#   
#   labs(x = "Predator Length (cm)",
#        y = "Number of Stomachs")


####

g05<- ggplot(pred, aes(reorder(length.range, Length))) +  # orders the X-axis categories based on numerical length of fish
        # basically makes ggplot see and order the x-axis as numbers vs text
  
  geom_bar(aes(fill = depth),
           position = "stack",
           stat="count",
           width = 0.4) +   # changes the width of the bars 
  
  facet_wrap(~ pred.name, 
             nrow = 5, 
             scale = "free_y",
             axes = "all_x") +
  
 # scale_x_discrete(limits = c("6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "36-40",
 #                              "41-45", "46-50", "51-55", "56-60", "61-65", "66-70", 
 #                             "71-75", "76-80", "81-85", "140+")) +
  
 # scale_fill_viridis(discrete = TRUE,   # color-blind friendly palet for discrete data
 #                    direction = -1) +  # flips the order of the colours
    
  theme_minimal() + 
  
  theme(axis.text.x = element_text(angle = 45,hjust = 1, size = 8 ),
        axis.title = element_text(size = 8),
        axis.title.y = element_text(vjust = +3),                 # pulls y-axis title away from chart
        axis.title.x = element_text(vjust = -2),                 # pulls x-axis title away from chart
        legend.title = element_blank(),                          # removes legend title
        common.legend = TRUE,
        legend.key.size = unit(3, "mm"),                         # adjusts width of legend color symbols
        legend.key.height = unit(3, "mm"),                       # adjusts height of legend color symbols
        legend.key.spacing.y = unit(1, "mm"),                    # defines space between legend items
        panel.grid.minor = element_blank(),                      # removes all minor grid-lines
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(13, 'point'),                         # adds space between panels
        # panel.border = element_rect(fill = "transparent",        # needed to add the boarder       
        #                            color = "black", linewidth = 1.0), # boarder color and width
        strip.text = element_text(face = "bold",      # bolds the panel titles
                                  hjust = 0)) +       # pulls panel titles to the top left
  
  labs(x = "Predator Length (cm)",
       y = "Number of Stomachs")


### Option 2 ###

# This option has 1 legend and uses a/b/c/d for each panel

g05 +  facet_wrap(~ pred.name, 
                   nrow = 5, 
                   scale = "free_y",
                   axes = "all_x",
                   labeller = labeller(pred.name = 
                                         c("Atlantic cod" = "a",
                                           "Greenland halibut" = "b",
                                           "Redfish" = "c",
                                           "Skate" = "d")))

### Option 3 ###

# This option has individual legends and uses a/b/c/d for each panel
    # So far I can't get each individual legends to show the 0'd values

# Testing using Grid-arrange

library(gridExtra)
library(ggpubr)

sp <- split(pred,f = pred$pred.name)

p01 <- ggplot(sp$`Atlantic cod`, aes(x = length.range, fill = depth)) +
  
  geom_bar(position = "stack",
           stat="count",
           width = 0.4) +   # changes the width of the bars 
  
  scale_x_discrete(limits = c("6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "36-40",
                              "41-45", "46-50", "51-55", "56-60", "61-65", "66-70", 
                              "71-75", "76-80", "81-85", "140+")) +
  
  scale_fill_viridis(discrete = TRUE,   # color-blind friendly palet for discrete data
                     direction = -1) +  # flips the order of the colours
  
  # scale_fill_manual(values = c("red", "blue", "green", "purple", "pink"),
  #                   breaks = c("100-200 m", "200-300 m", "300-400 m", "300-400 m", "500-750 m"),
  #                   labels = c("100-200 m", "200-300 m", "300-400 m", "300-400 m", "500-750 m"),
  #                   drop = FALSE) +
  
 # scale_fill_manual(values = depth, breaks = c("100-200 m", "200-300 m", "300-400 m", "300-400 m", "500-750 m")) +
  theme_minimal() + 
  
  theme(axis.text.x = element_text(angle = 45,hjust = 1, size = 8 ),
        axis.title = element_text(size = 8),
        axis.title.y = element_text(vjust = +3),                 # pulls y-axis title away from chart
        axis.title.x = element_text(vjust = -2),                 # pulls x-axis title away from chart
        legend.title = element_blank(),                          # removes legend title
        legend.key.size = unit(3, "mm"),                         # adjusts width of legend color symbols
        legend.key.height = unit(3, "mm"),                       # adjusts height of legend color symbols
        legend.key.spacing.y = unit(1, "mm"),                    # defines space between legend items
        panel.grid.minor = element_blank(),                      # removes all minor grid-lines
        panel.grid.major.x = element_blank(),
        plot.margin = margin(t=25, r=8, b=10, l=9)) +
  
  labs(x = "Predator Length (cm)",
       y = "Number of Stomachs")


p02 <- p01 %+% sp$`Greenland halibut`
p03 <- p01 %+% sp$Redfish
p04 <- p01 %+% sp$Skate

ggarrange(p01, p02, p03, p04, 
             nrow = 4, labels = "auto", vjust = 0.8)

?ggarrange

#### BELOW IS MESSY
# Using dplyr and ggarrange - this is getting messy fast. Not a fan

p1 <- pred %>% filter(pred.name == "Atlantic cod") %>% 
  ggplot(aes(x = length.range, fill = depth)) +
  geom_bar(position = "stack",
           stat = "count") +
  scale_x_discrete(limits = c("6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "36-40",
                              "41-45", "46-50", "51-55", "56-60", "61-65", "66-70", 
                              "71-75", "76-80", "81-85", "140+")) +
  theme_minimal() + 
  
  theme(axis.text.x = element_text(angle = 45,hjust = 1 ),
        legend.title = element_blank(),                          # removes legend title
        legend.key.size = unit(3, "mm"),                         # adjusts width of legend color symbols
        legend.key.height = unit(3, "mm"),                       # adjusts height of legend color symbols
        legend.key.spacing.y = unit(1, "mm"),                    # defines space between legend items
        panel.grid.minor = element_blank(),                      # removes all minor grid-lines
        panel.grid.major.x = element_blank())


p2 <- pred %>% filter(pred.name == "Greenland halibut") %>% 
  ggplot(aes(x = length.range, fill = depth)) +
  geom_bar(position = "stack",
           stat = "count") +
  scale_x_discrete(limits = c("6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "36-40",
                              "41-45", "46-50", "51-55", "56-60", "61-65", "66-70", 
                              "71-75", "76-80", "81-85", "140+")) +
  theme_minimal() + 
  
  theme(axis.text.x = element_text(angle = 45,hjust = 1 ),
        legend.title = element_blank(),                          # removes legend title
        legend.key.size = unit(3, "mm"),                         # adjusts width of legend color symbols
        legend.key.height = unit(3, "mm"),                       # adjusts height of legend color symbols
        legend.key.spacing.y = unit(1, "mm"),                    # defines space between legend items
        panel.grid.minor = element_blank(),                      # removes all minor grid-lines
        panel.grid.major.x = element_blank())


ggarrange(p1, p2, nrow = 2, labels = "AUTO")  

?scale_x_discrete()




####### BELOW IS FROM FIG 4

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


