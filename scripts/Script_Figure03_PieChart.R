################################################################################
################################################################################
#                        Figure 3 Pie Chart - Clean
################################################################################
################################################################################


# Author: Krista Kenyon (KAK)
# Date Created: May 6/2024
# Date Last Modified: July 4/2024 by KAK

# Document Purpose: Working document for creating the Figure 3 pie chart.
    # Working in code is simpler and faster here than in the R Markdown document
        # R Markdown places code results below chunks, which can mean a lot of scrolling
        # Not worried about accidentally knitting the whole document when testing out different code chunks
    # Current version is up to date clean code.

# Document Sections:
    # Remaining Tasks
    # Load Libraries
    # Chunk A: Importing and Prepping the Data
    # Chunk B: Creating the Pie Chart Figure
    # Additional Notes

# Chunk A was transferred into the R Markdown Chunk 2: import data
# Chunk B was transferred into the R Markdown Chunk 3: pie chart

# To see tutorials, key webpages, and key videos I used to create and format the 
    # pie chart, please see the 'Draft_Script_Fig3_PieChart.R' in the 'Draft Code' folder


################################################################################
#                              Remaining Tasks 
################################################################################


# Choose colour-blindness friendly colour scheme


################################################################################
#                       Load Libraries and Import Data
################################################################################

# !!! The data imported must be structured consistently in order to utilize this coding. !!!

# This code chunk:
    # loads necessary libraries
    # imports the data
    # checks the dataframe structure against example



### Load Libraries

library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggrepel)        # formats figure labels so they don't overlap



### Import Data

prey <- read.csv('data/raw/2019_StomachContent_test.csv') 
    # assumes header=TRUE and knows it's separated by commas



### Check the data structure! Does it match?

str(prey)
 

# EXAMPLE BELOW

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
 


################################################################################
#                     Creating Base Prey Dataframe 
###############################################################################


# This code chunk:
    # manipulates the data it to create the base prey dataframe
    # checks & saves the dataframe structure

# 'prey' dataframe: each row is a new prey item per sampled stomachs

# Dataframe transformations:
    # add column with shrimp fishing regions (EAZ, WAZ, SFA4)
    # add column with predator 'category' names (Atlantic code, Greenland halibut, Redfish, Skate)
    # add column with depth range category (100-200 m, 200-300 m, 300-400 m, 400-500 m, 500-750 m)
    # add predator length range category (binned into 5 cm length groups)

# Finish with checking and storing dataframe structure

# Export base prey dataframe to data/processed/



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



### Adding depth range category

# The Access database does have a depth-range column BUT it has with a variety of overlapping ranges.
# Below code creates desired depth ranges based on start-end depths
# This ensures code is repeatable even if new depth ranges are added into the database

prey <- prey %>% 
  mutate(depth = case_when(Start.Depth >= 100 & End.Depth >= 100 & 
                             Start.Depth <= 200 & End.Depth <= 200 ~ "100-200 m",
                           
                           Start.Depth >= 200 & End.Depth >= 200 & 
                             Start.Depth <= 300 & End.Depth <= 300 ~ "200-300 m",
                           
                           Start.Depth >= 300 & End.Depth >= 300 & 
                             Start.Depth <= 400 & End.Depth <= 400 ~ "300-400 m",
                           
                           Start.Depth >= 400 & End.Depth >= 400 & 
                             Start.Depth <= 500 & End.Depth <= 500 ~ "300-400 m",
                           
                           Start.Depth >= 500 & End.Depth >= 500 & 
                             Start.Depth <= 750 & End.Depth <= 750 ~ "500-750 m"))
    # yes multiple categories both use the same 'start/end' points.
    # the combination of requirements within each depth category means there isn't actually overlap



### Adding predator length range category

# Binning the data
prey <- prey %>% 
  mutate(length.range = cut(Length, breaks=seq(from = 0, to = 5.99, by = 5.99))) %>% 
    # sets first bin as < 6
  
  mutate(length.range = cut(Length, breaks=seq(from = 5.99, to = max(Length)+5, by = 5)))   
    # sets remaining bins as # >=6 and <11 (or range 6-10). Etc
    # max(Length) ends the sequence at the length of the longest fish sampled
    # longest fish ends up being excluded with this
    # max(Length)+5 ensures the longest fish will be included


# Reformatting the binned outputs 
prey <- prey %>%
  mutate(length.range = str_replace(length.range, ',', '-')) %>%        
    # replaces , with -
  
  mutate(length.range = gsub("[(]|[]]", '', length.range,)) %>%        
    # removes ( or ]. Equivalent code wasn't working with str_replace
  
  mutate(length.range = str_replace(length.range, "5.99", "6"))  %>%    
    # replaces 5.99 with 6
  
  mutate(length.range = str_replace(length.range, "1$", "0")) %>%       
    # if the last digit is 1, it is replaced with a 0
  
  mutate(length.range = str_replace(length.range, "6$", "5"))           
    # if the last digit is 6, it is replaced with a 5



### Any missing values?

colSums(is.na(prey)) 
    # check if there are any NAs remaining (shows # NA's per column)



### Saving the prey dataframe structure

 str(prey) 

 # 'data.frame':	1236 obs. of  23 variables:
 
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
 # $ depth             : chr  "200-300 m" "200-300 m" "200-300 m" "200-300 m" ...
 # $ length.range      : chr  "21-25" "16-20" "16-20" "16-20" ...


 
### Exporting Base Prey Dataframe
 
 write.csv(prey, file="data/processed/2019_basePrey.csv")


################################################################################
#                   Creating Base Predator Dataframe 
###############################################################################
 
 
# This code chunk:
    # manipulates the data it to create the base predator dataframe
    # checks & saves the dataframe structure
 
# 'pred' dataframe: each row is a new fish that's stomach was sampled 

# Dataframe transformations:
    # check number of fish sampled
    # duplicate prey dataframe
    # remove duplicate fish-id values (from multiple prey found within stomachs)
    # confirm number of sampled fish retained is correct
    # add column for number of stomachs sampled

# Finish with checking and storing dataframe structure
 
# Export base pred dataframe to data/processed/



### checking how many unique fish were sampled

length(unique(prey$FishKey))
    # 2019 data: 644 observations



### duplicating base prey dataframe

pred <- prey



### Retain one row per sampled fish

pred <- pred[!duplicated(pred$FishKey),]
    # removing duplicated values for FishKey. Results in one entry per predator


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



### Exporting Base Predator Dataframe

write.csv(pred, file="data/processed/2019_basePred.csv")


################################################################################
#                    Chunk B: Creating the Pie Chart Figure 
################################################################################


# The purpose of this code chunk is to create Figure 3

# The sections of this code chunk are:
    # Creating a new dataframe for Figure 3
    # Creating and Formatting Figure 3 pie chart
    # Recording Additional Geom_Label arguments that we may want to include in further edits 

# If desired, additional notes and links to tutorial youtubes/webpages I used to create Figure 3 are recorded in (scripts/draft code/Draft_Script_Fig3_PieChart.R)

# Those notes include other pie chart packages, functions, and arguments that I learned but did not end up using


################### Creating new dataframe for Figure  #########################


# This dataframe will detail the total sum of stomachs sampled per predator category

# All code to check dataframe structure are commented out. If not they will produce outputs into the report.



### Create figure dataframe

pred.total <- aggregate(pred$sampled.stomach, list(pred$pred.name), sum)

# pred.total  



### Renaming the columns in the new dataframe

names(pred.total)[names(pred.total) == "Group.1"] <- "pred.name"

names(pred.total)[names(pred.total) == "x"] <- "stomach.total"

# names(pred.total)



### Creating figure labels

pred.total$label <- paste(pred.total$stomach.total, sep = "
", 
paste0( "(", round(100 * pred.total$stomach.total / sum(pred.total$stomach.total), 0 ), "%)" ))
    # Label structure will be 'total stomach sampled (% of stomachs)'
    # DO NOT ADJUST THE SPACING AFTER [sep = "..."]. This splits the values into 2 lines. Adding spaces will uncenter second line. 
    # paste() puts spaces between labels
    # paste0() does not have spaces between labels. Which means there is no space between # and % (e.g. 40% vs 40 %)



### Checking Final Database Structure

# str(pred.total)



### Saving Final Database Structure 

# 'data.frame':	4 obs. of  3 variables:
#  $ pred.name    : chr  "Atlantic cod" "Greenland halibut" "Redfish" "Skate"
#  $ stomach.total: num  20 1340 900 40
#  $ label        : chr  "20\n(1%)" "1340\n(58%)" "900\n(39%)" "40\n(2%)"



### Exporting Figure 3 pie-chart Dataframe

write.csv(pred.total, file="data/processed/2019_F3_pred.total.csv")


################### Building and Formatting the Pie Chart ######################


### Load Fig 3 dataframe

#pred.total <- read.csv('data/processed/2019_F3_pred.total.csv')



### Creating the pie chart

ggplot(pred.total, aes(x="", y=stomach.total, fill=pred.name)) +
  coord_polar(theta = "y") +                         # basing the circular coordinates on the y value (stomach.total)
  geom_col(color = "white", linewidth = 0.5) +       # creates white line separations between pieces
  theme_void() +                                     # removes the default theme. Which is grey background with circular gridlines outside the pie
  geom_label(aes(x = 1.65,                           #'x=#' moves labels along 'x-axis'. Goal here is outside the pie chart. 
                 label = label),                     # label = pred.total$label created above. with 'aes()' argument it knows we are in 'pred.total', so we don't need 'pred.total$..'
             size = 2.5,                             # adjusts size of label text
             position = position_stack(vjus =0.5),   # defines 'y-axis' position of label. 'vjus=0.5' is centered. 
             # fill = "white",                       # changes label fill colour. Default colour matches pie chart chunk colours
             #label.padding = unit(1.2, "mm"),       # adjusts space between label name and boarder
             #label.size =  nit(0.2, "mm"),          # adjusts thickness of label boarders
             #label.r = unit(3, "mm"),               # adjusts roundness of label corners
             # family = "montserrat",                # this argument would change the font, with "montserrat" being a font style (would need to import before chart)
             show.legend = FALSE) +                  # Removes geom_label default of adding an 'a' to legend colors.
  theme(legend.title = element_blank(),              # removes legend title
        legend.key.size = unit(3, "mm"),             # adjusts width of legend color symbols
        legend.key.height = unit(3, "mm"),           # adjusts height of legend color symbols 
        legend.key.spacing.y = unit(1, "mm"),)       # defines space between legend items



################################################################################
#                             Additional Notes 
################################################################################


# Notes on different packages not used, or not yet used, are below.


################## Additional Notes on geom_label() ############################


# geom_label creates a box around the label. geom_text does not

# geom_label arguments that were not used because better solutions were found include:

      # hjust = "outward"   # Adjusts position label location and justification. 
                                # Other arguments include "inward", "left", "right", 1, 0, 0.5 

      # nudge_x = 0.6,      # moves the labels away from pie chart center. 'x=#' is much better solution
                                # 'nudge' does not center label in pie chart chunk (i.e. adjust position along 'y axis')
                                #  cannot use 'position' related arguments and nudge at the same time. 
                                    # 'position' arguments can be used to adjust label position along 'y-axis'
                                # formatting in general is tricky because the pie chart is an adjusted coordinate system of the bar charts.
                                # the 'x=#' adjusts label position along the 'x-axis' while allowing other arguments to adjust the 'y-axis'

      # family = "style"    # likely would need to install/load the font style in earlier code 


######################### Notes on geom_label_repel ############################


# replaces geom_label() with geom_label_repel()

# formats labels so they don't overlap.
# Default will push overlapping labels outside out defined x-axis and y-axis range. 
    # I haven't played around enough to solve this issue

# Default lines connecting the labels to center of the pie chart chunks rather than the outside.
    # It looks messy. And sometimes the lines don't actually connect to the right chunk.....

# I haven't played around with formatting to adjust defaults.
    # I'm waiting to have the actual data to see if that's needed.

# One additional argument that sets the minimum segment length is below
    # I don't fully understand this argument
    # min.segment.length = 0.25,



