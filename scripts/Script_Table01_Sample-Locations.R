################################################################################
################################################################################
#                   Table 1 Sample Locations per Depth - DRAFT
################################################################################
################################################################################


# Author: Krista Kenyon (KAK)
# Date Created: July 5/2024
# Date Last Modified: July 5/2024 by KAK


# Document Purpose: Working document for creating Table 1.
    # Working in code is simpler and faster here than in the R Markdown document
        # R Markdown places code results below chunks, which can mean a lot of scrolling
        # Not worried about accidentally knitting the whole document when testing out different code chunks
    # Current version is the draft code.

# Document Sections:
    # Remaining Tasks
    # Load Libraries & Base Data
    # Code Chunk: ............
    # .......


# Code Chunk was transferred into the R Markdown Table 1: sample-loc


# To see tutorials, key webpages, and key videos I used to create and format Table 1:
    # please see the 'Draft_Script_Table01_sample-location.R' in the 'Draft Code' folder

# Those notes include sites and tutorials that assisted me with:
    # Creating figure dataset
    # GGPLOT2 Code for Grouped Pie Charts
    # additional ggplot2 arguments not used in this chart


################################################################################
#                            Remaining Tasks 
################################################################################


# Step 1: Learn how to create tables in R and R Markdown



################################################################################
#                         Load Libraries & Base Data          
################################################################################


### load libraries

library(ggplot2) 
library(tidyverse)
library(dplyr)
library(reshape2) 
library(viridis)                 # Fig 5 - color blind friendly palette
# library(ggrepel)               # formats figure labels so they don't overlap
library(gt)                      # Table formatting



### import data

prey <- read.csv('data/processed/2019_basePrey.csv')


################################################################################
#              Code Chunk: Creating Fig 5 Paneled-Bar Chart 
################################################################################


# The purpose of this code chunk is to create Table 1

# The sections of this code chunk are:
    # ................
    # ...............


################### Creating New Dataframe for Table 1 ###########################


# This dataframe will detail (the total sum of sample stations per depth category in each region)

######################################## Old Section
# New Dataframe structure will need:
    # Three categorical variables:
        # x-axis: predator length
        # fill: depth range
        # facet: predator category
    # One numerical variable:
        # y-axis: sum stomach samples that falls within categories

########################################### Back to new section


### create Table 1 dataframe:

### checking how many unique sample locations occurred

length(unique(prey$Trawl_ID))
    # 2019 data: 92 observations



### duplicating base prey dataframe

samp <- prey



### Retain one row per sampled Trawl ID

samp <- samp[!duplicated(samp$Trawl_ID),]
    # removing duplicated values for Trawl_ID. Results in one entry per trawl (i.e. sample locations)


# CHECK NUMBER VARIABLES! Does it match # unique Trawl_ID sampled above


str(samp) 
    # for 2019 data: 92 observations



### Saving Final Dataframe Structure 

# str(samp)

# 'data.frame':	92 obs. of  24 variables:

# $ X                 : int  1 26 29 35 48 59 121 179 184 196 ...
# $ ContentKey        : chr  "62114003_892_3_1_-14_1733" "62114004_95_3_1_-36_1553" "62114013_892_3_1_-41_1691" "62114014_892_3_1_-63_380" ...
# $ Prey_OS_ID        : int  6967 6000 8020 8112 451 8020 6000 9998 6996 9998 ...
# $ PreyWt            : num  0.3 4.4 3.56 1.54 13.93 ...
# $ Prey_Count        : int  NA NA 1 1 1 NA NA NA 1 NA ...
# $ OS_ID             : int  892 95 892 892 892 89 794 794 89 792 ...
# $ Length            : num  24.5 45.5 35 19 36.2 28 13.5 27.7 48 28 ...
# $ Trawl_ID          : int  62114003 62114004 62114013 62114014 62114087 62114089 62114090 62114093 62114096 62114021 ...
# $ Year              : int  2019 2019 2019 2019 2019 2019 2019 2019 2019 2019 ...
# $ Start.Depth       : int  269 290 182 161 225 603 421 506 248 720 ...
# $ End.Depth         : int  271 289 184 163 225 604 427 504 239 695 ...
# $ Depth.Range       : chr  "201-300" "201-300" "0-200" "0-200" ...
# $ Study.Area        : chr  "2G" "2G" "2G" "2G" ...
# $ Scientific.Name   : chr  "REINHARDTIUS HIPPOGLOSSOIDES" "RAJA HYPERBOREA = AMBLYRAJA HYPERBOREA" "REINHARDTIUS HIPPOGLOSSOIDES" "REINHARDTIUS HIPPOGLOSSOIDES" ...
# $ CommonName        : chr  "GREENLAND HALIBUT, TURBOT" "ARCTIC SKATE" "GREENLAND HALIBUT, TURBOT" "GREENLAND HALIBUT, TURBOT" ...
# $ Start.Lat.Degree  : int  57 57 58 58 60 60 60 61 62 58 ...
# $ Start.Lat.Minutes : num  45.1 48.4 33.4 32.5 32.3 ...
# $ Start.Long.Degree : int  60 60 62 61 65 65 65 63 63 59 ...
# $ Start.Long.Minutes: num  24.16 23.56 5.16 56.02 57.33 ...
# $ FishKey           : chr  "62114003_892_3_1_-14" "62114004_95_3_1_-36" "62114013_892_3_1_-41" "62114014_892_3_1_-63" ...
# $ region            : chr  "SFA4" "SFA4" "SFA4" "SFA4" ...
# $ pred.name         : chr  "Greenland halibut" "Skate" "Greenland halibut" "Greenland halibut" ...
# $ depth             : chr  "200-300 m" "200-300 m" "100-200 m" "100-200 m" ...
# $ length.range      : chr  "21-25" "41-45" "31-35" "16-20" ...



### Exporting Table 1 Sample Location Dataframe

write.csv(samp, file="data/processed/2019_T1_sampleLocation.csv")

###################    Building and Formatting Table 1    ######################


# ..... Any Notes? .......


### Load Table 1 dataframe


samp <- read.csv('data/processed/2019_T1_sampleLocation.csv')


# Below doesn't group it the way I want for the table. Which is  what the default table() provides
# samp.loc <- samp %>%
#   select(depth, region)
# 
# view(samp.loc)


### create Table 1 dataframe: sum survey locations per regions, per depth

# formula will need something to sum. trawl.sample is the number of trawls per trawl_id (or location)
samp$trawl.sample <- 1 

# reformatting the data into the structure gt() will want
s1 <- dcast(samp, depth~region, value.var="trawl.sample", sum) %>%
  relocate(SFA4, .after = WAZ)
      # moves the SFA4 column after WAZ. the '.' in .after is important!
      # used the dyplr cheat sheet and copilot


view(s1)

# checking that we still have correct # of survey locations
sum(s1$EAZ + s1$SFA + s1$WAZ)
  # 2019 data = 92 survey locations



### Table 1

# table(samp$depth, samp$region) # option 1 - I think formatting will be tricky

# I WILL NEED TO RE-ORDER EAZ, WAZ, and SFA4
# I WILL ALSO WANT TO REMOVE THE 'm' FROM THE DEPTH COLUMN ROWS

s1 %>%
  gt() %>%
  cols_label(depth ~ "Depth Stratum (m)",
             SFA4 ~ "SFA 4")  %>% 
  tab_spanner(label = "Assessment Area",
              columns = EAZ:SFA4) %>%
  opt_stylize(style = 5, color = "gray") %>%
 # cols_width(everything() ~ px(130))
  
  cols_width(depth ~ px(150),
             ends_with("Z") ~ px(130),
             SFA4 ~ px(130))




