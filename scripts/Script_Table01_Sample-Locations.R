################################################################################
################################################################################
#                   Table 1 Sample Locations per Depth - DRAFT
################################################################################
################################################################################


# Author: Krista Kenyon (KAK)
# Date Created: July 5/2024
# Date Last Modified: July 23/2024 by KAK


# Document Purpose: Creating Table 1 for R Markdown document.
    # Working in code is simpler and faster here than in the R Markdown document
    # R Markdown places code results below chunks, which can mean a lot of scrolling


# R Markdown code pipes this code directly into the document.
    # !! Before knitting ensure to comment out all codes that produce console outputs except the table
    # e.g. str()


# Table 1 is built via flextable package which:
    # is designed for Word
    # allows for detailed formatting
    # compatible with Markdown cross-referencing


# Other table packages I tried but are not compatable with Word include:
    # kable with kableExtra
    # gt
    # Additional pdf/html options are found here: https://rfortherestofus.com/2019/11/how-to-make-beautiful-tables-in-r/ 


# Document Sections:
    # Remaining Tasks
    # Load Libraries & Base Data
    # Code Chunk: Creating Table 1
        # Subsection: Creating New Dataframe for Table 1
        # Subsection: Building and Formatting Table 1



# To see tutorials, key webpages, and key videos I used to create and format Table 1:
    # please see the 'Draft_Script_Table01_sample-location.R' in the 'Draft Code' folder

# Draft Script notes include sites and tutorials that assisted me with:
    # Creating figure dataset
    # Piping Data into R Markdown
    # Table Formatting - flextable & officer
    # Table Formatting - kable & kableExtras
    # Table Formatting - gt & gtExtra
        # subsection: tutorials
    # Unused Tables with kable() 
    # Unused Tables for gt()
    # Unused gt() Code


################################################################################
#                            Remaining Tasks 
################################################################################


# Team formatting approval


################################################################################
#                         Load Libraries & Base Data          
################################################################################


### load libraries

# library(tidyverse)
# library(dplyr)
# library(flextable)               # Table package that may work in Word



### import data

# prey <- read.csv('data/processed/2019_basePrey.csv')


################################################################################
#                      Code Chunk: Creating Table 1
################################################################################


# This code chunk:
    # creates a sample location dataframe containing all metadata (detailed df = samp.detail)
    # restructures above dataframe into the format required for Table 1 (table 1 df = samp.table)
    # build and format Table 1


# 'samp.detail' dataframe: one row per sample location. Contains all metadata.
# 'samp.table' dataframe: summed total of all sample locations per depth stratum and assessment area


# Table 1 Dataframe structure will need:
    # Two categorical variables:
        # depth stratum
        # fishing zones (EAZ, WAZ, SFA4)
    # One numerical variable:
        # sum sample locations that falls within categories


# Dataframe transformations:
    # removing duplicates Trawl IDs so there is only one row per sample location
    # add column for number of trawls that occurred at each sample location
        # required for reformatting to samp.table dataframe
    # reformatted text in 'depth' category
    # export samp.detail dataframe data/processed/
    # restructure dataframe into the total sum of sample stations per depth structure within each assessment area (samp.table)
    # reorder assessment areas for table
    # export samp.table dataframe to data/processed/


################### Creating New Dataframe for Table 1 ###########################


### reformatting dataframe to have one row per sample location

#length(unique(prey$Trawl_ID))
    # checking how many unique sample locations occurred
    # 2019 data: 92 observations

samp <- prey
    # duplicating base prey dataframe

samp <- samp[!duplicated(samp$Trawl_ID),]
    # Retain one row per sampled Trawl ID by removing duplicated values for Trawl_ID

# CHECK NUMBER VARIABLES! Does it match # unique Trawl_ID sampled above

#str(samp) 
    # for 2019 data: 92 observations



### Adding column for the number of trawls per Trawl ID (or location)
    # necessary to calculate total trawls per depth per region

samp$trawl.sample <- 1 



### format depth column data for table
samp.detail <- samp %>%
  mutate(depth = str_replace(depth, "m$", ""))
    # if text ends in 'm', remove it



### Saving Dataframe Structure for samp.detail 

# str(samp.detail)

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



### Exporting Table 1 Detailed Sample Location Dataframe

write.csv(samp.detail, 
          file="data/processed/2019_T1_sampleLocation_detailed.csv",
          row.names = FALSE)



### create Table 1 final dataframe: sum survey locations per regions, per depth

samp.table <- dcast(samp.detail, depth~region, value.var="trawl.sample", sum) %>%
    # reformatting the data into the structure gt() will want
  relocate(SFA4, .after = WAZ)
    # moves the SFA4 column after WAZ. the '.' in .after is important!
    # used the dyplr cheat sheet and copilot

#sum(samp.table$EAZ + samp.table$SFA + samp.table$WAZ)
    # checking that we still have correct # of survey locations
    # 2019 data = 92 survey locations



### Saving Final Dataframe Structure 

#str(samp.table)

# 'data.frame':	5 obs. of  4 variables:
#   
# $ depth: chr  "100-200 " "200-300 " "300-400 " "400-500 " ...
# $ EAZ  : num  4 18 20 8 13
# $ WAZ  : num  0 3 4 2 0
# $ SFA4 : num  7 3 3 4 3
 

 
### Exporting Table 1 Final Dataframe
 
write.csv(samp.table, 
          file="data/processed/2019_T1_sampleLocation_final.csv", 
          row.names = FALSE)


###################    Building and Formatting Table 1    ######################

# flextable() is designed to work with Word and is cross-reference compatible



### Load Table 1 dataframe

# samp.table <- read.csv('data/processed/2019_T1_sampleLocation_final.csv')



### Create Table 1 - flextable package

flextable(samp.table) %>%
  
  set_header_labels(depth = "Depth Stratum (m)",        # renames columns
                    SFA4 = "SFA 4") %>%

  bg(bg = "lightgray", part = "header") %>%             # defines header colour
  bold(part = "header") %>%                             # header text bold
  
  bg(i = c(2,4), bg = "#EFEFEF", part = "body") %>%     # colours row 2 & 4 in table body
  
  align(part = "all", align = "center") %>%             # centers the entire document
  
  width(j = 1, width = 4, unit = "cm") %>%              # set width in column 1
  width(j = c(2,3,4), width = 3, unit = "cm") %>%       # set width in column 2-4
  
  set_caption(caption = "Number of stomach sampling locations within each depth stratum of the Eastern Assessment 
Zone (EAZ), Western Assessment Zone (WAZ), and Shrimp Fishing Area 4 (SFA 4).") 
      # adds caption that R Markdown incorporates


