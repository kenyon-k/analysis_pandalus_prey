################################################################################
################################################################################
#                   Table 1 Sample Locations per Depth - DRAFT
################################################################################
################################################################################


# Author: Krista Kenyon (KAK)
# Date Created: July 23/2024
# Date Last Modified: July 23/2024 by KAK


# Document Purpose: Creating Table 2 for R Markdown document.
    # Working in code is simpler and faster here than in the R Markdown document
    # R Markdown places code results below chunks, which can mean a lot of scrolling


# R Markdown code pipes this code directly into the document.
    # !! Before knitting ensure to comment out all codes that produce console outputs except the table
    # e.g. str()


# Document Sections:
    # Remaining Tasks
    # Load Libraries & Base Data
    # Code Chunk: Creating Table 2
        # Subsection: Creating New Dataframe for Table 2
        # Subsection: Building and Formatting Table 2


# To see tutorials, key webpages, and key videos I used to create and format Table 1:
    # please see the 'Draft_Script_Table02_Stomachs-Full-Empty.R' in the 'Draft Code' folder

# Draft Script notes include sites and tutorials that assisted me with:
    # .......
    # .......
    # .......


################################################################################
#                            Remaining Tasks 
################################################################################


# Create table dataframe

# Build and format dataframe

# Clean code across documents

# Team formatting approval


################################################################################
#                         Load Libraries & Base Data          
################################################################################


### load libraries

library(tidyverse)
library(dplyr)
library(flextable)               # Table package that may work in Word



### import data

pred <- read.csv('data/processed/2019_basePred.csv')
region.total <- read.csv('data/processed/2019_F4_region.total.csv')


################################################################################
#                      Code Chunk: Creating Table 1
################################################################################


# This code chunk:
    # Subsections: .......
    # Subsections: .......
    # Build and format Table 2

# Table 2 Dataframe structure will need:
    # Two categorical variables:
        # predator categories
        # size class
    # One numerical variable:
        # whether stomachs were full or empty

# Whether stomachs were full or empty will need to be broken down into three categories:
    # Full (Prey_OS_ID = not 9998)
    # Empty (Prey_OS_ID = 9998). What about Unknown Material? I'll want to 
    # Total


# Dataframe transformations:
    # Step 1 (###) ........
    # Step 2 (###) .......
    # Step 3 (###) .......

############### BELOW IS ALL OLD


# 'samp.detail' dataframe: one row per sample location. Contains all metadata.
# 'samp.table' dataframe: summed total of all sample locations per depth stratum and assessment area



################### Creating New Dataframe for Table 2 ###########################

# below code is all one function that is 'broken up' with annotation notes

# 'stomach.ratio' dataframe will have the stomach data per predator species and predator length
    # stomach data = empty, full, and total stomachs sampled



### Creating new dataframe for Table 2

stomach.ratio <- pred %>%               # create new dataframe based on 'pred'                    
    
  select(Prey_OS_ID,                    # subsets dataframe by selected columns
         pred.name, 
         length.range, 
         sampled.stomach) %>%


  
### Assigning Full vs Empty Stomachs
  
  mutate(Full = ifelse(Prey_OS_ID == 9998,     # create new column 'Full' with values based on logical check
                       0,                      # value if logical check is TRUE
                       1)) %>%                 # value if logical check is FALSE
 
   mutate(Empty = ifelse(Prey_OS_ID == 9998,   # Prey_OS_ID of 9998 = 'Empty'
                        1,
                        0))  %>%


### Assigning Groups for Table Layout 

# desiring stomach data summed and displayed by:
    # fish length group (column 1) and 
    # predator category group (rows) 
  
  group_by(length.range, pred.name) %>%        # group by specified categories


                                            
### Sum Stomach Data by Groups
  
# sums the specified values by the grouped categories
    # e.g. total 'full' stomachs by predator length and category
    # creates new dataframe that contains:
        # grouped variables (e.g. length.range, pred.name)
        # new columns containing the summarized values defined in summarise()

  summarise('Full' = sum(Full),                 # 'New Column' = sum('Old Column') 
            'Empty' = sum(Empty),
            'Total' = sum(sampled.stomach),
            .groups = "keep")  %>%              # tells R to keep current group structure
                                                # gives warning that goes into Markdown document if '.groups' not specified

  

### Final Table Structure Formatting

# merge predator categories with the full, empty, total columns.
  
    # columns will now be 'stomach-predator' (e.g. Full-Greenland Halibut)  
    # rows will be predator length
  
  pivot_wider(names_from = pred.name,                       # categorical values to pull from rows to columns
              values_from = c(Full, Empty, Total)) %>%      # numerical values to fill in those columns

# Re-Ordering Columns 
  
  relocate(contains('cod'), .after = length.range) %>%     # moves any column with name containing 'cod' to the right of length.range
  relocate(contains('Greenland'), .after = contains('cod')) %>%
  relocate(contains('Red'), .after = contains('Green')) # %>%


# Add length.range 0-5 row if none exist

  ifelse(length.range != "0-5",   # Prey_OS_ID of 9998 = 'Empty'
         add_row(length.range = '0-5')) 
         0)) %>%
 
  mutate(Full = ifelse(Prey_OS_ID == 9998,     # create new column 'Full' with values based on logical check
                       0,                      # value if logical check is TRUE
                       1)) #%>%                 # value if logical check is FALSE
  
  
t3 <- stomach.ratio   

t4 <- t3 %>%
  ungroup(length.range) %>%
  ifelse(length.range != "0-5",  
         add_row(length.range = '0-5'))
         )
  
  
  
  if(length.range != '0-5'){
    add_row(length.range = '0-5')}
t4$length.range[645]

# Re-Order Rows from Smallest to Largest Fish

    # length.range not naturally sorted because format is 6-10 vs 006-10
    # so I added 0s, sorted, then removed the extra 0s
  
  mutate(length.range = str_pad(length.range,     # code adds 0s to start of length.range string (6 becomes 006)
                                width=6,          # strings <6 characters will be padded until they =6 characters
                                pad = "0")) %>%   # padding with 0s
                                                  # width = 6 bumps all strings to ###- (006-, 016-, 141-)
  
  arrange(length.range) %>%                       # sort smallest to largest now that length.range strings equal character length before '-'
  
  mutate(length.range = str_replace(length.range, # removing the extra zeros for Table formatting
                                    "^0{1,}",     # select 1+ '0's at the beginning of strings
                                    ""))          # remove selected '0's

  


flextable(stomach.ratio)
  
# Re-Naming Columns
names(stomach.ratio)

# Replace 0 and NA values with '-'



### Saving the stomach.ratio dataframe structure

str(stomach.ratio) 



### Exporting Base Prey Dataframe

write.csv(prey, 
          file="data/processed/2019_T2_stomach.ratio.csv",
          row.names = FALSE)                # removes auto-generated unique ID row


###################    Building and Formatting Table 2    ######################


### Load Table 2 dataframe

# samp.table <- read.csv('data/processed/2019_T2_stomach.ratio.csv')



### Create Table 2

flextable(stomach.ratio)



########### Below is old


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



# I am trying to add striped colors to the table body based on odd/even rows
    # so far flextable hasn't liked any provided equations
    # below is one attempt

# row_odd <- seq_len(nrow(samp.table)) %% 2

