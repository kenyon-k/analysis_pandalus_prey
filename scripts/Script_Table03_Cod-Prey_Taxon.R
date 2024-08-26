################################################################################
################################################################################
#                Table 3 Atlantic Cod Prey Proportions by Taxon - DRAFT
################################################################################
################################################################################


# Author: Krista Kenyon (KAK)
# Date Created: Aug 22/2024
# Date Last Modified: Aug 22/2024 by KAK


# Document Purpose: Creating Table 3 for R Markdown document.
    # Working in code is simpler and faster here than in the R Markdown document
    # R Markdown places code results below chunks, which can mean a lot of scrolling


# R Markdown code pipes this code directly into the document.
    # !! Before knitting ensure to comment out all codes that produce console outputs except the table
    # e.g. str()


# Document Sections:
    # Remaining Tasks
    # Load Libraries & Base Data
    # Code Chunk: Creating Table 3
        # Subsection: Creating New Dataframe for Table 3
        # Subsection: Building and Formatting Table 3


# To see tutorials, key webpages, and key videos I used to create and format Table 1:
    # please see the 'Draft_Script_Table03-06_Prey-by-taxon.R' in the 'Draft Code' folder

# Draft Script notes include sites and tutorials that assisted me with:
    # ......
    # ......
    # ......


################################################################################
#                            Remaining Tasks 
################################################################################

# Worms database QA/QC
# prepping taxon data
# creating table database
# creating table
# clean code


################################################################################
#                         Load Libraries & Base Data          
################################################################################



### load libraries

library(tidyverse)
library(dplyr)
library(flextable)               # Table package that may work in Word



### import data

# !!!! This is first %W and %N so I may need to transfer that dataframe creation into this doc.

# prey <- read.csv('data/processed/2019_basePrey.csv')    # base prey dataframe

# ..... <- read.csv('........csv')                        # prey taxon dataframe


################################################################################
#                      Code Chunk: Creating Table 3
################################################################################


# This code chunk subsections:
    # ....................
    # ....................
    # ..................


# ......... df: one row per prey item. Details? .......
    # details? ........


# Table 2 Dataframe structure will need:
    # Two categorical variables:
        # predator categories
        # prey items grouped to genus-species
        # retain order
        # retain phylum
    # One numerical variable:
        # % weight
        # % count


# Dataframe transformations - Creating df:
    # ............
    # ............


# Dataframe transformations - Formatting for Table:
    # ..............
    # .............


################### Creating New Dataframe for Table 5 ###########################


############################### Below code is old !!!!!!!!!!!!!!

# below code is all one function that is 'broken up' with annotation notes

# 'stomach.ratio' dataframe will have the stomach data per predator species and predator length
    # stomach data = empty, full, and total stomachs sampled



### Creating new dataframe for Table 2

stomach.ratio  <- pred %>%               # create new dataframe based on 'pred'                    
    
  select(Prey_OS_ID,                    # subsets dataframe by selected columns
         pred.name, 
         length.range, 
         sampled.stomach) %>%


  
### Assigning Full vs Empty Stomachs
    # if I add these in, THEN I MUST FILTER STOMACHS FROM PREY DATABASE THAT ONLY HAVE THESE
    # THIS IS KEY - the pred df has indiscriminately removed duplicates. 
        # Fine for Empty Stomachs.
        # NOT fine when there could have been multiple prey/objects in stomachs
    # other potential 'Empty Stomachs' codes:
        # 2222 = Plant
        # unknown????? (if yes, 10746-10750)
        # other. 9981 (sand), 9982 (stone), 9983 (shells), 9993 (bait), 9995 (other offal), 10757 (mud)
        
  
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

  
#############   Formatting Dataframe Structure for Table    ####################



### Final Table Structure Formatting

# merge predator categories with the full, empty, total columns.
  
    # columns will now be 'stomach-predator' (e.g. Full-Greenland Halibut)  
    # rows will be predator length
  
  pivot_wider(names_from = pred.name,                       # categorical values to pull from rows to columns
              values_from = c(Full, Empty, Total)) %>%      # numerical values to fill in those columns

  
# Re-Ordering Columns 
  
  relocate(contains('cod'), .after = length.range) %>%     # moves any column with name containing 'cod' to the right of length.range
  relocate(contains('Greenland'), .after = contains('cod')) %>%
  relocate(contains('Red'), .after = contains('Green'))  %>%


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
                                    "")) %>%         # remove selected '0's
  
  
# Replace 0 and NA values with '-' 
    # I could not select and replace across the entire dataframe within dyplr
    # I had to use Base R for Step 2
    # Step 1: replacing 'NA' with 0 in dyplr (replace)
    # Step 2: replacing '0' with '-' in base R (lapply) and stringr (str_replace_all)
    

  replace(is.na(.), 0)                                 # if there are NA's, replace with 0

stomach.ratio <- data.frame(lapply(stomach.ratio, function(x){    # honestly - I don't know what above code does but it works
  str_replace_all(x,                                              # will be related to the code in the above line. But Idk who it actually works
                  "(?<!\\S)0(?!\\S)",                             # select '0' with nothing before or after
                  "-")                                            # replace with '-'
})) 



### Saving the stomach.ratio dataframe structure

# str(stomach.ratio) 

# 'data.frame':	18 obs. of  13 variables:

# $ length.range           : chr  "6-10" "11-15" "16-20" "21-25" ...
# $ Full_Atlantic.cod      : chr  "-" "-" "-" "-" ...
# $ Empty_Atlantic.cod     : chr  "-" "-" "-" "-" ...
# $ Total_Atlantic.cod     : chr  "-" "-" "-" "-" ...
# $ Full_Greenland.halibut : chr  "8" "19" "22" "20" ...
# $ Empty_Greenland.halibut: chr  "-" "11" "7" "13" ...
# $ Total_Greenland.halibut: chr  "8" "30" "29" "33" ...
# $ Full_Redfish           : chr  "1" "5" "9" "12" ...
# $ Empty_Redfish          : chr  "1" "-" "4" "11" ...
# $ Total_Redfish          : chr  "2" "5" "13" "23" ...
# $ Full_Skate             : chr  "-" "14" "29" "31" ...
# $ Empty_Skate            : chr  "1" "1" "1" "1" ...
# $ Total_Skate            : chr  "1" "15" "30" "32" ...



### Exporting stomach.ratio Dataframe

write.csv(stomach.ratio, 
          file="data/processed/2019_T2_stomach.ratio.csv",
          row.names = FALSE)                # removes auto-generated unique ID row


###################    Building and Formatting Table 2    ######################


### Load Table 2 dataframe

# stomach.ratio <- read.csv('data/processed/2019_T2_stomach.ratio.csv')

# confirm fish order is cod, halibut, redfish, skates. If yes - continue



### Create Table 2

flextable(stomach.ratio) %>%
  
  add_header_row(top = TRUE,                           # adding a header (spanner) on top of current header
                 values = c("", "Atlantic cod", "Greenland halibut", "Redfishes", "Skates"),
                 colwidths = c(1, 3, 3, 3, 3)) %>%     # number of columns wide each header cell is
  
 set_header_labels(length.range = "Size Class",        # renames columns
                   Full_Atlantic.cod = "Full",
                   Empty_Atlantic.cod = "Empty",
                   Total_Atlantic.cod = "Total",
                   Full_Greenland.halibut = "Full",
                   Empty_Greenland.halibut = "Empty",
                   Total_Greenland.halibut = "Total",
                   Full_Redfish = "Full",
                   Empty_Redfish = "Empty",
                   Total_Redfish = "Total",
                   Full_Skate = "Full",
                   Empty_Skate = "Empty",
                   Total_Skate = "Total") %>%
  
  bg(bg = "lightgray", part = "header") %>%               # defines header colour
  bold(part = "header") %>%                               # header text bold
  
  align(part = "all", align = "center")  %>%              # centers text throughout table
  
  vline(j=c(1, 4, 7, 10), part = "all") %>%               # adds vertical lines at defined columns across table
  
  bg(                                                  # adding shading every second line in table body
    i = seq(from = 2, to = nrow(stomach.ratio), by = 2),  # select every 2nd row until the max row number 
    bg = "#EFEFEF",                                       # color
    part = "body") %>%                                    # part of table to apply formatting
  
  width(j=1, width = 18, unit = "mm") %>%                 # define column width as narrow as possible
  width(j=c(2, 5, 8, 11), width = 12, unit = "mm") %>%   
  width(j=c(3, 6, 9, 12), width = 16, unit = "mm") %>%
  width(j=c(4, 7, 10, 13), width = 13, unit = "mm") %>%
  
  set_caption(                                         # manual caption formatting to insert footnote numbering
    caption = as_paragraph(                               # allows manual formatting in caption
      as_i("Number of full"),                             # italics
      as_i(as_sup("1")),                                  # superscript 1
      as_i(" empty, and total (full and emtpy) stomachs collected for Atlantic Cod ("), # spacing within "" matters!
      "Gadus morhua",
      as_i("), Greenland Halibut ("),
      "Reinhardtius hippoglossides",
      as_i("), redfishes ("),
      "Sebastes ",
      as_i("sp.), and skates ("),
      "Rajidae",
      as_i(") within each size class"),
      as_i(as_sup("2")),
      as_i(".")
      ))  %>%                        
  
  add_footer_lines(                                  # add footnotes compatible to Markdown
    as_paragraph(                                      # allows manual formatting in footnote
      as_sup("1"),                                     # superscript 1
      " A full stomach is any stomach that was not empty and contained prey items other than only parasites and/or only mucous."
      )) %>%
  
  add_footer_lines(                                 # add second footnote in new line
    as_paragraph(                     
      as_sup("2"),                                     
      " Total length (cm) was used to measure all predators."
      ))


