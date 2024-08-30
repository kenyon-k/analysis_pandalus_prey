################################################################################
################################################################################
#                Table 3 Atlantic Cod Prey Proportions by Taxon - DRAFT
################################################################################
################################################################################


# Author: Krista Kenyon (KAK)
# Date Created: Aug 27/2024
# Date Last Modified: Aug 27/2024 by KAK


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
library(officer)                 # extra Flextable formatting options



### import data

prey <- read.csv('data/processed/2019_basePrey.csv')    # base prey dataframe


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


################### Creating New Dataframe for Table 3 ###########################

##### Below code is from Figure 6


### Creating Base Percentages Dataframe

apple <- prey %>%                        # manipulate 'prey' df and save to new Table 2 Base df 
  
# Step 1: Subset Fig 6 Base df to desired columns  
  
  select(Prey_OS_ID,                     
         Prey_Detail_Code,               # needed to determine which stomachs full of parasites
         ScientificName_W,
         CommonName_DB,
         Phylum,
         Order,
         FishKey,
         PreyWt,
         Prey_Count,
         pred.name,                      # to split out data by predator
         length.range)  %>%               # including this for Figs 8-11 which use this base df 
  
  
# Step 2: remove rows with 'empty stomachs'. Leave unidentified items in for now.
      # by using %in% -> if there are every 'NA's, they will be retained
  
  filter(!Prey_OS_ID %in%             # selects rows that do not (!) contain the following 'in' them
           c('9998',                    # empty
             '9981',                    # sand
             '9982',                    # stone
             '9983',                    # shells
             '9987',                    # plant material
             '10757')) %>%              # mud

  
# Step 3: remove rows with 'parasitic' stomach contents
  filter(!Prey_Detail_Code %in%     # selects rows that do not (!) contain the following 'in' them
           c('40'))  %>%                  # parasites

  
# Step 4: create new column where Prey_OS_ID's are redefined to the 4 prey categories (Fig 6 fill)

    # shrimp P. borealis        (OS_ID code = 8111)
    # shrimp P. montagui        (OS_ID code = 8112)
    # shrimp Pandalus. sp.      (OS_ID code = 8110)
    # other (i.e. everything not Pandalus)

  mutate(prey.name = ifelse(Prey_OS_ID == 8111,         # create new column 'prey.name' with values based on logical check
                          "borealis",                        # value if logical check is TRUE
                          ifelse(Prey_OS_ID == 8112,          # if logical check is FALSE, begin second logical test
                                 "montagui",                  # value if second logical test is TRUE
                                 ifelse(Prey_OS_ID == 8110,   # if second logical test is FALSE, being third logical test
                                        "Pandalus",           # value if third logical test is TRUE
                                        "other")              # value if third logical test is FALSE
                          ))) %>%
  
# Step 5: Create new column to force 'other' prey category into solo column in Figure
  
  mutate(other.prey =                           # creating a new column called 'other.prey'
           prey.name == "other") %>%            # fill column with TRUE/FALSE on whether corresponding prey.name row is 'other'
  
  mutate(other.prey = ifelse(other.prey == FALSE, # making changes to 'other.prey'
                                                  # if 'other.prey' values is FALSE
                             "Shrimp",            # enter 'Shrimp'
                             "other")) %>%            # otherwise enter 'other'      


# Step 6: Format scientific names for table
    # if ScientificName_W has one word, it not identified to species level. That word is the level it was identified to.
    # We want all of those instances to be 'Unidentifiable xxxxx'
  
  mutate(ScientificName_W = ifelse(str_detect(ScientificName_W, pattern = '\\s') == TRUE,  # If there are are spaces ('\\s') in ScientificName_W string
                        ScientificName_W,                                                  # leave as is (should be 2 words)
                        gsub("^", paste0("Unidentifiable", " "), ScientificName_W)         # if not - paste 'Unidentifiable' in front of the current string
                        )) %>%
  
  mutate(ScientificName_W = ifelse(str_detect(ScientificName_W, pattern = 'Unidentifiable') == TRUE,  # If ScientificName_W contains 'Unidentifiable'
                            str_to_title(ScientificName_W),                                           # Have the first letter of each word be upper case
                            str_to_sentence(ScientificName_W)                                         # if not - only have the first letter of the first word be uppercase
                            ))


### Creating Table 3A Specific Dataframe



t3a <- apple  %>%     # manipulate 'apple' df and save to new Table 3a df 
  
  # Step 1: converts NAs within PreyWt to 0s   
  
  mutate(PreyWt = ifelse(is.na(PreyWt),          # if PreyWt is NA
                         0,                      # replace it with a 0
                         PreyWt))  %>%              # otherwise retain original value
  
  filter(pred.name == "Atlantic cod") %>%

  # Step 2: Sum Prey Weight by scientific name (ScientificName_W)
  
  group_by(ScientificName_W, Order, Phylum) %>%               # group by the categories we will want to retain after summarize(). First in list is what summarize() works from
  summarize(taxa.weight = sum(PreyWt), .groups = "keep") %>%     # create new column (taxa.weight) that sums PreyWt by scientific name
  
  # Step 3: Turn Weight into Percentage for Table
  ungroup() %>%                                       # group by the categories I that will feed into the below mutate     
  mutate(taxa.weight = (taxa.weight/sum(taxa.weight))*100)   # change 'prey.percent' column. OG values formatted into percentage per pred.name
  


##################################

# I'll still need to do % N and then merge together somehow

# But first I want to work on the table a bit




################################# Build the Table ##############################



# additional library needed for some table formating

install.packages('ftExtra')
library(ftExtra)

# final formatting that may be incorporated into the above code
cone <- t3a %>%
  mutate(Phylum = str_to_upper(Phylum)) %>%           # all caps text
  mutate(across('taxa.weight', round, 1)) %>%             # rounds to 1 decimal
  group_by(Phylum, Order)


# replacing NAs in Order with 'none'
cone$Order <- cone$Order %>%
  replace_na('none')

# adding another Phylum to test table structure
new_row <- data.frame(ScientificName_W= "Test test", Order= "Xxxxx", Phylum= "TEST", taxa.weight = 30)

test <- rbind(cone, new_row)



# re-grouping data (not sure if needed, but used for some test I did)
  
cone.grouped <- as_grouped_data(test, groups = c("Phylum", "Order"))   # group layers I'll want

 
# flextable time

as_flextable(cone.grouped,
             hide_grouplabel = TRUE) %>%              # removes labels flextables adds onto each group (keeps group name as is within dataset)
  
  set_header_labels(ScientificName_W = "Prey/Taxon",
                    taxa.weight = "Percent by Weight (%W)") %>% 
  
  bg(bg = "lightgray", part = "header") %>%               # defines header colour
  bold(bold = TRUE, part = "header") %>%              # header text bold
  
  bold(i = ~ !is.na(Phylum), j = 1) %>%                      # phylum text is bold
 # bold(i = ~ !is.na(Order), j = 1) %>%                       # Order text is bold
  style(i = ~ str_detect(ScientificName_W,                 # making only scientific names italic by:
                         pattern = 'Unidentifiable',       # string to search for is 'Unidentifiable'
                         negate = TRUE),                   # select any row that DOES NOT contain 'Unidentifiable'
        j = 1,
        pr_t = fp_text_default(italic=TRUE)) %>%                          # make them italic
  
  style(i = ~ !is.na(ScientificName_W),                       # Scientific Names in 1st column
        j = 1,                                                # first column only
        pr_p = fp_par(text.align = "left", padding.left = 15)) %>%    # add padding to left of selected text
  
  
  border(i = ~!is.na(Phylum),                                 # horizontal border per Phylum
         border.top = fp_border(color = "black"),             # placing the border on top of the row
         part = "body") %>%                                   # within the body of the table
  
  # hline_top(i = 4, border = fp_border(color = "black"), part = "body") %>%
  # border_inner_h(part = "body") %>%
  # 
  # hline(i = ~!is.na(Phylum),
  #       border.top = fp_border(color = "black"),
  #       part = "body") %>%               # adds horizontal lines below Phylum rows
  
  width(j = 1, width = 55, unit = "mm") %>%
  width(j = 2, width = 30, unit = "mm") # %>%
  
  set_caption(                                         # manual caption formatting to insert footnote numbering
    caption = as_paragraph(                               # allows manual formatting in caption
      as_i("Relative contribution, expressed as percent by weight (%W) and percent by number (%N), of different prey taxa found in the stomachs of Atlantic Cod ("),
      "Gadus morhua",             # regular text
      as_i(").")                  # italics
    )) # %>%                       

  
#  align(i = ~ !is.na(Order), align = "center")      # code from website that is interesting



############# Below is Table 2 old code

 stomach.ratio <- read.csv('data/processed/2019_T2_stomach.ratio.csv')

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


