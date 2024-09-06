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
                            )) %>%
  mutate(ScientificName_W = ifelse(ScientificName_W == "Unidentified material",
                                   "UNIDENTIFIABLE MATERIAL",
                                   ScientificName_W))


### Creating Table 3A Specific Dataframe



t3a <- apple  %>%     # manipulate 'apple' df and save to new Table 3a df 
  
  # Step 1: converts NAs within PreyWt to 0s   
  
  mutate(PreyWt = ifelse(is.na(PreyWt),          # if PreyWt is NA
                         0,                      # replace it with a 0
                         PreyWt))  %>%              # otherwise retain original value
  
  # Step X: split out by species
  
 # filter(pred.name == "Atlantic cod") %>%
  filter(pred.name == "Greenland halibut") %>%

  
  # Step 2: Sum Prey Weight by scientific name (ScientificName_W)
  
  group_by(ScientificName_W, Order, Phylum) %>%               # group by the categories we will want to retain after summarize(). First in list is what summarize() works from
  summarize(taxa.weight = sum(PreyWt), .groups = "keep") %>%     # create new column (taxa.weight) that sums PreyWt by scientific name
  
  
  # Step 3: Turn Weight into Percentage for Table
  ungroup() %>%                                       # group by the categories I that will feed into the below mutate     
  mutate(taxa.weight = (taxa.weight/sum(taxa.weight))*100)   # change 'prey.percent' column. OG values formatted into percentage per pred.name
  


##################################

# I'll still need to do % N and then merge together somehow

# But first I want to work on the table a bit



###############################################################################
####                      Build the Table                  ####################
###############################################################################



# final formatting that may be incorporated into the above code
cone <- t3a %>%
  mutate(Phylum = str_to_upper(Phylum)) %>%           # all caps text
  mutate(taxa.weight = round(taxa.weight, digits = 1)) %>%
  # mutate(Phylum = ifelse(ScientificName_W == "Unidentified material",
  #                        "UNDENTIFIABLE MATERIAL",
  #                        Phylum)) %>%
 # mutate(across(c('taxa.weight'), round, 1)) %>%             # rounds to 1 decimal
  group_by(Phylum, Order)

cone <- cone %>%
  mutate(taxa.weight = ifelse(taxa.weight < 0.1,
                              0.001,
                              taxa.weight)) %>%
  mutate(taxa.weight = str_replace(taxa.weight, pattern = "0.001", replacement = "< 0.1"))


## first step would be sorting the columns alphebetically

# replacing NAs in Order with 'none'
cone$Order <- cone$Order %>%
  replace_na('AAA')


cone <- cone %>%
  mutate(anchor = ifelse(str_detect(ScientificName_W, "Unidentifiable"),
                         1,
                         0)) %>%
  arrange(Phylum, Order, anchor, ScientificName_W, by_group = TRUE) %>%
  select(-anchor)


# assigning which rows that I will want to delete within the table
acorn <- as_grouped_data(cone, groups = c("Phylum", "Order")) # this data structure matches what flextable will produce naturally

nut <- acorn %>%
  mutate(dups = duplicated(Phylum, incomparables = NA)) %>%
  mutate(index = row_number()) %>%
  filter(dups == TRUE) %>%
  .$index

# Removes empty phylum row of Unidentifiable Material.
    # We are formatting ScientificName_W -> 'Unidentifiable Material' to look like it is a phylum inside the table, while still displaying %W and %N within the same row
acorn <- acorn[rowSums(is.na(acorn)) != ncol(acorn), ]   # removes row only if the entire row consists of 'NA'

# this works to select appropriate rows, but does not work if there is only ONE Phylum.
# Will need to adjust for that

chip <- acorn %>%
  mutate(dups = duplicated(Phylum, incomparables = NA))  %>%          
  mutate(index = (row_number()))  %>%               # will be able to remove in the future. Was handy to ensure correct rows are being removed
  slice(-c(nut))  %>%                              # removes rows that are duplicate Phylums
  filter((Order != 'AAA') %>% replace_na(TRUE))  %>%  # removes rows that have 'blank' Orders
  mutate(index.final = (row_number()-1))  %>%       # adds row numbers now that data structure matches final table structure. -1 will force the row to be the one above the phylum
# enter a code that replaces index.final = 0 with the last row number (look at fish.length separation for ideas)
  mutate(index.final = ifelse(index.final == 0,         # if index.final == 0
                              max(index.final+1),               # replace it with the highest value in index (should be the last row number)
                              index.final))   %>%            # otherwise keep values as they are
  # then if there is only one Phylum, the line will be placed at the bottom which is fine.
  # And the bottom row should never be a Phylum, so it should add a duplicate value which may or may not mess with things
  
  # switch below code so that it removes rows where Phylum = NAs
  
  filter(Phylum != 'AAA') %>%
  .$index.final                                    # gives final values

### Adding line above Unidentifiable Material

pizza <- acorn %>%
  mutate(dups = duplicated(Phylum, incomparables = NA))  %>%          
  mutate(index = (row_number()))  %>%               # will be able to remove in the future. Was handy to ensure correct rows are being removed
  slice(-c(nut))  %>%                              # removes rows that are duplicate Phylums
  filter((Order != 'AAA') %>% replace_na(TRUE)) %>%
  mutate(index.final = ifelse(ScientificName_W == "UNIDENTIFIABLE MATERIAL",
                              row_number()-1,
                              row_number())) %>%
  top_n(1, index.final) %>%
  distinct(index.final) %>%
  .$index.final
  


##########################      Building the Table      ########################

### Creating the table

as_flextable(acorn,                                   # dataframe we are using
             hide_grouplabel = TRUE) %>%              # flextable auto-adds additional labels to group heading rows. This turns that off

## Formatting the header    
  set_header_labels(ScientificName_W = "Prey/Taxon",         # rename column titles
                     taxa.weight = "Percent by Weight (%W)")  %>% 
  
  bg(bg = "lightgray", part = "header") %>%                   # defines header colour
  bold(bold = TRUE, part = "header") %>%                      # header text bold

## Removing duplicate or empty group and subgroup heading rows  
  delete_rows(i = nut, part = 'body')  %>%                     # removes duplicate Phylum rows. Function is above table
  delete_rows(i = ~ str_detect(Order, pattern = 'AAA')) %>%    # removes empty Order rows. In earlier code, these rows were filled with 'AAA' so that they would first order when sorted alphabetically. This way they will always be directly below the Phylum group header. 

## Formatting table text
  bold(i = ~ !is.na(Phylum), j = 1) %>%                      # phylum text is bold
  bold(i = ~ !is.na(Order), j = 1) %>%                       # Order text is bold
  bold(i = ~str_detect(ScientificName_W,                      # bold 'Unidentifiable material'
                       pattern = "UNIDENTIFIABLE MATERIAL"),
       j = 1) %>%
  align(j = 2, align = "center", part = "body")   %>%        # centering %W and %N columns
    
    # making scientific names italic
  style(i = ~ str_detect(ScientificName_W,                            # in ScientificName_W              
                         pattern = 'Unidentifiable',                  # find the text pattern 'Unidentifiable'
                         negate = TRUE),                              # select any row that DOES NOT contain 'Unidentifiable'
        j = 1,                                                        # formatting changes only apply to the column 1
        pr_t = fp_text_default(italic=TRUE))  %>%                     # make text italic
  
    # making Unidentifiable material non-italic  
  style(i = ~ str_detect(ScientificName_W,                            # in ScientificName_W              
                         pattern = "UNIDENTIFIABLE MATERIAL"),        # find and select the text pattern 'UNIDENTIFIABLE MATERIAL'
        j = 1,                                                        # formatting changes only applies for column 1
        pr_t = fp_text_default(italic=FALSE)) %>%                     # make text not italic
  
    # Indenting all scientific names other than 'unidentifiable material'
  style(i = ~ str_detect(ScientificName_W,                            # in ScientificName_W
                         pattern = "UNIDENTIFIABLE MATERIAL",         # find the text pattern 'UNIDENTIFIABLE MATERIAL'
                         negate = TRUE),                              # select everything else
        j = 1,                                                        # formatting changes only applies for column 1
        pr_p = fp_par(text.align = "left", padding.left = 15)) %>%    # add padding to left of selected text

## Adding horizontal lines to the table body
   hline(i = chip, part = "body")  %>%                       # inserts border line above Phylum. Function code is above the table
   hline(i = pizza, part = "body") %>%                       # inserts border line above Unidentifiable Material if it is present. If Unidentifiable material is not present, a line is added at the bottom of the table. Function code is above the table
  hline_bottom(border = fp_border(color = "dimgray", width = 1.5), part = "body") %>%  # adds a border to the bottom of the table. Formula to add lines above phylum overrides the normal bottom border. This was my solution to that issue.

## Setting column widths
  width(j = 1, width = 65, unit = "mm") %>%                  # sets width of column 1. All scientific names should fit on 1 line
  width(j = 2, width = 30, unit = "mm") %>%                  # sets width of column 2.

## adding a caption compatible with Markdown
  set_caption(caption = as_paragraph(                       # allows manual formatting in caption. Whatever spacing is included within "" is what will occure in the caption. So "text " vs "text" matters.
    as_i("Relative contribution, expressed as percent by weight (%W) and percent by number (%N), of different prey taxa found in the stomachs of Atlantic Cod ("),
    "Gadus morhua",                                   # regular text
    as_i(").")                                        # as_i() = italics
    ))

  
  
########################### Code I used Trying to Figure out Formatting



# pred <- pred[!duplicated(pred$FishKey),]
# 
# 
# anyDuplicated(burnt.toast$taxa.weight)
# 
# 
# burnt.toast %>%
#   filter(!duplicated(Phylum, fromLast = FALSE))
# 
# 
# duplicated(burnt.toast$Phylum)
# 
# burnt.toast[duplicated(burnt.toast$Phylum),]


  # adding another Phylum to test table structure
  # new_row <- data.frame(ScientificName_W= "Test test", Order= "Xxxxx", Phylum= "TEST", taxa.weight = 30)
  # 
  # test <- rbind(cone, new_row) %>%
  #   
  # 
  # flower <- test %>%
  #   arrange(Phylum, Order, by_group = TRUE)
  
  
  
  # re-grouping data (not sure if needed, but used for some test I did)
  
  # g.group <-   as_grouped_data(test, groups = c("Phylum"))               # testing data structure for why 2 groups repeats
  # cone.grouped <- as_grouped_data(test, groups = c("Phylum", "Order"))   # group layers I'll want
  # 
  # cone.grouped <- cone.grouped %>%
  #   arrange(Phylum, Order, by_group = TRUE)
  # 
  # ftest <- cone.grouped %>%
  #   mutate(Order = c("Decapoda", "", "", "", "", "", "", "", "", "XXXX", "", "")) %>%
  #  # mutate(Order = c("", "Decapoda", "", "", "", "", "", "", "", "", "Xxxx", "")) %>%
  #   mutate(Phylum = c("ANTHROPODA", "", "", "", "", "", "", "", "", "TEST", "", ""))
  # #test.order <- data.frame(Order = c("", "Decapoda", "", "", "", "", "", "", "", "", "Xxxx", ""))
  
  #ftest <- cbind(cone.grouped, test.order)
  
  # ftest <- ftest %>%
  #   group_by(Phylum, Order) #%>%
  # 
  # # gtest <- ftest[rowSums(is.na(ftest)) != ncol(ftest),]
  # gtest <- ftest[-c(2, 4,5,7,8,11),]  
  # 
  # gtest <- gtest %>%
  #   mutate(ScientificName_W = str_replace(ScientificName_W, pattern = "NA", replacement = ""))
  
  # htest <- gtest[!is.na(gtest)]  
  # ?mutate
  
  ############ Create from scratch a test df from as_flextable to see if it works
  # egg <-  data.frame(
  #   Phylum = c("ANTHROPODA", "", "", "TEST", ""),
  #   Order = c("Decapoda", "", "", "XXXX", ""),
  #   ScientificName_W = c("Name 1", "Name 2", "Name 3", "", "Name 4"),
  #   taxa.weight = c("1.5", "2.5", "5.8", "", "10.5")
  # )
  # 
  # egg <- egg %>%
  #   group_by(Phylum, Order)
  
  
  ############ Create from scratch a dataframe with no blanks
  # toast <-  data.frame(
  #   Phylum = c("ANTHROPODA", "ANTHROPODA", "ANTHROPODA", "TEST"),
  #   Order = c("Decapoda", "Decapoda", "Decapoda", "XXXX"),
  #   ScientificName_W = c("Name 1", "Name 2", "Name 3", "Name 4"),
  #   taxa.weight = c("1.5", "2.5", "5.8", "10.5")
  # )
  # 
  # toast <- toast %>%
  #   group_by(Phylum, Order)
  
  
  
  
  
  ############ Create from scratch a dataframe with no blanks but multiple orders
  # burnt.toast <-  data.frame(
  #   Phylum = c("ARTHROPODA", "ARTHROPODA", "ARTHROPODA", "TEST", "TEST"),
  #   Order = c("Decapoda", "Decapoda", "None", "Testing", "Pass"),
  #   ScientificName_W = c("Name 1", "Name 2", "Name 3", "Name 4", "Name 5"),
  #   taxa.weight = c("1.5", "2.5", "5.8", "10.5", "11")
  # )
  # 
  # jam.toast <- as_grouped_data(burnt.toast, groups = c("Phylum", "Order"))
  # 
  # !duplicated(jam.toast$Phylum)
  # 
  # burnt.toast %>%
  #   rowwise() %>%
  #   mutate(dups = anyDuplicated(na.omit(c(Phylum, Order, ScientificName_W, taxa.weight))))  %>%
  #   ungroup %>%
  #   mutate(index = row_number()) %>%
  #   filter(dups > 0) %>%
  #   .$index
  
  
  
  
  # burnt.toast <- burnt.toast %>%
  #   group_by(Phylum, Order)
  # 
  # install.packages('hablar')
  # library(hablar)
  # ?find_duplicates
  
  
  as_flextable(...)
  
  # delete_rows(i = 5, part = 'body') %>%             # this works but I would prefer a formula
  
  # delete_rows(i = ~ select(unique(Phylum)), part = "body") %>%
  
  # delete_rows(i = ~ !duplicated(Phylum, by = Phylum, fromLast = TRUE), part = "body") %>%
  #delete_rows(i = ~ duplicated(Phylum), by = Phylum, part = "body") %>%
  # delete_rows(i = ~ burnt.toast[duplicated(burnt.toast$Phylum),], part = "body") %>%
  # delete_rows(i = ~ duplicated(Phylum) == TRUE, part = "body") %>%
  # delete_rows(i funct ~ select(unique(Phylum)), part = "body") %>%
  # delete_rows(i = ~ filter(distinct(Phylum, .keep_all = TRUE), part = "body")) %>%
  
  
  # hline_top(i = 4, border = fp_border(color = "black"), part = "body") %>%
  # border_inner_h(part = "body") %>%
  # 
  # hline(i = ~!is.na(Phylum),
  #       border.top = fp_border(color = "black"),
  #       part = "body") %>%               # adds horizontal lines below Phylum rows
  
  #  align(i = ~ !is.na(Order), align = "center")      # code from website that is interesting
