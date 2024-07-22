################################################################################
################################################################################
#                   Table 1 Sample Locations per Depth - DRAFT
################################################################################
################################################################################


# Author: Krista Kenyon (KAK)
# Date Created: July 5/2024
# Date Last Modified: July 22/2024 by KAK


# Document Purpose: Working document for creating Table 1.
    # Working in code is simpler and faster here than in the R Markdown document
        # R Markdown places code results below chunks, which can mean a lot of scrolling
        # Not worried about accidentally knitting the whole document when testing out different code chunks
    # Current version is the draft code.


# The only package that I have found that works in Word output is 'kable'
    # it has no real formatting options with kable()!!!!
        # kableExtra only works in pdf/HTML outputs. 
        # kableExtra Will stop Markdown export if used for Word
    # some packages will export partial formatting into Word, 
        # BUT it won't be able to be cross-referenced in text or with captions
        # This include gt() - which has great formatting options
    # With pdf/html exports, formatting and cross-references can be used:
        # gt()
        # kableExtra()
        # flextable()
    # Additional pdf/html options I didn't look into re cross-referencing include:
        # anything else listed here: https://rfortherestofus.com/2019/11/how-to-make-beautiful-tables-in-r/


# CURRENT PLAN: we want a word output.
    # those options with Markdown are terrible for formatting. BUT Cross-referencing works
    # If I insert a nicer table, then there won't be a caption or cross-referencing. That's not okay.
    # Step 1: Create Table01a with kable() for captions and cross-reference
    # Step 2: Create Table01b with gt() with formatting. Export as .jpeg in '/output'
        # Do not include in Markdown document via knitting
    # Step 3: post knit, replace Table01a in output with Table01b .jpeg 


# Document Sections:
    # Remaining Tasks
    # Load Libraries & Base Data
    # Code Chunk: Creating Table 1A Sample Locations : Functional Version
    # Code Chunk: Creating Table 1B Sample Locations : Pretty Version
    # Unused Code - Table 1C - Ideal Version



# Code Chunk Table 1A was transferred into the R Markdown: table-kable
# Code Chunk Table 1B was transferred into R Markdown: sample-loc


# To see tutorials, key webpages, and key videos I used to create and format Table 1:
    # please see the 'Draft_Script_Table01_sample-location.R' in the 'Draft Code' folder

# Draft Script notes include sites and tutorials that assisted me with:
    # Creating figure dataset
    # Piping script into R Markdown
    # Table formatting - kable and kableExtras
    # Table formatting - gt and gtExtras
    # Unused But Interesting gt() Code


################################################################################
#                            Remaining Tasks 
################################################################################


# Clean code across documents

# Team formatting approval

# If find formatting solution, update code


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
library(gtExtras)                # Additional table formatting options
library(flextable)               # Table package that may work in Word



### import data

prey <- read.csv('data/processed/2019_basePrey.csv')


################################################################################
#     Code Chunk: Creating Table 1A Sample Locations : Functional Version
################################################################################


# !!!!! After Markdown knits Word doc, replace Table 1 with 'output/Table01_pretty.jpeg'


# This code chunk:
    # creates a sample location dataframe containing all metadata (detailed df = samp.detail)
    # restructures above dataframe into the format required for Table 1 (table 1 df = samp.table)
    # build and format Table 1

# 'samp.detail' dataframe: one row per sample location. Contains all metadata.
# 'samp.table' dataframe: summed total of all sample locations per depth stratum and assessment area

# Dataframe transformations:
    # removing duplicates Trawl IDs so there is only one row per sample location
    # add column for number of trawls that occurred at each sample location
        # required for reformatting to samp.table dataframe
    # reformatted text in 'depth' category
    # export samp.detail dataframe data/processed/
    # restructure dataframe into the total sum of sample stations per depth structure within each assessment area (samp.table)
    # reorder assessment areas for table
    # export samp.table dataframe to data/processed/


# Table 1 Dataframe structure will need:
    # Two categorical variables:
        # depth stratum
        # fishing zones (EAZ, WAZ, SFA4)
    # One numerical variable:
        # sum sample locations that falls within categories


# There are 3 table versions: 1a, 1b, and 1c
    # There is only 1 table package compatable with Word: Table 1a
        # Formatting is terrible
        # BUT cross referencing happens
    # Table 1b has better formatting and is exported to 'output/Table01_pretty.jpeg'
        # After Markdown knits Word doc, replace Table 1a with Table 1b
    # Table 1c is my ideal formatting, but some formatting not retained when exported to Word


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


###################    Building and Formatting Table 1A    ######################


# Insert into R Markdown code chunk:

# This is the only package that I have found will export into Word and be cross-referenced
# It has terrible formatting options
# After the Markdown Word doc is knitted, replace this table (Table 1A) with the jpeg exported from Table 1B code



### Load Table 1 dataframe

samp.table <- read.csv('data/processed/2019_T1_sampleLocation_final.csv')



### Create Table 1A

table01a <- samp.table %>%
  knitr::kable(                                              # creating kable() table
    align = 'c',                                             # center alignment
    col.names = c("Depth Range", "EAZ", "WAZ", "SFA 4"),     # defines column names
    caption = '*Number of stomach sampling locations within each depth stratum of the Eastern ASsessment Zone (EAZ), Western Assessment Zone (WAZ), and Shrimp Fishing Area 4 (SFA 4)*'
        # caption - will be pulled into the R Markdown output. Not included in code chunk {fig.cap} equivalent like figures
      )                                                          

table01a


################################################################################
#     Code Chunk: Creating Table 1B Sample Locations : Pretty Version
################################################################################


# Insert into R Markdown code chunk: sample-loc 


# The below code creates a nicely formatted table.
# This version will be exported as a jpeg and will need to be inserted into the Markdown Word document post knit
# Table 1c (below) is my favorite but does not retain the spanner formatting upon export.
# The spanner is not really needed, so this version removes the spanner

# If a future me discovers how to retain the spanner formatting, then update to use Table 1C


######################    Table 1B - Pretty Version    ########################


### Load Table 1 dataframe

samp.table <- read.csv('data/processed/2019_T1_sampleLocation_final.csv')



### Create Table 1B

table01b <- samp.table %>%
  gt() %>%
  cols_label(depth ~ "Depth Stratum (m)",      # change column names
             SFA4 ~ "SFA 4")  %>%              

  opt_stylize(style = 1, color = "gray") %>%   # uses preset theme in the 'gray' color
                                               # I like elements of this theme (boarders and shading)
                                               # header text and fill color will be adjusted below

  cols_width(depth ~ px(160),                  # sets width of the 'depth' column to specified pixel number
             ends_with("Z") ~ px(130),         # sets width of columns ending in 'Z' to specified pixel number
             SFA4 ~ px(130)) %>%               # sets width of the 'SFA4' column to specified pixel number

  cols_align(align = c("center"),              # centers text
             columns = everything()) %>%       # alignment applies throughout table

  tab_style(                                   # forces styles onto cells
    style = list(                              # applies multiple styles
      cell_fill(color = "lightgray"),          # fill color to 'light gray', because the opt_stylize 'gray' looks black
      cell_text(weight = "bold",               # bold text
                color = "black")),             # make text black
    locations = list(cells_column_labels()))   # formatting applies to the column headers


### Exporting as jpeg

jpeg(filename="output/Table01_pretty.jpeg", width = 480, height = 480)
plot(table01b)
dev.off()

# replace jpeg() with png() if that becomes a desired format


################################################################################
#                    Unused Code - Table 1C - Ideal Version
################################################################################


# The below code creates my ideal table format & unused gt() arguments
# It only keeps the grey beside the spanner within R itself.
# I'm retaining the code in case future me finds a solution for exporting with my desired formatting


######################    Table 1C - Ideal Version    ########################


### Load Table 1 dataframe

samp.table <- read.csv('data/processed/2019_T1_sampleLocation_final.csv')



### Create Table 1C - full pretty formatting

table01c <- samp.table %>%
  gt() %>%
  cols_label(depth ~ "Depth Stratum (m)",
             SFA4 ~ "SFA 4")  %>%              # change column names

  tab_spanner(label = "Assessment Area",       # add a 'spanner' or sub-heading titled Assessment Area
              columns = EAZ:SFA4) %>%          # spanner is over columns EAZ through SFA4 (goes off column names from df itself - not what you renamed it)

  opt_stylize(style = 1, color = "gray") %>%   # uses preset theme in the 'gray' color
                                               # I like elements of this theme (boarders and shading)
                                               # header text and fill color will be adjusted below

  cols_width(depth ~ px(160),                  # sets width of the 'depth' column to specified pixel number
             ends_with("Z") ~ px(130),         # sets width of columns ending in 'Z' to specified pixel number
             SFA4 ~ px(130)) %>%               # sets width of the 'SFA4' column to specified pixel number

  cols_align(align = c("center"),              # centers text
             columns = everything()) %>%       # alignment applies throughout table

  tab_style(                                   # forces styles onto cells
    style = list(                              # applies multiple styles
      cell_fill(color = "lightgray"),          # fill color to 'light gray', because the opt_stylize 'gray' looks black
      cell_text(weight = "bold",               # bold text
                color = "black")),             # make text black
    locations = list(cells_column_spanners(),  #   # formatting applies to the column spanner
                     cells_column_labels()))   # formatting applies to the column headers

table01c


######################    Table 1D - Flextable    ########################


### Load Table 1 dataframe

samp.table <- read.csv('data/processed/2019_T1_sampleLocation_final.csv')



### Create Table 1D - flextable package

install.packages("officer")
library(officer)

set_flextable_defaults(hline_top(), 
                       hline_bottom(),
                       hline(i=2, ))

set_flextable_defaults(border.color = "darkgray", border.width = 1.5, border.style = "solid", odd_body = "#EFEFEF")
#table01d <- 
big_border <- fp_border(color = "darkgray", style = "solid" , width = 1.5)
    # officer() need for above function

row_odd <- seq_len(nrow(samp.table)) %% 2

  flextable(samp.table) %>%
   # hline_top(part = "all", border = big_border) %>%
  # add_header_row(values = c("","Assessment Area"),
  #                           colwidths = c(1,3)) %>%
    set_header_labels(depth = "Depth Stratum (m)",           # renames columns
                      SFA4 = "SFA 4") %>%
    bg(bg = "lightgray", part = "header") %>%            # defines header colour
    bold(part = "header") %>%                            # header text bold
    #bg(i = seq_len(nrow(samp.table)) %% 2, bg = "gray", part = "body") %>%
  align(part = "all", align = "center") %>%             # centers the entire document
  set_caption(caption = "Does Flextable Work") %>%           # adds caption that R Markdown incorporates
    width(j = 1, width = 4, unit = "cm") #  %>%
  theme_zebra()                                              # theme

table01d
?set_header_labels
?theme_fun
 
###################       Unused gt() arguments      ##########################

# below is coding if I want to make further boarder adjustments.
# current version feels cleaner than adding additional boarders.

# tab_style(style = list(
#   cell_borders(
#     sides = c("left", "right"),
#     color = "black"),
#     weight = px(30)),
#   locations = list(cells_body(), cells_column_labels(), cells_column_spanners()))


