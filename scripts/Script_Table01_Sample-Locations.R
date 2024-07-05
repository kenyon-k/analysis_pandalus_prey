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



### import data

prey <- read.csv('data/processed/2019_basePrey.csv')
pred <- read.csv('data/processed/2019_basePred.csv')


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



### Saving Final Dataframe Structure 



### Exporting Table 1 Dataframe



###################    Building and Formatting Table 1    ######################


# ..... Any Notes? .......


### Load Table 1 dataframe


### Table 1






