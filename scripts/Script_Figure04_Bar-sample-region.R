################################################################################
################################################################################
#                   Figure 4 Bar Samples per Zone - FINAL
################################################################################
################################################################################


# Author: Krista Kenyon (KAK)
# Date Created: May 10/2024
# Date Last Modified: June 14/2024 by KAK

# Document Purpose: Working document for creating the Figure 4 grouped bar chart.
    # Working in code is simpler and faster here than in the R Markdown document
        # R Markdown places code results below chunks, which can mean a lot of scrolling
        # Not worried about accidentally knitting the whole document when testing out different code chunks
    # Current version is up to date clean code.

# Document Sections:
    # Remaining Tasks
    # Load Libraries & Base Data
    # Code Chunk: Creating the Grouped Bar Figure
    # Additional Notes

# Code Chunk was transferred into the R Markdown Chunk 4: samples-zone


# To see tutorials, key webpages, and key videos I used to create and format the 
# bar chart, please see the 'Draft_Script_Fig04_Bar-sample-region.R' in the 'Draft Code' folder


################################################################################
#                            Remaining Tasks 
################################################################################


# cleaning up the code


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
#                Code Chunk: Creating Fig 4 Grouped-Bar Chart 
################################################################################


# The purpose of this code chunk is to create Figure 4

# The sections of this code chunk are:
    # Creating a new dataframe for Figure 4
    # Creating and Formatting Figure 4 grouped-bar chart


# If desired, additional notes and links to tutorial youtubes/webpages I used to create Figure 4 are recorded in (scripts/draft code/Draft_Script_Fig4_Bar-sample-region.R)

# Those notes include sites and tutorials that assisted me with:
    # Creating figure dataset
    # GGPLOT2 Code for Grouped Pie Charts
    # additional ggplot2 arguments not used in this chart


################### Creating New Dataframe for Fig 4 ###########################


# This dataframe will detail the total sum of stomachs sampled per predator category in each region

# All code to check dataframe structure are commented out. If not they will produce outputs into the report.

# New Dataframe structure will need:
    # Two categorical variables:
        # x-axis: regions
        # fill: predator category
    # One numerical variable: 
        # y-axis: total samples per predator species per region

# library(reshape2) 
    # included in code chunk 'r setup'



### create Fig 4 dataframe: sum stomach samples per species, per region

wide.data <- dcast(pred, region~pred.name, value.var="sampled.stomach", sum)
    # creates a dataframe in a 'wide format' and we will want a 'long format' dataframe
    # Tutorial 1 in Fig 4 draft code doc

# wide.data



### converting 'wide format' to 'long format'

region.total <- melt(data = wide.data,                       # melt() converts from wide format (species spread across rows) to long format (species compiled into one row)
                     id.vars = "region",                     # id.vars : vector of variables I want to stack
                     measured.vars = c("Atlantic cod",       # measured.vars : identifies the columns that have the data I want to stack
                                       "Greenland halibut", 
                                       "Redfish", "Skate"),
                     variable.name = "pred.name",            # variable.name : what I want to call the stacked column
                     value.name = "stomach.sum")             # value.name : name of the column of my stacked measurements in my output data frame
    # Tutorial 2 in Fig 4 draft code doc
    
# region.total



### Checking Final Database Structure

# str(region.total)



### Saving Final Database Structure 

# 'data.frame':	12 obs. of  3 variables:
# $ region     : chr  "EAZ" "SFA4" "WAZ" "EAZ" ...
# $ pred.name  : Factor w/ 4 levels "Atlantic cod",..: 1 1 1 2 2 2 3 3 3 4 ...
# $ stomach.sum: num  2 1 0 198 92 43 34 28 3 120 ...



### Exporting Figure 4 Dataframe

write.csv(pred, file="data/processed/2019_F4_region.total.csv")


############### Building and Formatting the Grouped Bar Chart ##################


### Load Fig 4 dataframe

#region.total <- read.csv('data/processed/2019_F4_region.total.csv')



### Grouped Bar Chart

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


