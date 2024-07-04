################################################################################
################################################################################
#                   Figure 5 Stomach by Length & Depth - DRAFT
################################################################################
################################################################################


# Author: Krista Kenyon (KAK)
# Date Created: June 17/2024
# Date Last Modified: July 3/2024 by KAK

# Document Purpose: Working document for creating the Figure 5 paneled bar charts.
    # Working in code is simpler and faster here than in the R Markdown document
        # R Markdown places code results below chunks, which can mean a lot of scrolling
        # Not worried about accidentally knitting the whole document when testing out different code chunks
    # Current version is up to date clean code.

# Document Sections:
    # Remaining Tasks
    # Load Libraries & Base Data
    # Code Chunk: Creating the Paneled Bar Charts examining stomachs sampled per fish length
    # Additional Notes

# Code Chunk was transferred into the R Markdown Chunk 5: stomach-length

# To see tutorials, key webpages, and key videos I used to create and format the 
# bar chart, please see the 'Draft_Script_Fig05_Stomach-by-Length.R' in the 'Draft Code' folder


################################################################################
#                            Remaining Tasks 
################################################################################


# Step 1: Select which chart style I will go with

# Step 2: Save base prey and pred dataframe outputs for cleaner code

# Step 3: Clean up Document Code

# Step 4: Clean up Draft Script doc
  
# Step 5: Decide if I want to mess around with panel boarders right now



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

pred <- read.csv('data/processed/2019_basePred.csv')


################################################################################
#              Code Chunk: Creating Fig 5 Paneled-Bar Chart 
################################################################################


# The purpose of this code chunk is to create Figure 5

# The sections of this code chunk are:
    # Creating and Formatting Figure 5 paneled-bar chart

# No creation of a new dataframe is required for Fig 5


# This figure will detail the total sum of stomachs sampled per size class within each depth stratum.
    # Different panel for each predator category

# Figure structure will need:
    # Three categorical variables:
        # x-axis: predator length
        # fill: depth range
        # facet: predator category
    # One numerical variable:
        # y-axis: sum stomach samples that falls within categories


# If desired, old/test code and additional links to tutorial youtubes/webpages I used to create Figure 5 are recorded in (scripts/draft code/Draft_Script_Fig5_Stomach-by-Length.R)

# Those notes include sites and tutorials that assisted me with:
    # Data structure for figure
    # Building faceted stacked bar charts
    # Formatting faceted stacked bar charts
    # Unused but Interesting ggplot2 arguments
    # Creating a Base Theme for R Markdown Plots


############### Building and Formatting the Faceted Stacked Bar Chart ##################

# So far I haven't figured out how to code the exact same formatting as template

# HOWEVER - I'm not the biggest fan of their formatting for this Figure anyway.

# I'm putting a pause on fine-tuning the figure format for now
    # I will return to this after completing the later figures to have consistent formatting
    # the template structure doesn't really have consistent formatting across all figures



### Faceted Stacked Bar Chart


# Below are the different options:


#### Option A: Fish Panel Titles ####

# This option has 1 legend and uses fish names to distinguish each panel

g05<- ggplot(pred, aes(reorder(length.range, Length))) +  
  # orders the X-axis categories based on numerical length of fish
  # basically makes ggplot see and order the x-axis as numbers vs text
  
  geom_bar(aes(fill = depth),
           position = "stack",
           stat="count",
           width = 0.4) +   # changes the width of the bars 
  
  facet_wrap(~ pred.name, 
             nrow = 5, 
             scale = "free_y",
             axes = "all_x") +

  
  # scale_fill_viridis(discrete = TRUE,   # color-blind friendly palet for discrete data
  #                    direction = -1) +  # flips the order of the colours
  
  theme_minimal() + 
  
  theme(axis.text.x = element_text(angle = 45,hjust = 1, size = 8 ),
        axis.title = element_text(size = 8),
        axis.title.y = element_text(vjust = +3),                 # pulls y-axis title away from chart
        axis.title.x = element_text(vjust = -2),                 # pulls x-axis title away from chart
        legend.title = element_blank(),                          # removes legend title
        # common.legend = TRUE,
        legend.key.size = unit(3, "mm"),                         # adjusts width of legend color symbols
        legend.key.height = unit(3, "mm"),                       # adjusts height of legend color symbols
        legend.key.spacing.y = unit(1, "mm"),                    # defines space between legend items
        panel.grid.minor = element_blank(),                      # removes all minor grid-lines
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(13, 'point'),                         # adds space between panels
        # panel.border = element_rect(fill = "transparent",        # needed to add the boarder       
        #                            color = "black", linewidth = 1.0), # boarder color and width
        strip.text = element_text(face = "bold",      # bolds the panel titles
                                  hjust = 0)) +       # pulls panel titles to the top left
  
  labs(x = "Predator Length (cm)",
       y = "Number of Stomachs")

g05


#### Option B: a/b/c/d Panel Titles ####


# This option has 1 legend and uses a/b/c/d for each panel

g05 +  facet_wrap(~ pred.name, 
                  nrow = 5, 
                  scale = "free_y",
                  axes = "all_x",
                  labeller = labeller(pred.name = 
                                        c("Atlantic cod" = "a",
                                          "Greenland halibut" = "b",
                                          "Redfish" = "c",
                                          "Skate" = "d")))


#### Option C: Multiple Legends ###

# This option has individual legends and uses a/b/c/d for each panel
# So far I can't get each individual legends to show the 0'd values
    # because each panel doesn't include any 0'd values


# Testing using Grid-arrange

library(gridExtra)
library(ggpubr)

sp <- split(pred,f = pred$pred.name)

p01 <- ggplot(sp$`Atlantic cod`, aes(x = length.range, fill = depth)) +
  
  geom_bar(position = "stack",
           stat="count",
           width = 0.4) +   # changes the width of the bars 
  
  scale_x_discrete(limits = c("6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "36-40",
                              "41-45", "46-50", "51-55", "56-60", "61-65", "66-70", 
                              "71-75", "76-80", "81-85", "140+")) +
  
  scale_fill_viridis(discrete = TRUE,   # color-blind friendly palet for discrete data
                     direction = -1) +  # flips the order of the colours
  
  # scale_fill_manual(values = c("red", "blue", "green", "purple", "pink"),
  #                   breaks = c("100-200 m", "200-300 m", "300-400 m", "300-400 m", "500-750 m"),
  #                   labels = c("100-200 m", "200-300 m", "300-400 m", "300-400 m", "500-750 m"),
  #                   drop = FALSE) +
  
  # scale_fill_manual(values = depth, breaks = c("100-200 m", "200-300 m", "300-400 m", "300-400 m", "500-750 m")) +
  theme_minimal() + 
  
  theme(axis.text.x = element_text(angle = 45,hjust = 1, size = 8 ),
        axis.title = element_text(size = 8),
        axis.title.y = element_text(vjust = +3),                 # pulls y-axis title away from chart
        axis.title.x = element_text(vjust = -2),                 # pulls x-axis title away from chart
        legend.title = element_blank(),                          # removes legend title
        legend.key.size = unit(3, "mm"),                         # adjusts width of legend color symbols
        legend.key.height = unit(3, "mm"),                       # adjusts height of legend color symbols
        legend.key.spacing.y = unit(1, "mm"),                    # defines space between legend items
        panel.grid.minor = element_blank(),                      # removes all minor grid-lines
        panel.grid.major.x = element_blank(),
        plot.margin = margin(t=25, r=8, b=10, l=9)) +
  
  labs(x = "Predator Length (cm)",
       y = "Number of Stomachs")


p02 <- p01 %+% sp$`Greenland halibut`
p03 <- p01 %+% sp$Redfish
p04 <- p01 %+% sp$Skate

ggarrange(p01, p02, p03, p04, 
          nrow = 4, labels = "auto", vjust = 0.8)

?ggarrange


#### Option D: Testing Facet_Grid ####


# This option makes many graphs showing depth and fish species individually.

# This option has 1 legend and uses the predator category as a title vs a/b/c/d

# look at for panel labels and outlines in following webpage: https://stackoverflow.com/questions/67098543/facet-wrap-labels-as-panel-labels-in-ggplot
# look at for labels per panel https://stackoverflow.com/questions/14840542/place-a-legend-for-each-facet-wrap-grid-in-ggplot2
 
# code below:

# ggplot(pred, aes(reorder(length.range, Length))) +
# 
#   geom_bar(aes(fill = depth),
#            position = "stack",
#            stat="count",
#            width = 0.4) +
# 
#   facet_grid(depth ~ pred.name) #,
#              # scale = "free_y",
#              # axes = "all_x")
# 
# theme_minimal() +
# 
#   theme(axis.text.x = element_text(angle = 45,hjust = 1, size = 8 ),
#         axis.title = element_text(size = 8),
#         axis.title.y = element_text(vjust = +3),                 # pulls y-axis title away from chart
#         axis.title.x = element_text(vjust = -2),                 # pulls x-axis title away from chart
#         legend.title = element_blank(),                          # removes legend title
#         common.legend = TRUE,
#         legend.key.size = unit(3, "mm"),                         # adjusts width of legend color symbols
#         legend.key.height = unit(3, "mm"),                       # adjusts height of legend color symbols
#         legend.key.spacing.y = unit(1, "mm"),                    # defines space between legend items
#         panel.grid.minor = element_blank(),                      # removes all minor grid-lines
#         panel.grid.major.x = element_blank(),
#         panel.spacing = unit(13, 'point'),                         # adds space between panels
#         # panel.border = element_rect(fill = "transparent",        # needed to add the boarder
#         #                            color = "black", linewidth = 1.0), # boarder color and width
#         strip.text = element_text(face = "bold",      # bolds the panel titles
#                                   hjust = 0)) +       # pulls panel titles to the top left
# 
#   labs(x = "Predator Length (cm)",
#        y = "Number of Stomachs")



#### Option E: ggarrange long format ####


# Using dplyr and ggarrange - this is getting messy fast. Not a fan
# Option B is the cleaned/simplified version of below code.


# p1 <- pred %>% filter(pred.name == "Atlantic cod") %>% 
#   ggplot(aes(x = length.range, fill = depth)) +
#   geom_bar(position = "stack",
#            stat = "count") +
#   scale_x_discrete(limits = c("6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "36-40",
#                               "41-45", "46-50", "51-55", "56-60", "61-65", "66-70", 
#                               "71-75", "76-80", "81-85", "140+")) +
#   theme_minimal() + 
#   
#   theme(axis.text.x = element_text(angle = 45,hjust = 1 ),
#         legend.title = element_blank(),                          # removes legend title
#         legend.key.size = unit(3, "mm"),                         # adjusts width of legend color symbols
#         legend.key.height = unit(3, "mm"),                       # adjusts height of legend color symbols
#         legend.key.spacing.y = unit(1, "mm"),                    # defines space between legend items
#         panel.grid.minor = element_blank(),                      # removes all minor grid-lines
#         panel.grid.major.x = element_blank())
# 
# 
# p2 <- pred %>% filter(pred.name == "Greenland halibut") %>% 
#   ggplot(aes(x = length.range, fill = depth)) +
#   geom_bar(position = "stack",
#            stat = "count") +
#   scale_x_discrete(limits = c("6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "36-40",
#                               "41-45", "46-50", "51-55", "56-60", "61-65", "66-70", 
#                               "71-75", "76-80", "81-85", "140+")) +
#   theme_minimal() + 
#   
#   theme(axis.text.x = element_text(angle = 45,hjust = 1 ),
#         legend.title = element_blank(),                          # removes legend title
#         legend.key.size = unit(3, "mm"),                         # adjusts width of legend color symbols
#         legend.key.height = unit(3, "mm"),                       # adjusts height of legend color symbols
#         legend.key.spacing.y = unit(1, "mm"),                    # defines space between legend items
#         panel.grid.minor = element_blank(),                      # removes all minor grid-lines
#         panel.grid.major.x = element_blank())
# 
# 
# ggarrange(p1, p2, nrow = 2, labels = "AUTO")  






