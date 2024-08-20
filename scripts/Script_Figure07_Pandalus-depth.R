################################################################################
################################################################################
#                   Figure 7 Pandalus Percent - DRAFT
################################################################################
################################################################################


# Author: Daniel Enright (DTE)
# Date Created: Aug 16/2024
# Date Last Modified: Aug 20/2024 by DTE


# Document Purpose: Creating Figure 7 for R Markdown document.
# Working in code is simpler and faster here than in the R Markdown document
# R Markdown places code results below chunks, which can mean a lot of scrolling


# R Markdown code pipes this code directly into the document.
# !! Before knitting ensure to comment out all codes that produce console outputs except the table
# e.g. str()


# Document Sections:
# Remaining Tasks
# Load Libraries & Base Data
# Code Chunk: Creating Figure 7
# Subsection: Creating New Dataframe for Figure 7
# Subsection: Building and Formatting Figure 7


# To see tutorials, key webpages, and key videos I used to create and format Figure 6:
# please see the 'Draft_Script_Figure 6_Pandalus-percent.R' in the 'Draft Code' folder

# Draft Script notes include sites and tutorials that assisted me with:
# .........
# .........
# .........


################################################################################
#                            Remaining Tasks 
################################################################################


# update below script for Figure 7
# create Fig 7 df
# create Fig 7 itself
# clean up code


################################################################################
#                         Load Libraries & Base Data          
################################################################################

install.packages('ggtext')  # allows for text formatting in ggplots

### load libraries

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggtext)

### import data

prey <- read.csv('data/processed/2019_basePrey.csv')


################################################################################
#                      Code Chunk: Creating Figure 7
################################################################################


# This code chunk subsections:
# Creates a ....... dataframe capturing ..............
# Restructures above dataframe into the format required for Figure 7
# .................
# Build and format Figure 6


# ....... df: one row per prey sampled.
# prey will need to be recoded (or filtered) for:
# shrimp P. borealis        (OS_ID code = 8111)
# shrimp P. montagui        (OS_ID code = 8112)
# shrimp Pandalus. sp.      (OS_ID code = 8110)
# other (i.e. everything not P. or 'empty' stomachs)
# the other will have to filter out:
# all shrimp P . species      (OS_ID code = 8110-8112)
# empty                       (OS_ID code = 9998)
# sand                        (OS_ID code = 9981)
# stone                       (OS_ID code = 9982)
# shells                      (OS_ID code = 9983)
# mud                         (OS_ID code = 10757)
# plant material              ((OS_ID code = 9987))
# Unidentified material depends on what's being measured
# unidentified material       (OS_ID code = 10746-10750)
# % W includes unidentified material
# % N exclude unidentified material
# Parasites to filter out include nematodes:
# nematomorpha [hairworm]   (OS_ID code = 2585)
# nematoda - nemata [nematode]   (OS_ID code = 2585)


# in the "other' species code classificaiton - offal is entrails
# mud/dirt/veg/debris (anything that goes into the 'empty' category)

## I'll need to update this with the parasites that Sheila tells me about

## I'll need to ask Sheila how Unknown Sp are entered into the database.
# each gets their own value?
# how are the unkn sp a, b, c, d, e determined?
# if each new unknown gets a unique code, then I'll have to adjust my code


# Figure 7 Dataframe structure will need:
# 2 categorical variables:
# prey species
# predator species
# 2 numerical variable:
# percent weight of prey sp. within stomachs
# precent number of prey sp. within stomachs

####### Below Needs to be updated

# Whether stomachs were full or empty will need to be broken down into three categories:
# Full (Prey_OS_ID = not 9998)
# Empty (Prey_OS_ID = 9998).  
# Total


###### Below is updated

# Dataframe transformations - Creating df:
#......
#......
#......
#......


# Dataframe transformations - Formatting for Table:
#......
#......
#......
#......


################### Creating Base Dataframe for Figure 7 ########################
names(prey)

### Creating base dataframe for Figure 7

### Adding 'region' category (WAZ, EAZ, SFA4) 

prey$region = prey$Study.Area  
# creates new column that is duplicate of sa.code, and renames to pred. name

prey <- prey %>% 
  mutate(region=recode(region, "RISA"="EAZ", "SFA2EX"="EAZ", "SFA3" ="WAZ","2G"="SFA4"))
# renaming values in the prey$region column



pandalus.percent <- prey %>%             # create new dataframe based on 'pred' 
  
  # Step 1: subset dataframe to desired columns  
  
  select(Prey_OS_ID,                     # subsets dataframe by selected columns
         # Scientific.Name,
         pred.name,
         PreyWt,
         Prey_Count,
         depth,
         region) %>%
  
  # Step 2: remove rows with 'empty stomachs'. Leave unidentified for now. 
  
  filter(                           # selects rows that do not contain:
    Prey_OS_ID != 9998 |                # empty
      Prey_OS_ID != 9981 |                # sand
      Prey_OS_ID != 9982 |                # stone
      Prey_OS_ID != 9983 |                # shells
      Prey_OS_ID != 10757 |               # mud
      Prey_OS_ID != 9987)  %>%            # plant material
  
  
  # Step 3: create new column where OS_ID's are redifined to the 4 prey categories
  
  # shrimp P. borealis        (OS_ID code = 8111)
  # shrimp P. montagui        (OS_ID code = 8112)
  # shrimp Pandalus. sp.      (OS_ID code = 8110)
  
  mutate(prey.name = ifelse(Prey_OS_ID == 8111,     # create new column 'Full' with values based on logical check
                            "borealis",                      # value if logical check is TRUE
                            ifelse(Prey_OS_ID == 8112,
                                   "montagui",
                                   ifelse(Prey_OS_ID == 8110,
                                          "Pandalus",
                                          "other")
                            ))) %>%
  
  # Step 4: Create new column to force 'other' prey category into solo column in Figure
  mutate(other.prey = prey.name == "other")

#define as either shrimp or not shrimp for prey
pandalus.percent$other.prey[pandalus.percent$other.prey == FALSE] = "Shrimp"
pandalus.percent$other.prey[pandalus.percent$other.prey == TRUE] = "other"


write.csv(pandalus.percent, 
          file="data/processed/2019_F7_pandalus.depth.csv",
          row.names = FALSE)                # removes auto-generated unique ID row

#read in total clean data
pandalus.percent <- read.csv('data/processed/2019_F7_pandalus.depth.csv')


#separate into figure 7a and 7b dfs, sum weight and count per region and depth stratum
pandalus.weight = pandalus.percent |> 
  subset(!is.na(PreyWt)) |> 
  group_by(other.prey, depth, region) |> 
  summarize(wt.subtotal = sum(PreyWt))

pandalus.weight = pandalus.weight |> 
  group_by(region, depth) |> 
  mutate(wt.total = sum(wt.subtotal))

pandalus.weight = pandalus.weight |> 
  mutate(weight.percent = (wt.subtotal/wt.total) * 100) |> 
  subset(other.prey == "Shrimp")

pandalus.count = pandalus.percent |> 
  subset(!is.na(Prey_Count)) |> 
  group_by(other.prey, depth, region) |> 
  summarize(count.subtotal = sum(Prey_Count))

pandalus.count = pandalus.count |> 
  group_by(region, depth) |> 
  mutate(count.total = sum(count.subtotal)) 

pandalus.count = pandalus.count |> 
  mutate(count.percent = (count.subtotal/count.total) * 100) |> 
  subset(other.prey == "Shrimp")

#write into reusable dataframes
write.csv(pandalus.count,
          file = "data/processed/2019_F7B_pandalus_depth.csv", 
          row.names = F)

write.csv(pandalus.weight,
          file = "data/processed/2019_F7A_pandalus_depth.csv", 
          row.names = F)

###################     Creating Figure 7 Dataframe    ########################


### Load Base Figure 7 dataframe
pandalus.weight <- read.csv('data/processed/2019_F7A_pandalus_depth.csv')
pandalus.count <- read.csv('data/processed/2019_F7B_pandalus_depth.csv')



fig7a = ggplot(pandalus.weight,
       aes(x = depth, y = weight.percent, fill = region)) +
  theme_minimal() +
  geom_col(position = "stack", width = 0.5) +

  labs(x = "Depth strata (meters)",                                    # renames x and y axis
       y = "%W") +
  
 # scale_x_discrete(expand = c(1, 1)) +                          # adds padding around the bars
  scale_y_continuous(expand = c(0,0)) +                         # removes padding around y-axis. Places facet_wrap titles directly below bars
  
  scale_fill_manual(values = c(                # assigns colour to prey sp. categories
                               "EAZ" = "#CCEDB1",
                               "WAZ" = "#41B7C4",
                               "SFA4" ="#FF9999"),
                    breaks = c("WAZ",                       # assigns legend order
                               "EAZ",
                               "SFA4")) + 

  theme(
        axis.title.x = element_text(vjust = -1),
        axis.title = element_text(size = 8),                     # size of x and y axis titles
        axis.title.y = element_text(vjust = +3),                 # pulls y-axis title away from chart
        axis.ticks = element_blank(),                            # removes axis ticks
        
        legend.title = element_blank(),                          # removes legend title
        legend.key.size = unit(3, "mm"),                         # adjusts width of legend color symbols
        legend.key.height = unit(3, "mm"),                       # adjusts height of legend color symbols
        legend.key.spacing.y = unit(1, "mm"),                    # defines space between legend items
        legend.background = element_rect(color = "black", linewidth = 1),
        
        panel.grid.minor = element_blank(),                      # removes all minor grid-lines
        panel.grid.major.x = element_blank(),
        #panel.spacing = unit(0, 'point') ,                      # removes space between panels
        
        legend.text = element_markdown(),                        # allows me to italics in legend via markdown code
        strip.text.x = element_markdown(),                        # allows me to insert line breaks into facet stip titles (pred names) via markdown code
        plot.background = element_rect(color = "black", linewidth = 1)
        )

#figure 7b

fig7b = ggplot(pandalus.count,
       aes(x = depth, y = count.percent, fill = region)) +
  theme_minimal() +
  geom_col(position = "stack", width = 0.5) +
  
  labs(x = "Depth strata (meters)",                                    # renames x and y axis
       y = "%N") +
  
  # scale_x_discrete(expand = c(1, 1)) +                          # adds padding around the bars
  scale_y_continuous(expand = c(0,0)) +                         # removes padding around y-axis. Places facet_wrap titles directly below bars
  
  scale_fill_manual(values = c(                # assigns colour to prey sp. categories
    "EAZ" = "#CCEDB1",
    "WAZ" = "#41B7C4",
    "SFA4" ="#FF9999"),
    breaks = c("WAZ",                       # assigns legend order
               "EAZ",
               "SFA4")) + 
  
  theme(
    axis.title.x = element_text(vjust = -1),
    axis.title = element_text(size = 8),                     # size of x and y axis titles
    axis.title.y = element_text(vjust = +3),                 # pulls y-axis title away from chart
    axis.ticks = element_blank(),                            # removes axis ticks
    
    legend.title = element_blank(),                          # removes legend title
    legend.key.size = unit(3, "mm"),                         # adjusts width of legend color symbols
    legend.key.height = unit(3, "mm"),                       # adjusts height of legend color symbols
    legend.key.spacing.y = unit(1, "mm"),                    # defines space between legend items
    legend.background = element_rect(color = "black", linewidth = 1),
    
    panel.grid.minor = element_blank(),                      # removes all minor grid-lines
    panel.grid.major.x = element_blank(),
    #panel.spacing = unit(0, 'point') ,                      # removes space between panels
    
    legend.text = element_markdown(),                        # allows me to italics in legend via markdown code
    strip.text.x = element_markdown(),                        # allows me to insert line breaks into facet stip titles (pred names) via markdown code
    plot.background = element_rect(color = "black", linewidth = 1)
  )

#combine the two with ggpubr
library(ggpubr)

f7 <- ggarrange(fig7a, fig7b,                               # combine figure f6a and f6b into one plot
                ncol = 2,                                     # have figures on two columns (nrow = 1 should yield same result)
                labels = "auto",                              # label figures in lowercase a, b
                common.legend = TRUE,                         # both figures share a common legend
                legend = "bottom",                            # place the shared legend on the bottom
                font.label = list(face = "plain")) +
  
  theme(plot.margin = margin(1,1,1,1, "cm"))

f7
