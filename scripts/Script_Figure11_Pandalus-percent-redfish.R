################################################################################
################################################################################
#                   Figure 11 Pandalus Percent - DRAFT
################################################################################
################################################################################


# Author: Krista Kenyon (KAK) (Fig 8 code)
# Date Created: Aug 19/2024 (Fig 8 code)
# Date Last Modified: Sep 3/2024 by DTE


# Document Purpose: Creating Figure 8 for R Markdown document.
# Working in code is simpler and faster here than in the R Markdown document
# R Markdown places code results below chunks, which can mean a lot of scrolling


# R Markdown code pipes this code directly into the document.
# !! Before knitting ensure to comment out all codes that produce console outputs except the table
# e.g. str()


# Document Sections:
# Remaining Tasks
# Load Libraries & Base Data
# Code Chunk: Creating Figure 8
# Subsection: Creating New Dataframe for Figure 8
# Subsection: Building and Formatting Figure 8


# To see tutorials, key webpages, and key videos I used to create and format Figure 6:
# please see the 'Draft_Script_Figure06_Pandalus-percent-Halibut.R' in the 'Draft Code' folder

# Draft Script notes include sites and tutorials that assisted me with:
# ......
# ......
# ......


################################################################################
#                            Remaining Tasks 
################################################################################


# clean up code

# Team's input on final formatting


################################################################################
#                         Load Libraries & Base Data          
################################################################################



### load libraries

# library(tidyverse)
# library(dplyr)
# library(ggplot2)
# library(ggtext)     # allows use of Markdown syntax to bold, italics, etc figure text
# library(ggpubr)     # give ggarrange function



### import data

# pandalus.percent <- read.csv('data/processed/2019_F6_pandalus.percent.csv')



################################################################################
#                      Code Chunk: Creating Figure 8 Dataframe
################################################################################


# This code chunk subsections:
# Creates Figure 8 Dataframe
# .....

# The Figure 8 df contains/details .....


# Figure 8 Dataframe structure will need:
# 3 categorical variables:
# fill: detailed shrimp categories (borealis, montagui, P. sp., other)
# columns: predator length
# will need predator included to use 1 df in multiple figures
# 2 numerical variable:
# Fig 8A y-axis: percent weight of prey sp. within stomachs
# Fig 8B y-axis: precent number of prey sp. within stomachs


# Dataframe transformations for Fig 8 df ( df name ):
# .......
# ......
# ......


################### Creating Figure 8 Dataframe ########################


# Figure 8 Base Dataframe is called ........
# Each row represents ...... with corresponding 

# below code is one pipeline that is 'broken up' with annotation notes



### Creating Fig 8 Base Dataframe


fish.pandalus <- pandalus.percent %>%     # manipulate 'pandalus.prey' df and save to new Fig 8 Base df
  
  # Step 1: remove other.prey column (may be unnecessary - but want to not be confused)    
  
  select(-other.prey)    %>%              # column used for Fig 6. Not needed for Fig 8. 
  
  # Step 2: make pred.name data same length (useful for ordering)
  mutate(length.range = str_pad(length.range,          # code adds 0s to start of length.range string (6 becomes 006)
                                width=6,             # strings <6 characters will be padded until they =6 characters
                                pad = "0")) # %>%   # padding with 0s

# .......

write.csv(fish.pandalus, 
          file="data/processed/2019_F8_fish.pandalus.csv",
          row.names = FALSE)   

### Creating Fig 8A Base Dataframe

fish.p.weight <- fish.pandalus %>%   # manipulate 'fish.pandalus' df and save to Fig 8A df
  
  # Step 3: turn NAs into 0
  mutate(PreyWt_noNA = replace_na(PreyWt, replace = 0)) |> 
  #subset(!is.na(PreyWt))  %>%        # removes rows where PreyWt is NA    
  
  
  # Step 4: Sum Prey Weight by Prey Category (prey.name) per Predator length (length.range) 
  
  group_by(length.range, prey.name, pred.name) %>%                  # group by the categories we will want to retain after summarize()
  #  summarize(shrimp.weight = sum(PreyWt), .groups = "keep") %>%     # create new column (prey.percent) that sums PreyWt by prey.name per pred.name
  summarize(shrimp.weight = sum(PreyWt_noNA), .groups = "keep") %>%      # set up to replace NAs with 0
  
  # Step 5: Turn Weight into Percentage for Figure 
  group_by(length.range, pred.name) %>%                                       # group by the categories I that will feed into the below mutate     
  mutate(shrimp.weight.p = (shrimp.weight/sum(shrimp.weight))*100)        # change 'prey.percent' column. OG values formatted into percentage per pred.name


### Saving the fish.p.weight dataframe structure

#str(fish.p.weight) 


### Exporting pandalus.percent Dataframe

# of note - this df will be used for the next 3 figures

write.csv(fish.p.weight, 
          file="data/processed/2019_F8-11_fish.p.weight.csv",
          row.names = FALSE)                # removes auto-generated unique ID row



### Creating Fig 8B Base Dataframe

fish.p.count <- fish.pandalus %>%   # manipulate 'fish.pandalus' df and save to Fig 8A df
  
  # Step 3: removes rows where PreyWt is NA  
  
  subset(!is.na(Prey_Count))  %>%        # removes rows were PreyWt is NA    
  
  
  # Step 4: Sum Prey Weight by Prey Category (prey.name) per Predator length (length.range) 
  
  group_by(length.range, prey.name, pred.name) %>%                  # group by the categories we will want to retain after summarize()
  summarize(shrimp.count = sum(Prey_Count), .groups = "keep") %>%     # create new column (prey.percent) that sums PreyWt by prey.name per pred.name
  
  
  # Step 5: Turn Weight into Percentage for Figure 
  group_by(length.range, pred.name) %>%                                       # group by the categories I that will feed into the below mutate     
  mutate(shrimp.count.p = (shrimp.count/sum(shrimp.count))*100)        # change 'prey.percent' column. OG values formatted into percentage per pred.name


### Saving the fish.p.weight dataframe structure

#str(fish.p.count) 


### Exporting pandalus.percent Dataframe

# of note - this df will be used for the next 3 figures

write.csv(fish.p.count, 
          file="data/processed/2019_F8-11_fish.p.count.csv",
          row.names = FALSE)                # removes auto-generated unique ID row


################################################################################
#                   Code Chunk: Building & Formatting Figure 8
################################################################################


# Below code builds Fig 8a, Fig 8b, then combines them into Figure 8

# Alternate formtting options for Figure 6 are saved in:
# ...............



### Load Figure 8a dataframe

# fish.p.weight <- read.csv('data/processed/2019_F8-11_fish.p.weight.csv')


### Build Figure 8a

t <- fish.p.weight %>%
  
  # sort by length.range
  arrange(length.range) %>%                       # sort smallest to largest now that length.range strings equal character length before '-'
  
  # remove extra 0's in length range
  mutate(length.range = str_replace(length.range, # removing the extra zeros for Table formatting
                                    "^0{1,}",     # select 1+ '0's at the beginning of strings
                                    "")) %>%         # remove selected '0's
  # filter by Redfish
  filter(pred.name == "Redfish")          # selects rows that contain Greenland halibut



f8a <- ggplot(t, aes(x = length.range, y = shrimp.weight.p, 
                     fill = factor(prey.name, levels = c('other',                        # orders pray.name variables in bars
                                                         'Pandalus', 
                                                         'montagui', 
                                                         'borealis')) )) +
  geom_col(position = "stack",                                                # creates stacked bar chart
           width = 0.7) +                                           # reduces width of columns
  
  theme_minimal()       +                                                         # pre-set 'minimal' theme 
  
  labs(x = "Predator Length (cm)",                                   # renames x and y axis
       y = "%W") +
  
  scale_fill_manual(values = c("other" = "grey",                 # assigns colour to prey sp. categories
                               "Pandalus" = "#CCEDB1",
                               "borealis" = "#41B7C4",
                               "montagui" ="#FF9999"),
                    breaks = c("borealis",                       # assigns legend order
                               "montagui",
                               "Pandalus",
                               "other"), 
                    labels = c("*P. borealis*",                  # *xx* insert italics with markdown code/syntax
                               "*P. montagui*",                  # needs accompanying argument in theme()
                               "*Pandalus sp.*", 
                               "other")) +
  
  scale_y_continuous(expand = c(0,0)) +                         # removes padding around y-axis. Places facet_wrap titles directly below bars
  scale_x_discrete(expand = c(0.3, 0.3)) +                          # adds padding around the bars
  
  theme(axis.title = element_text(size = 8),                     # size of x and y axis titles
        axis.title.y = element_text(vjust = +3),                 # pulls y-axis title away from chart
        axis.ticks = element_blank(),                            # removes axis ticks
        
        legend.title = element_blank(),                          # removes legend title
        legend.key.size = unit(3, "mm"),                         # adjusts width of legend color symbols
        legend.key.height = unit(3, "mm"),                       # adjusts height of legend color symbols
        legend.key.spacing.y = unit(1, "mm"),                    # defines space between legend items
        
        panel.grid.minor = element_blank(),                      # removes all minor grid-lines
        panel.grid.major.x = element_blank(),
        
        legend.text = element_markdown(),                        # allows me to italics in legend via markdown code
        plot.margin = margin(t=5, r = 10, b = 5, l = 8,          # adds space around figure
                             unit = "mm"))                        # space included when combining figures in ggarrange()


####################   Build Figure 8b  #######################################


### Load Figure 8b dataframe

# fish.p.count <- read.csv('data/processed/2019_F8-11_fish.p.count.csv')



### Build Figure 8a

apple <- fish.p.count %>%
  
  # sort by length.range
  arrange(length.range) %>%                       # sort smallest to largest now that length.range strings equal character length before '-'
  
  # remove extra 0's in length range
  mutate(length.range = str_replace(length.range, # removing the extra zeros for Table formatting
                                    "^0{1,}",     # select 1+ '0's at the beginning of strings
                                    "")) %>%         # remove selected '0's
  # filter by Redfish
  filter(pred.name == "Redfish")          # selects rows that contain Greenland halibut



f8b <- ggplot(apple, aes(x = length.range, y = shrimp.count.p, 
                         fill = factor(prey.name, levels = c('other',                        # orders pray.name variables in bars
                                                             'Pandalus', 
                                                             'montagui', 
                                                             'borealis')) )) +
  geom_col(position = "stack",                                                # creates stacked bar chart
           width = 0.7) +                                           # reduces width of columns
  
  theme_minimal()       +                                                         # pre-set 'minimal' theme 
  
  labs(x = "Predator Length (cm)",                                   # renames x and y axis
       y = "%N") +
  
  scale_fill_manual(values = c("other" = "grey",                 # assigns colour to prey sp. categories
                               "Pandalus" = "#CCEDB1",
                               "borealis" = "#41B7C4",
                               "montagui" ="#FF9999"),
                    breaks = c("borealis",                       # assigns legend order
                               "montagui",
                               "Pandalus",
                               "other"), 
                    labels = c("*P. borealis*",                  # *xx* insert italics with markdown code/syntax
                               "*P. montagui*",                  # needs accompanying argument in theme()
                               "*Pandalus sp.*", 
                               "other")) +
  
  scale_y_continuous(expand = c(0,0)) +                         # removes padding around y-axis. Places facet_wrap titles directly below bars
  scale_x_discrete(expand = c(0.3, 0.3)) +                          # adds padding around the bars
  
  theme(axis.title = element_text(size = 8),                     # size of x and y axis titles
        axis.title.y = element_text(vjust = +3),                 # pulls y-axis title away from chart
        axis.ticks = element_blank(),                            # removes axis ticks
        
        legend.title = element_blank(),                          # removes legend title
        legend.key.size = unit(3, "mm"),                         # adjusts width of legend color symbols
        legend.key.height = unit(3, "mm"),                       # adjusts height of legend color symbols
        legend.key.spacing.y = unit(1, "mm"),                    # defines space between legend items
        
        panel.grid.minor = element_blank(),                      # removes all minor grid-lines
        panel.grid.major.x = element_blank(),
        
        legend.text = element_markdown(),                        # allows me to italics in legend via markdown code
        plot.margin = margin(t=5, r = 10, b = 5, l = 8,          # adds space around figure
                             unit = "mm"))                        # space included when combining figures in ggarrange()


####################   Combine to Create Figure 8 ##################################

### Combine Figure 8A and 8B

### Combine Figure 6A and 6B

f8 <- ggarrange(f8a, f8b,                               # combine figure f6a and f6b into one plot
                ncol = 2,                                     # have figures on two columns (nrow = 1 should yield same result)
                labels = "auto",                              # label figures in lowercase a, b
                common.legend = TRUE,                         # both figures share a common legend
                legend = "bottom",                            # place the shared legend on the bottom
                font.label = list(face = "plain")) +
  
  theme(plot.margin = margin(1,1,1,1, "cm"))

f8



################################################################################
####################### Testing Figure Formatting ##############################
################################################################################
# 
# 
# apple
# 
# 
# ggplot(apple,
#        aes(x = length.range, y = shrimp.count.p, fill = pred.name)) +
#   theme_bw() +
#   geom_col() +
#   facet_wrap(~ prey.name) +
#   theme(legend.position = "bottom",
#         legend.background = element_rect(fill="NA", color = 1),
#         plot.background = element_rect(colour = "black", fill = NA, linewidth = 1))
# 
# 
# ggplot(apple, aes(x = length.range, y = shrimp.count.p, 
#                   fill = factor(prey.name, levels = c('other',                        # orders pray.name variables in bars
#                                                       'Pandalus', 
#                                                       'montagui', 
#                                                       'borealis')) )) +
#   geom_col(position = "stack",                                                # creates stacked bar chart
#            width = 0.7) +                                           # reduces width of columns
#   
#   theme_minimal()       +                                                         # pre-set 'minimal' theme 
#   
#   labs(x = "Predator Length (cm)",                                   # renames x and y axis
#        y = "%N") +