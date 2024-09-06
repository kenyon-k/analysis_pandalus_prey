################################################################################
################################################################################
#                   Figure 6 Pandalus Percent - DRAFT
################################################################################
################################################################################


# Author: Krista Kenyon (KAK)
# Date Created: Aug 8/2024
# Date Last Modified: Sep 3/2024 by DTE


# Document Purpose: Creating Figure 6 for R Markdown document.
    # Working in code is simpler and faster here than in the R Markdown document
    # R Markdown places code results below chunks, which can mean a lot of scrolling


# R Markdown code pipes this code directly into the document.
    # !! Before knitting ensure to comment out all codes that produce console outputs except the table
    # e.g. str()


# Document Sections:
    # Remaining Tasks
    # Load Libraries & Base Data
    # Code Chunk: Creating Figure 6
        # Subsection: Creating New Dataframe for Figure 6
        # Subsection: Building and Formatting Figure 6


# To see tutorials, key webpages, and key videos I used to create and format Figure 6:
    # please see the 'Draft_Script_Figure 6_Pandalus-percent.R' in the 'Draft Code' folder

# Draft Script notes include sites and tutorials that assisted me with:
    # Creating figure dataset
    # Figure Formatting - ggplot2 - facet & stacked
        # subsection: tutorials
    # Figure Formatting - ggarrange package
    # Unused Code - ggplot2
    # Unused Code - ggarrange
    # OG Code that Dan Enright Helped Write
    # Final Formatting Options
        # subsection: options


################################################################################
#                            Remaining Tasks 
################################################################################


# clean up code
# Ask Sheila about empty stomach/parasites codes, and prey scientific name

## I'll need to update the code chunk notes with the parasites that Sheila tells me about

## I'll need to ask Sheila how Unknown Sp are entered into the database.
    # each gets their own value?
    # how are the unkn sp a, b, c, d, e determined?
    # if each new unknown gets a unique code, then I'll have to adjust my code

# Parasites to filter out include nematodes:
    # nematomorpha [hairworm]   (OS_ID code = 2585)
    # nematoda - nemata [nematode]   (OS_ID code = 2585)


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

# prey <- read.csv('data/processed/2019_basePrey.csv')


################################################################################
#                      Code Chunk: Creating Figure 6 Dataframes
################################################################################


# This code chunk subsections:
    # Creates Figure 6 Base Dataframe
    # Creates Figure 6A Dataframe
    # Creates Figure 6B Dataframe

# The Figure 6 Base df contains all the shared data for Fig 6 panel A and panel B.
# From there the data is filtered for individual panel (Fig 6A and Fig 6B df) 

# This figure details the total % Weight AND % Number of prey categories consumed by each predator category.
    # Fig 6A: % Weight
    # Fig 6B: % Number

# Fig 6A includes unidentified materials.
# Fig 6B removes unidentified materials.




# Figure 6 combines stacked and dodged bars within one figure.
    # This is not possible with a simple figure argument

# To create this effect:
    # Created two separate figures and then joined them together with ggarrange()
    # Each figure is faceted by predator species (4 facets) and formatted to look like one figure
    # Each facet is a stacked bar chart with two categories:
        # shrimp: P. borealis, P. montagui, Pandalus sp.
        # other: everything else not parasite, debris, or empty stomachs




# Figure 6 Dataframe structure will need:
    # 3 categorical variables:
        # fill: detailed shrimp categories (borealis, montagui, P. sp., other)
        # columns: shrimp and other categories
        # facets: predator categories
    # 2 numerical variable:
        # Fig 6A y-axis: percent weight of prey sp. within stomachs
        # Fig 6B y-axis: precent number of prey sp. within stomachs


# Dataframe transformations for Fig 6 Base df (pandalus.percent):
    # subset prey df to retain desired columns
    # remove rows with 'empty stomachs'. Leave unidentified material for now.
    # create new column where Prey_OS_ID's are redefined to the 4 prey categories (fill)
    # Create new column to force 'other' prey category into solo column in Figure (x-axis)
    # Saving the pandalus.percent dataframe structure
    # Exporting pandalus.percent Dataframe


# Dataframe transformations for Fig 6A and 6B df:
    # A|B: make copy of Fig 6 Base Dataframe (pandalus.percent)
    # B: Remove rows with Unidentified Material (leave in for A)
    # A|B: removes rows where PreyWt (A) or Prey_Count (B) is NA 
    # A|B: Sum PreyWt (A) or Prey_Count (B) by Prey Category (prey.name) per Predator Category (pred.name)
    # A|B: Convert Summed Weight (A) or Prey_Count (B) into Percentage for Figure
    # A|B: Save Fig 6A (pandalus.f6a) or Fig 6B (pandalus.f6b) dataframe structure
    # A|B: Export pandalus.f6a or pandalus.f6b dataframes


################### Creating Base Dataframe for Figure 6 ########################


# Figure 6 Base Dataframe is called pandalus.percent
    # Each row represents one prey item with corresponding prey categories, weight, count, and predator the sample is from

# below code is one pipeline that is 'broken up' with annotation notes



### Creating Fig 6 Base Dataframe

pandalus.percent <- prey %>%             # manipulate 'prey' df and save to new Fig 6 Base df 

  
# Step 1: Subset Fig 6 Base df to desired columns  
  
  select(Prey_OS_ID,                     # subsets dataframe by selected columns
        # Scientific.Name,
        Prey_Detail_Code,
         CommonName_DB,
         pred.name,
         PreyWt,
         Prey_Count,
         length.range) %>%               # including this for Figs 8-11 which use this base df

   
# Step 2: remove rows with 'empty stomachs'. Leave unidentified items in for now. 
  
  filter(                           # selects rows that do not (!=) contain:
    Prey_OS_ID != 9998 |                # empty, or (|)
    Prey_OS_ID != 9981 |                # sand
    Prey_OS_ID != 9982 |                # stone
    Prey_OS_ID != 9983 |                # shells
    Prey_OS_ID != 10757 |               # mud
    Prey_OS_ID != 9987) %>%              # plant material
  
  # Step 3: remove rows with 'parasitic' stomach contents
  filter(!Prey_Detail_Code %in%     # selects rows that do not (!) contain the following 'in' them
           c('40'))  %>%                  # parasites
  
# Step 3: create new column where Prey_OS_ID's are redefined to the 4 prey categories (Fig 6 fill)

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
                                  ifelse(Prey_OS_ID == 9980,
                                         "Unidentified material",
                                         ifelse(grepl("unknown", CommonName_DB),  #check for unknown values, can incorporate anything with "unknown" in common name
                                         #ifelse(Prey_OS_ID == 10746 | Prey_OS_ID == 10747 | Prey_OS_ID == 10748 | Prey_OS_ID == 10749 | Prey_OS_ID == 10750,
                                  "Unknown",            # value if fourth logical test is TRUE
                                  "other")              # value if fourth logical test is FALSE
                    ))))) %>%
  
# Step 4: Create new column to force 'other' prey category into solo column in Figure
  
  mutate(other.prey =                           # creating a new column called 'other.prey'
           prey.name == "other" | prey.name == "Unknown" | prey.name == "Unidentified material") %>%            # fill column with TRUE/FALSE on whether corresponding prey.name row is 'other'

  mutate(other.prey = ifelse(other.prey == FALSE, # making changes to 'other.prey'
                                                  # if 'other.prey' values is FALSE
                             "Shrimp",            # enter 'Shrimp'
                             "other"))            # otherwise enter 'other'


### Saving the pandalus.percent dataframe structure

 str(pandalus.percent) 

# 'data.frame':	1236 obs. of  7 variables:
#   
# $ Prey_OS_ID  : int  6967 9998 6967 6967 6967 6967 8020 8020 4950 8530 ...
# $ pred.name   : chr  "Greenland halibut" "Greenland halibut" "Greenland halibut" "Greenland halibut" ...
# $ PreyWt      : num  0.3 NA 0.286 0.091 0.079 ...
# $ Prey_Count  : int  NA NA NA NA NA NA NA NA NA NA ...
# $ length.range: chr  "21-25" "16-20" "16-20" "16-20" ...
# $ prey.name   : chr  "other" "other" "other" "other" ...
# $ other.prey  : chr  "other" "other" "other" "other" ...



### Exporting pandalus.percent Dataframe

write.csv(pandalus.percent, 
          file="data/processed/2019_F6_pandalus.percent.csv",
          row.names = FALSE)                # removes auto-generated unique ID row


###################     Creating Figure 6A Dataframe    ########################


### Load Base Figure 6 dataframe

# pandalus.percent <- read.csv('data/processed/2019_F6_pandalus.percent.csv')



### Creating Fig 6A Specific Dataframe

pandalus.f6a <- pandalus.percent  %>%     # manipulate 'prandalus.percent' df and save to new Fig 6a df 
  
  
# Step 1: removes rows where PreyWt is NA  

  subset(!is.na(PreyWt)) %>%        # removes rows were PreyWt is NA    

  
# Step 2: Sum Prey Weight by Prey Category (prey.name) per Predator Category (pred.name) 
  
  group_by(pred.name, prey.name, other.prey) %>%                  # group by the categories we will want to retain after summarize()
  summarize(prey.percent = sum(PreyWt), .groups = "keep") %>%     # create new column (prey.percent) that sums PreyWt by prey.name per pred.name
 
  
# Step 3: Turn Weight into Percentage for Figure 
  group_by(pred.name) %>%                                       # group by the categories I that will feed into the below mutate     
  mutate(prey.percent = (prey.percent/sum(prey.percent))*100)   # change 'prey.percent' column. OG values formatted into percentage per pred.name



### Saving the Figure 6a dataframe structure

# str(pandalus.f6a) 

# gropd_df [14 × 4] (S3: grouped_df/tbl_df/tbl/data.frame)

# $ pred.name   : chr [1:14] "Atlantic cod" "Atlantic cod" "Atlantic cod" "Greenland halibut" ...
# $ prey.name   : chr [1:14] "Pandalus" "borealis" "other" "Pandalus" ...
# $ other.prey  : chr [1:14] "Shrimp" "Shrimp" "other" "Shrimp" ...
# $ prey.percent: num [1:14] 29.73 64.52 5.75 2.07 10.99 ...
# - attr(*, "groups")= tibble [4 × 2] (S3: tbl_df/tbl/data.frame)
# ..$ pred.name: chr [1:4] "Atlantic cod" "Greenland halibut" "Redfish" "Skate"
# ..$ .rows    : list<int> [1:4] 
# .. ..$ : int [1:3] 1 2 3
# .. ..$ : int [1:4] 4 5 6 7
# .. ..$ : int [1:3] 8 9 10
# .. ..$ : int [1:4] 11 12 13 14
# .. ..@ ptype: int(0) 
# ..- attr(*, ".drop")= logi TRUE



### Exporting pandalus.f6a Dataframe

write.csv(pandalus.f6a, 
          file="data/processed/2019_F6_pandalus.f6a.csv",
          row.names = FALSE)                # removes auto-generated unique ID row


###################     Creating Figure 6B Dataframe    ########################


### Load Base Figure 6 dataframe

# pandalus.percent <- read.csv('data/processed/2019_F6_pandalus.percent.csv')



### Creating Fig 6B Specific Dataframe


pandalus.f6b <- pandalus.percent %>%   # manipulate 'prandalus.percent' df and save to new Fig 6b df 


# Step 1: Remove rows with Unidentified Material
  filter(                           # selects rows that do not contain:
    Prey_OS_ID != 10746 |                # unknown sp. A
    Prey_OS_ID != 10747 |                # unknown sp. B
    Prey_OS_ID != 10748 |                # unknown sp. C
    Prey_OS_ID != 10749 |                # unknown sp. D
    Prey_OS_ID != 10750)  %>%            # unknown sp. E

  
# Step 2: removes rows where Prey_Count is NA  
  subset(!is.na(Prey_Count)) %>%        # removes rows where Prey_Count is NA

  
# Step 3: Sum Prey Count by Prey Category (prey.name) per Predator Category (pred.name) 
  group_by(pred.name, prey.name, other.prey) %>%                         # group by the categories we will want to retain after summarize()
  summarize(prey.count.percent = sum(Prey_Count), .groups = "keep") %>%  # create new column (prey.count.percent) that sums Prey_Count by prey.name per pred.name

  
  # Step 4: Turn Weight into Percentage for Figure 
  group_by(pred.name) %>%                                               # group by the categories I that will feed into the below mutate 
  mutate(prey.count.percent = (prey.count.percent/sum(prey.count.percent))*100) # change 'prey.count.percent' column. OG values formatted into percentage per pred.name



### Saving the Figure 6b dataframe structure

# str(pandalus.f6b) 

# gropd_df [13 × 4] (S3: grouped_df/tbl_df/tbl/data.frame)
# $ pred.name         : chr [1:13] "Atlantic cod" "Atlantic cod" "Greenland halibut" "Greenland halibut" ...
# $ prey.name         : chr [1:13] "Pandalus" "borealis" "Pandalus" "borealis" ...
# $ other.prey        : chr [1:13] "Shrimp" "Shrimp" "Shrimp" "Shrimp" ...
# $ prey.count.percent: num [1:13] 57.14 42.86 1.16 7.72 12.74 ...
# - attr(*, "groups")= tibble [4 × 2] (S3: tbl_df/tbl/data.frame)
# ..$ pred.name: chr [1:4] "Atlantic cod" "Greenland halibut" "Redfish" "Skate"
# ..$ .rows    : list<int> [1:4] 
# .. ..$ : int [1:2] 1 2
# .. ..$ : int [1:4] 3 4 5 6
# .. ..$ : int [1:3] 7 8 9
# .. ..$ : int [1:4] 10 11 12 13
# .. ..@ ptype: int(0) 
# ..- attr(*, ".drop")= logi TRUE



### Exporting pandalus.f6b Dataframe

write.csv(pandalus.f6b, 
          file="data/processed/2019_F6_pandalus.f6b.csv",
          row.names = FALSE)                # removes auto-generated unique ID row
  
  
################################################################################
#                   Code Chunk: Building & Formatting Figure 6
################################################################################


# Below code builds Fig 6a, Fig 6b, then combines them into Figure 6

# Alternate formtting options for Figure 6 are saved in:
    # 'scripts/draft code/Draft_Script_Figure 6_Pandalus-percent.R'



### Load Figure 6 dataframe

# pandalus.f6a <- read.csv('data/processed/2019_F6_pandalus.f6a.csv')



### Build Figure 6a

f6a <- ggplot(pandalus.f6a,
       aes(x = factor(other.prey, levels = c('Shrimp', 'other')),               # orders x-axis discrete data for figure
           y = prey.percent, 
           fill = factor(prey.name, levels = c('other',                        # orders pray.name variables in bars
                                               'Pandalus', 
                                               'montagui', 
                                               'borealis')) )) + 
  theme_minimal(                                                               # pre-set 'minimal' theme
    base_size = 16) +                                                          # sets base text (minus titles) to size 16
  
  geom_col(position = "stack",                                                 # creates stacked bar chart
           width = 1) +                                                        # specifies width. Width = 1 forces bars to touch
  
  facet_wrap(~ pred.name,                                                      # facet multiple charts by predator name
             ncol = 4,                                                         # plots in 4 columns (so all appear in 1 row)
             strip.position = "bottom",                                        # moves the facet strip to the bottom
             labeller = as_labeller(c("Atlantic cod" = "Atlantic<br>cod",             # renames to inserts  line breaks <br> into labels
                                      "Greenland halibut" = "Greenland<br>halibut",   # needs accompanying argument in theme() 
                                      "Redfish" = "Redfish",                          # uses Markdown syntax to insert line breaks
                                      "Skate" = "Skate"))) +                      
  
  labs(x = "Predator Species",                                   # renames x and y axis
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

  coord_cartesian(ylim = c(0, 100)) +                           # sets y-axis between 0-100
  scale_x_discrete(expand = c(1, 1)) +                          # adds padding around the bars
  scale_y_continuous(expand = c(0,0)) +                         # removes padding around y-axis. Places facet_wrap titles directly below bars
  
  theme(axis.text.x = element_blank(),                           # removes x-axis labels
        axis.title = element_text(size = 8),                     # size of x and y axis titles
        axis.title.y = element_text(vjust = +3),                 # pulls y-axis title away from chart
        axis.ticks = element_blank(),                            # removes axis ticks

        legend.title = element_blank(),                          # removes legend title
        legend.key.size = unit(3, "mm"),                         # adjusts width of legend color symbols
        legend.key.height = unit(3, "mm"),                       # adjusts height of legend color symbols
        legend.key.spacing.y = unit(1, "mm"),                    # defines space between legend items

        panel.grid.minor = element_blank(),                      # removes all minor grid-lines
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(0, 'point') ,                       # removes space between panels
        
        legend.text = element_markdown(),                        # allows me to italics in legend via markdown code
        strip.text.x = element_markdown(),                       # allows me to insert line breaks into facet strip titles (pred names) via markdown code
        plot.margin = margin(t=5, r = 10, b = 5, l = 8,          # adds space around figure
                             unit = "mm"))                        # space included when combining figures in ggarrange()


####################   Build Figure 6b  #######################################


### Load Figure 6B dataframe

# pandalus.f6b <- read.csv('data/processed/2019_F6_pandalus.f6b.csv')



### Build Figure 6B

f6b <- ggplot(pandalus.f6b,
              aes(x = factor(other.prey, levels = c('Shrimp', 'other')),               # orders x-axis discrete data for figure
                  y = prey.count.percent, 
                  fill = factor(prey.name, levels = c('other',                  # orders pray.name variables in bars
                                                      'Pandalus', 
                                                      'montagui', 
                                                      'borealis')) )) +
  
  theme_minimal(                                                               # pre-set 'minimal' theme
    base_size = 16) +                                                          # sets base text (minus titles) to size 16
  
  geom_col(position = "stack",                                                 # creates stacked bar chart
           width = 1) +                                                        # specifies width. Width = 1 forces bars to touch
  
  facet_wrap(~ pred.name,                                                      # facet multiple charts by predator name
             ncol = 4,                                                         # plots in 4 columns (so all appear in 1 row)
             strip.position = "bottom",                                        # moves the facet strip to the bottom
             labeller = as_labeller(c("Atlantic cod" = "Atlantic<br>cod",             # renames to inserts  line breaks <br> into labels
                                      "Greenland halibut" = "Greenland<br>halibut",   # needs accompanying argument in theme() 
                                      "Redfish" = "Redfish",                          # uses Markdown syntax to insert line breaks
                                      "Skate" = "Skate")))+                      
  
  labs(x = "Predator Species",                                    # renames x and y axis
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
  
  coord_cartesian(ylim = c(0, 100)) +                           # sets y-axis between 0-100
  scale_x_discrete(expand = c(1, 1)) +                          # adds padding around the bars
  scale_y_continuous(expand = c(0,0)) +                         # removes padding around y-axis. Places facet_wrap titles directly below bars
  
  theme(axis.text.x = element_blank(),                           # removes x-axis labels
        axis.title = element_text(size = 8),                     # size of x and y axis titles
        axis.title.y = element_text(vjust = +3),                 # pulls y-axis title away from chart
        axis.ticks = element_blank(),                            # removes axis ticks
        
        legend.title = element_blank(),                          # removes legend title
        legend.key.size = unit(3, "mm"),                         # adjusts width of legend color symbols
        legend.key.height = unit(3, "mm"),                       # adjusts height of legend color symbols
        legend.key.spacing.y = unit(1, "mm"),                    # defines space between legend items
        
        panel.grid.minor = element_blank(),                      # removes all minor grid-lines
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(0, 'point') ,                      # removes space between panels
        
        legend.text = element_markdown(),                        # allows me to italics in legend via markdown code
        strip.text.x = element_markdown(),                        # allows me to insert line breaks into facet strip titles (pred names) via markdown code
        plot.margin = margin(t=5, r = 10, b = 5, l = 8,          # adds space around figure
                             unit = "mm"))                        # space included when combining figures in ggarrange()


####################   Combine to Create Figure 6 ##################################


### Combine Figure 6A and 6B

f6 <- ggarrange(f6a, f6b,                               # combine figure f6a and f6b into one plot
          ncol = 2,                                     # have figures on two columns (nrow = 1 should yield same result)
          labels = "auto",                              # label figures in lowercase a, b
          common.legend = TRUE,                         # both figures share a common legend
          legend = "bottom",                            # place the shared legend on the bottom
          font.label = list(face = "plain")) +
  
  theme(plot.margin = margin(1,1,1,1, "cm"))
  
f6
