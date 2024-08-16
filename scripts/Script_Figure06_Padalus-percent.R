################################################################################
################################################################################
#                   Figure 6 Pandalus Percent - DRAFT
################################################################################
################################################################################


# Author: Krista Kenyon (KAK)
# Date Created: Aug 8/2024
# Date Last Modified: Aug 9/2024 by KAK


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
    # .........
    # .........
    # .........


################################################################################
#                            Remaining Tasks 
################################################################################


# update below script for Figure 6
# create Fig 6 df
# create Fig 6 itself
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
#                      Code Chunk: Creating Figure 6
################################################################################


# This code chunk subsections:
    # Creates a ....... dataframe capturing ..............
    # Restructures above dataframe into the format required for Figure 6
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


# Figure 6 Dataframe structure will need:
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


# !! I need to ask Sheila if others codes should be added! Paper included if only mud, or full of parasites.


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


################### Creating Base Dataframe for Figure 6 ########################
names(prey)

### Creating base dataframe for Figure 6

pandalus.percent <- prey %>%             # create new dataframe based on 'pred' 

# Step 1: subset dataframe to desired columns  
    
  select(Prey_OS_ID,                     # subsets dataframe by selected columns
        # Scientific.Name,
         pred.name,
         PreyWt,
         Prey_Count) %>%
 
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

pandalus.percent$other.prey[pandalus.percent$other.prey == FALSE] = "Shrimp"
pandalus.percent$other.prey[pandalus.percent$other.prey == TRUE] = "other"



### Saving the pandalus.percent dataframe structure

str(pandalus.percent) 

# 'data.frame':	1236 obs. of  6 variables:

# $ Prey_OS_ID: int  6967 9998 6967 6967 6967 6967 8020 8020 4950 8530 ...
# $ pred.name : chr  "Greenland halibut" "Greenland halibut" "Greenland halibut" "Greenland halibut" ...
# $ PreyWt    : num  0.3 NA 0.286 0.091 0.079 ...
# $ Prey_Count: int  NA NA NA NA NA NA NA NA NA NA ...
# $ prey.name : chr  "other" "other" "other" "other" ...
# $ other.prey: chr  "other" "other" "other" "other" ...



### Exporting pandalus.percent Dataframe

write.csv(pandalus.percent, 
          file="data/processed/2019_F6_pandalus.percent.csv",
          row.names = FALSE)                # removes auto-generated unique ID row


###################     Creating Figure 6A Dataframe    ########################


### Load Base Figure 6 dataframe

pandalus.percent <- read.csv('data/processed/2019_F6_pandalus.percent.csv')



### Creating Fig 6A Specific Dataframe


# Step 1: Copy Fig 6 Base Dataframe
pandalus.f6a <- pandalus.percent


# Step 2: removes rows where PreyWt is NA  
pandalus.f6a <- pandalus.f6a  %>%
  subset(!is.na(PreyWt))%>%        # removes NAs from PreyWt column only   

  
# Step 3: Sum Prey Weight by Prey Category (prey.name) per Predator Category (pred.name) 
  group_by(pred.name, prey.name, other.prey) |> summarize(prey.percent = sum(PreyWt), .groups = "keep") %>%  # sum PreyWt by prey.name per pred.name
  group_by(pred.name) |> mutate(pizza = prey.percent/sum(prey.percent))


# Step 4: Turn Weight into Percentage for Figure
pandalus.f6a$pizza <- pandalus.f6a[['pizza']]*100    # format into percentage
  

  
### Saving the Figure 6a dataframe structure

# str(pandalus.f6a) 

# 'data.frame':	1236 obs. of  6 variables:
#   
# $ Prey_OS_ID: int  6967 9998 6967 6967 6967 6967 8020 8020 4950 8530 ...
# $ pred.name : chr  "Greenland halibut" "Greenland halibut" "Greenland halibut" "Greenland halibut" ...
# $ PreyWt    : num  0.3 NA 0.286 0.091 0.079 ...
# $ Prey_Count: int  NA NA NA NA NA NA NA NA NA NA ...
# $ prey.name : chr  "other" "other" "other" "other" ...
# $ other.prey: chr  "other" "other" "other" "other" ...



### Exporting pandalus.f6a Dataframe

write.csv(pandalus.f6a, 
          file="data/processed/2019_F6_pandalus.f6a.csv",
          row.names = FALSE)                # removes auto-generated unique ID row


###################     Creating Figure 6B Dataframe    ########################


### Load Base Figure 6 dataframe

pandalus.percent <- read.csv('data/processed/2019_F6_pandalus.percent.csv')



### Creating Fig 6B Specific Dataframe
names(pandalus.f6b)

# Step 1: Copy Fig 6 Base Dataframe
pandalus.f6b <- pandalus.percent


# Step 2: Remove rows with Unidentified Material
pandalus.f6b <- pandalus.f6b  %>%
  filter(                           # selects rows that do not contain:
    Prey_OS_ID != 10746 |                  # unknown sp. A
    Prey_OS_ID != 10747 |                # unknown sp. B
    Prey_OS_ID != 10748 |                # unknown sp. C
    Prey_OS_ID != 10749 |                # unknown sp. D
    Prey_OS_ID != 10750)  %>%            # unknown sp. E

  
# Step 3: removes rows where Prey_Count is NA  
  subset(!is.na(Prey_Count)) %>%        # removes NAs from PreyWt column only   
  

# Step 4: Sum Prey Count by Prey Category (prey.name) per Predator Category (pred.name) 
group_by(pred.name, prey.name, other.prey) |> summarize(prey.count.percent = sum(Prey_Count), .groups = "keep") %>%  # sum PreyWt by prey.name per pred.name
  group_by(pred.name) |> mutate(apple = prey.count.percent/sum(prey.count.percent))

# Step 5: Turn Count into Percentage for Figure
pandalus.f6b$apple <- pandalus.f6b[['apple']]*100    # format into percentage



### Saving the Figure 6b dataframe structure

 str(pandalus.f6b) 

 # gropd_df [13 × 5] (S3: grouped_df/tbl_df/tbl/data.frame)
 # $ pred.name         : chr [1:13] "Atlantic cod" "Atlantic cod" "Greenland halibut" "Greenland halibut" ...
 # $ prey.name         : chr [1:13] "Pandalus" "borealis" "Pandalus" "borealis" ...
 # $ other.prey        : chr [1:13] "Shrimp" "Shrimp" "Shrimp" "Shrimp" ...
 # $ prey.count.percent: int [1:13] 4 3 3 20 33 203 1 1 837 1 ...
 # $ apple             : num [1:13] 57.14 42.86 1.16 7.72 12.74 ...
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
  
  


###################    Building and Formatting Figure 6    ######################


### Load Figure 6 dataframe

# pandalus.f6a <- read.csv('data/processed/2019_F6_pandalus.f6a.csv')



### Build Figure 6a

f6a <- ggplot(pandalus.f6a,
       aes(x = factor(other.prey, levels = c('Shrimp', 'other')),               # orders x-axis discrete data for figure
           y = pizza, 
           fill = factor(prey.name, levels = c('other', 'Pandalus', 'montagui', 'borealis')) # orders pray.name variables in bars
           )) +
  
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
                                      "Skate" = "Skate")
             ))+                      
  
  labs(x = "Predator Species",                                    # renames x and y axis
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
      # scale_y_continuous() did not like the scale_fill_manual(breaks =) 
      # scale_y_continuous() was not forcing a tick at 100. I'm not sure why
      # I OG retained the code to ensure the y-axis breaks were at 0, 25, 50, 75, 100
      # removed scale_y_continuous() because of errors/non-compatability with scale_fill_manual(breaks)
  
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
                             unit = "mm")                        # space included when combining figures in ggarrange()
  )


        
# didn't need the below theme() arguments to move the axis.
# The issue was that my Strip Labels were within the y-axis padding.
# I removed this padding in scale_y_continuous(expand())
        
      #  axis.text.x = element_text(vjust = -10),
       # strip.text = element_text(vjust = 8)
        
  #       strip.text = element_text(vjust = -10)) +       # pulls panel titles to the top left
  
# plot.background = element_rect(fill='transparent', color=NA)#,
#  scale_y_continuous(breaks = seq(0, 100, by = 25)) +


####################   Build Figure 6b  #######################################


### Load Figure 6B dataframe

# pandalus.f6b <- read.csv('data/processed/2019_F6_pandalus.f6b.csv')



### Build Figure 6B

f6b <- ggplot(pandalus.f6b,
              aes(x = factor(other.prey, levels = c('Shrimp', 'other')),               # orders x-axis discrete data for figure
                  y = apple, 
                  fill = factor(prey.name, levels = c('other', 'Pandalus', 'montagui', 'borealis')) # orders pray.name variables in bars
              )) +
  
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
                                      "Skate" = "Skate")
             ))+                      
  
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
                             unit = "mm")                        # space included when combining figures in ggarrange()
  )


####################   Combine to Create Figure 6 ##################################

library(ggpubr)     # give ggarrange function

### Combine Figure 6A and 6B

f6 <- ggarrange(f6a, f6b,                   # combine figure f6a and f6b into one plot
          ncol = 2,                   # have figures on two columns (nrow = 1 should yield same result)
          labels = "auto",            # label figures in lowercase a, b
          common.legend = TRUE,       # both figures share a common legend
          legend = "bottom",           # place the shared legend on the bottom
          font.label = list(face = "plain")
          ) 
  theme(plot.margin = margin(1,1,1,1, "cm"))

# this version does create an outside border
f6 +
  border()

# This also creates an outside border
f6 +
  theme(plot.background = element_rect(colour = "black", fill = NA, size = 1))


# outside border per panel. Legend below without border

f6ab <- f6a +
  theme(plot.background = element_rect(colour = "black", fill = NA, size = 1))

f6bb <- f6b +
  theme(plot.background = element_rect(colour = "black", fill = NA, size = 1))

ggarrange(f6ab, f6bb,                   # combine figure f6a and f6b into one plot
          ncol = 2,                   # have figures on two columns (nrow = 1 should yield same result)
          labels = "auto",            # label figures in lowercase a, b
          common.legend = TRUE,       # both figures share a common legend
          legend = "bottom",           # place the shared legend on the bottom
          font.label = list(face = "plain")
) 

# outside border per panel with legend border

f6abl <- f6ab +
  theme(legend.background = element_rect(colour = 1))

f6bbl <- f6bb +
  theme(legend.background = element_rect(colour = 1))

ggarrange(f6abl, f6bbl,                   # combine figure f6a and f6b into one plot
          ncol = 2,                   # have figures on two columns (nrow = 1 should yield same result)
          labels = "auto",            # label figures in lowercase a, b
          common.legend = TRUE,       # both figures share a common legend
          legend = "bottom",           # place the shared legend on the bottom
          font.label = list(face = "plain")
) 

# outside border per panel and outside figure border

ggarrange(f6ab, f6bb,                   # combine figure f6a and f6b into one plot
          ncol = 2,                   # have figures on two columns (nrow = 1 should yield same result)
          labels = "auto",            # label figures in lowercase a, b
          common.legend = TRUE,       # both figures share a common legend
          legend = "bottom",           # place the shared legend on the bottom
          font.label = list(face = "plain")
) +
  theme(plot.background = element_rect(colour = "black", fill = NA, size = 1))

?plot.margin
?theme

### Add Boarders around Figure 6 if desired

  annotate_figure(f6,
    'rect',
    xmin = 40,
    xmax = 47,
    ymin = 210,
    ymax = 225,
    col = 'black')
            

?annotate_figure

# adjust plot.margins in each figure to increase/decrease spacing between figures
    # there is not a way to do that within ggarrange() itself


### Combine Fig 6a & 6b to create Figure 6

?ggarrange






##############      Below is code Dan and I worked on    ######################


## Below is code Dan and I worked on 

test = pandalus.percent 

test = subset(test, !is.na(PreyWt))
test = test |> group_by(pred.name, prey.name) |> summarize(prey.percent = sum(PreyWt))
test = test |> group_by(pred.name) |> mutate(pizza = prey.percent/sum(prey.percent))

test = test |> mutate(other.prey = prey.name == "other")
test$other.prey[test$other.prey == TRUE] = "other"
test$other.prey[test$other.prey == FALSE] = "Shrimp"

# stacking the count
ggplot(test, aes(x = other.prey, y = pizza, fill = prey.name)) +
  theme_bw() +
  geom_col(position = "stack") +
  facet_wrap(~ pred.name)


