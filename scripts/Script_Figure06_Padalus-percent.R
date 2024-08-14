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



### load libraries

library(tidyverse)
library(dplyr)
library(ggplot2) 


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


################### Creating New Dataframe for Figure 6 ########################


### Creating new dataframe for Table 2

pandalus.percent <- prey %>%             # create new dataframe based on 'pred' 
  
  select(Prey_OS_ID,                     # subsets dataframe by selected columns
         pred.name,
         PreyWt,
         Prey_Count) %>%
 
# Step 1: remove rows with 'empty stomachs'. Leave unidentified for now. 
  
  filter(                           # selects rows that do not contain:
    Prey_OS_ID != 9998 |                # empty
    Prey_OS_ID != 9981 |                # sand
    Prey_OS_ID != 9982 |                # stone
    Prey_OS_ID != 9983 |                # shells
    Prey_OS_ID != 10757 |               # mud
    Prey_OS_ID != 9987)  %>%               # plant material


# Step 2: create new column where OS_ID's are redifined to the 4 prey categories

    # shrimp P. borealis        (OS_ID code = 8111)
    # shrimp P. montagui        (OS_ID code = 8112)
    # shrimp Pandalus. sp.      (OS_ID code = 8110)

# test <- pandalus.percent %>%
  mutate(prey.name = ifelse(Prey_OS_ID == 8111,     # create new column 'Full' with values based on logical check
                     "borealis",                      # value if logical check is TRUE
                    ifelse(Prey_OS_ID == 8112,
                           "montagui",
                           ifelse(Prey_OS_ID == 8110,
                                  "Pandalus",
                                  "other")
                    ))) %>%
  
  subset(!is.na(PreyWt))  %>%            # removes NAs from PreyWt column only
  group_by(pred.name, prey.name) |> summarize(prey.percent = sum(PreyWt), .groups = "keep") %>%  # sum PreyWt by prey.name per pred.name
  group_by(pred.name) |> mutate(pizza = prey.percent/sum(prey.percent)) %>%   
  
  mutate(other.prey = prey.name == "other")

pandalus.percent$other.prey[pandalus.percent$other.prey == FALSE] = "Shrimp"
pandalus.percent$other.prey[pandalus.percent$other.prey == TRUE] = "other"


pandalus.percent$pizza <- pandalus.percent[['pizza']]*100    # format into percentage



### Saving the pandalus.percent dataframe structure

# str(pandalus.percent) 



### Exporting pandalus.percent Dataframe

write.csv(pandalus.percent, 
          file="data/processed/2019_F6_pandalus.percent.csv",
          row.names = FALSE)                # removes auto-generated unique ID row


###################    Building and Formatting Figure 6    ######################

names(pandalus.percent)

### Load Figure 6 dataframe

# pandalus.percent <- read.csv('data/processed/2019_F6_pandalus.percent.csv')

geom_bar(aes(fill = depth),
         position = "stack",
         stat="count",
         width = 0.4)    # changes the width of the bars 
  
?geom_col
?theme

### Build Figure 6a

ggplot(pandalus.percent, 
       aes(x = other.prey, y = pizza, fill = prey.name)) +
  
  theme_minimal() +
  
  geom_col(position = "stack",
           width = 1) +
          # width = 0.8) +                                      # reduces column width
  
  facet_wrap(~ pred.name,
             ncol = 4,                                         # plots in 4 columns (so all appear in 1 row)
             strip.position = "bottom") +                      # moves the facet strip to the bottom
  
  labs(x = "Predator Species",
       y = "%W") +
  
  scale_fill_manual(values = c("other" = "grey",               # assigns colour to prey sp. categories
                    "Pandalus" = "#CCEDB1",
                    "borealis" = "#41B7C4",
                    "montagui" ="#FF9999")) +
  
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(breaks = seq(0, 100, by = 25)) +
  
  scale_x_discrete(expand = c(1, 1)) +                          # adds padding around the bars
  
  
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
        plot.background = element_rect(fill='transparent', color=NA),
        strip.text = element_text(vjust = 6)
  )
        
  #       strip.text = element_text(vjust = -10)) +       # pulls panel titles to the top left
  




### Build Figure 6b



### Combine Fig 6a & 6b to create Figure 6


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


###################################################################################
################################################################################

# Below is old code

##################################################################################
##################################################################################

############ Below is old code

# new columns containing the summarized values defined in summarise()

test <- pandalus.percent %>%
  group_by(pred.name, PreyWt) %>%        # group by specified categories
  summarise('Weight' = sum(PreyWt),                 # 'New Column' = sum('Old Column') 
            'Count' = sum(Prey_Count),
            .groups = "keep")             # tells R to keep current group structure
# gives warning that goes into Markdown document if '.groups' not specified

# Step 3: create % weight Figure that does percentage per fish category
# fish category may have to be grouped first to do this


########### testing figures

# stacking the count
ggplot(data = pandalus.percent,
       aes(fill = prey.name, x = pred.name, y = PreyWt)) +
  geom_bar(position="stack", stat = "identity")

# percent count
ggplot(data = pandalus.percent,
       aes(fill = prey.name, x = pred.name, y = PreyWt)) +
  geom_bar(position="fill", stat = "identity")

# geom_bar(aes(position = "fill",
#          stat="count",
#         width = 0.4))

?geom_bar



# Step 4: create # number Figure
# when feeding data in, filter out unidentified categories

# Step 5: see if these figures can be combined into one multi-panneled figure
# concern is that they use slightly different data (i.e. unidentified)

# Step 6: create either 1 figure or the 2 plots that then get merged into one figure

# Step 7: format figure



# trying to figure out how I wan my data structured.....
names(pandalus.percent)

# new column of Prey_OS_ID that fits things to the 4 prey categories


##### Below is old code

stomach.ratio  <- pred %>%               # create new dataframe based on 'pred'                    
  
  select(Prey_OS_ID,                    # subsets dataframe by selected columns
         pred.name, 
         length.range, 
         sampled.stomach) %>%
  
  
  
  ### Assigning Full vs Empty Stomachs
  # if I add these in, THEN I MUST FILTER STOMACHS FROM PREY DATABASE THAT ONLY HAVE THESE
  # THIS IS KEY - the pred df has indiscriminately removed duplicates. 
  # Fine for Empty Stomachs.
  # NOT fine when there could have been multiple prey/objects in stomachs
  # other potential 'Empty Stomachs' codes:
  # 2222 = Plant
  # unknown????? (if yes, 10746-10750)
# other. 9981 (sand), 9982 (stone), 9983 (shells), 9993 (bait), 9995 (other offal), 10757 (mud)


mutate(Full = ifelse(Prey_OS_ID == 9998,     # create new column 'Full' with values based on logical check
                     0,                      # value if logical check is TRUE
                     1)) %>%                 # value if logical check is FALSE
  
  mutate(Empty = ifelse(Prey_OS_ID == 9998,   # Prey_OS_ID of 9998 = 'Empty'
                        1,
                        0))  %>%
  
  
  ### Assigning Groups for Table Layout 
  
  # desiring stomach data summed and displayed by:
  # fish length group (column 1) and 
  # predator category group (rows) 
  
  group_by(length.range, pred.name) %>%        # group by specified categories
  
  
  
  ### Sum Stomach Data by Groups
  
  # sums the specified values by the grouped categories
  # e.g. total 'full' stomachs by predator length and category
  # creates new dataframe that contains:
  # grouped variables (e.g. length.range, pred.name)
  # new columns containing the summarized values defined in summarise()
  
summarise('Full' = sum(Full),                 # 'New Column' = sum('Old Column') 
          'Empty' = sum(Empty),
          'Total' = sum(sampled.stomach),
          .groups = "keep")  %>%              # tells R to keep current group structure
  # gives warning that goes into Markdown document if '.groups' not specified

  
#############   Formatting Dataframe Structure for Table    ####################



### Final Table Structure Formatting

# merge predator categories with the full, empty, total columns.

# columns will now be 'stomach-predator' (e.g. Full-Greenland Halibut)  
# rows will be predator length

pivot_wider(names_from = pred.name,                       # categorical values to pull from rows to columns
            values_from = c(Full, Empty, Total)) %>%      # numerical values to fill in those columns
  
  
  # Re-Ordering Columns 
  
  relocate(contains('cod'), .after = length.range) %>%     # moves any column with name containing 'cod' to the right of length.range
  relocate(contains('Greenland'), .after = contains('cod')) %>%
  relocate(contains('Red'), .after = contains('Green'))  %>%
  
  
  # Re-Order Rows from Smallest to Largest Fish
  
  # length.range not naturally sorted because format is 6-10 vs 006-10
  # so I added 0s, sorted, then removed the extra 0s
  
  mutate(length.range = str_pad(length.range,     # code adds 0s to start of length.range string (6 becomes 006)
                                width=6,          # strings <6 characters will be padded until they =6 characters
                                pad = "0")) %>%   # padding with 0s
  # width = 6 bumps all strings to ###- (006-, 016-, 141-)
  
  arrange(length.range) %>%                       # sort smallest to largest now that length.range strings equal character length before '-'
  
  mutate(length.range = str_replace(length.range, # removing the extra zeros for Table formatting
                                    "^0{1,}",     # select 1+ '0's at the beginning of strings
                                    "")) %>%         # remove selected '0's
  
  
  # Replace 0 and NA values with '-' 
  # I could not select and replace across the entire dataframe within dyplr
  # I had to use Base R for Step 2
  # Step 1: replacing 'NA' with 0 in dyplr (replace)
  # Step 2: replacing '0' with '-' in base R (lapply) and stringr (str_replace_all)
  
  
  replace(is.na(.), 0)                                 # if there are NA's, replace with 0

stomach.ratio <- data.frame(lapply(stomach.ratio, function(x){    # honestly - I don't know what above code does but it works
  str_replace_all(x,                                              # will be related to the code in the above line. But Idk who it actually works
                  "(?<!\\S)0(?!\\S)",                             # select '0' with nothing before or after
                  "-")                                            # replace with '-'
})) 







