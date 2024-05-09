################################################################################
################################################################################
#                        Figure 3 Pie Chart - Clean
################################################################################
################################################################################

# Author: Krista Kenyon (KAK)
# Date Created: May 6/2024
# Date Last Modified: May 8/2024 by KAK

# Document Purpose: Working document for creating the Figure 3 pie chart.
    # Working in code is simpler and faster here than in the R Markdown document
        # R Markdown places code results below chunks, which can mean a lot of scrolling
        # Not worried about accidentally knitting the whole document when testing out different code chunks
    # Current version is up to date clean code.

# Document Sections:
    # Remaining Tasks
    # Library's to Load
    # Chunk A: Importing and Prepping the Data
        # copy of R Markdown Chunk 2: import data
        # no modifications were made to this chunk
    # Chunk B: Creating the Pie Chart Figure
        # code transferred into the R Markdown Chunk 3: pie chart
    # Additional Notes


# To see tutorials, key webpages, and key videos I used to create and format the 
    # pie chart, please see the 'Draft_Script_Fig3_PieChart.R' in the 'Draft Code' folder


################################################################################
######################### Remaining Tasks ######################################
################################################################################

# I have stopped working on the pie chart formatting until I have the 2019 data set
    # The last issue is connecting the labels to their corresponding pieces while
        # outside the pie chart. To have this formatting look nice and connect properly
    # I am hoping the the 2019 data set has bigger slices to where I can just have
        # the labels be inside the pie pieces (potentially angled)
    # If I still need labels, the last formatting will need to be done anyway with 
        # the correct data set.

################################################################################
####################### Library's to Load ######################################
################################################################################
# library(ggplot2) 
# library(tidyverse)
# library(dplyr)
# library(ggrepel)

################################################################################
################### Importing and Prepping the Data ########################
###############################################################################

# This code chunk:
    # imports the data
    # manipulates the data it to create the base dataframe that will be used throughout the document
    # checks the dataframe structure

# The data imported must be structured consistently in order to utilize this coding.

# All code to check dataframe structure are commented out. If not they will produce outputs into the report.


############################ Key Data Structure Assumption #####################

# It is assumed that the imported data will initially have a structure consistent to the example saved immediately below. Particularly the variable names and variable data type (e.g. chr, int, num)

### Example dataset structure 

# 'data.frame':	2300 obs. of  16 variables:
#  $ content.key: chr  "72138002_892_1_1_-171_212" "72138002_892_1_1_-171_212" "72138002_892_1_1_-171_212" "72138002_892_1_1_-171_212" ...
#  $ prey.sp    : int  9998 9998 9998 9998 4770 4770 4770 4770 9998 9998 ...
#  $ weight     : num  0 0 0 0 0.0015 0.0015 0.0015 0.0015 0 0 ...
#  $ count      : int  0 0 0 0 1 1 1 1 0 0 ...
#  $ pred.sp    : int  892 892 892 892 892 892 892 892 892 892 ...
#  $ length     : int  40 40 40 40 45 45 45 45 52 52 ...
#  $ year       : int  2013 2013 2013 2013 2013 2013 2013 2013 2013 2013 ...
#  $ start.depth: int  823 823 823 823 823 823 823 823 823 823 ...
#  $ end.depth  : int  829 829 829 829 829 829 829 829 829 829 ...
#  $ lat.deg    : int  64 64 64 64 64 64 64 64 64 64 ...
#  $ lat.min    : num  27.9 27.9 27.9 27.9 27.9 ...
#  $ long.deg   : int  57 57 57 57 57 57 57 57 57 57 ...
#  $ long.min   : num  59.8 59.8 59.8 59.8 59.8 ...
#  $ sa.code    : chr  "2G" "RISA" "SFA2EX" "SFA3" ...
#  $ sort.key   : int  10 7 6 8 10 7 6 8 10 7 ...
#  $ trawl.id   : int  72138002 72138002 72138002 72138002 72138002 72138002 72138002 72138002 72138002 72138002 ...


####################### Importing Your Data ####################################


### import data

stomach <- read.csv('data/raw/2013_test_stomach_data.csv') 
# assumes header=TRUE and knows it's separated by commas

# str(stomach)
# check initial data structure. Does it match example?


################### Transforming Data into Base Dataframe ######################

# Additional data variables will be needed in the base dataframe for future figures tables including:
# column containing count of stomachs sampled per fish. 
# column with predator 'category' names
# Categories: Atlantic cod, Greenland halibut, Redfish, Skate


### Adding number stomach sampled per fish

stomach$stomach.count <- 1 


### Adding predator category

stomach$pred.name = stomach$pred.sp  
# creates new column that is duplicate of pred.sp, and renames to pred. name


stomach <- stomach %>% 
  mutate(pred.name=recode(pred.name, "438"="Atlantic cod", "892"="Greenland halibut", "793" ="Redfish","794"="Redfish", "90"="Skate"))  
# replaces species codes with predatory categories in new column. 
# currently the above code only includes the species codes for 2013


### Possible species codes for predator species categories
# Greenland halibut = 892
# Atlantic cod = 438
# Redfish = 792, 793, 794, 997, 998
# Skates = 80, 88, 89, 90, 91, 92, 94, 95, 96, 97

# See 'Predator Codes and Names.txt' file in 'Scripts' folder for list of species names attached to each code


################### Checking our Base Dataframe Modifications ##################

which(is.na(stomach))
# checking if there are blank values or NA's in the dataframe

str(stomach) 
# checking new dataframe structure to ensure all updates are good


### Saving Structure of Base Dataframe

# 'data.frame':	2300 obs. of  18 variables:
#  $ content.key  : chr  "72138002_892_1_1_-171_212" "72138002_892_1_1_-171_212" "72138002_892_1_1_-171_212" "72138002_892_1_1_-171_212" ...
#  $ prey.sp      : int  9998 9998 9998 9998 4770 4770 4770 4770 9998 9998 ...
#  $ weight       : num  0 0 0 0 0.0015 0.0015 0.0015 0.0015 0 0 ...
#  $ count        : int  0 0 0 0 1 1 1 1 0 0 ...
#  $ pred.sp      : int  892 892 892 892 892 892 892 892 892 892 ...
#  $ length       : int  40 40 40 40 45 45 45 45 52 52 ...
#  $ year         : int  2013 2013 2013 2013 2013 2013 2013 2013 2013 2013 ...
#  $ start.depth  : int  823 823 823 823 823 823 823 823 823 823 ...
#  $ end.depth    : int  829 829 829 829 829 829 829 829 829 829 ...
#  $ lat.deg      : int  64 64 64 64 64 64 64 64 64 64 ...
#  $ lat.min      : num  27.9 27.9 27.9 27.9 27.9 ...
#  $ long.deg     : int  57 57 57 57 57 57 57 57 57 57 ...
#  $ long.min     : num  59.8 59.8 59.8 59.8 59.8 ...
#  $ sa.code      : chr  "2G" "RISA" "SFA2EX" "SFA3" ...
#  $ sort.key     : int  10 7 6 8 10 7 6 8 10 7 ...
#  $ trawl.id     : int  72138002 72138002 72138002 72138002 72138002 72138002 72138002 72138002 72138002 72138002 ...
#  $ count.stomach: num  1 1 1 1 1 1 1 1 1 1 ...
#  $ pred.name    : chr  "Greenland halibut" "Greenland halibut" "Greenland halibut" "Greenland halibut" 

################################################################################
############## Chunk B: Creating the Pie Chart Figure ##########################
################################################################################

# The purpose of this code chunk is to create Figure 3

# The sections of this code chunk are:
    # Creating a new dataframe for Figure 3
    # Creating and Formatting Figure 3 pie chart
    # Recording Additional Geom_Label arguments that we may want to include in further edits 

# If desired, additional notes and links to tutorial youtubes/webpages I used to create Figure 3 are recorded in (scripts/draft code/Draft_Script_Fig3_PieChart.R)

# Those notes include other pie chart packages, functions, and arguments that I learned but did not end up using


################### Creating new dataframe for Figure #########################

# This dataframe will detail the total sum of stomachs sampled per predator category

# All code to check dataframe structure are commented out. If not they will produce outputs into the report.



### create figure dataframe

pred.total <- aggregate(stomach$stomach.count, list(stomach$pred.name), sum)

# pred.total  


### Renaming the columns in the new dataframe

names(pred.total)[names(pred.total) == "Group.1"] <- "pred.name"

names(pred.total)[names(pred.total) == "x"] <- "stomach.total"

# names(pred.total)


### Creating figure labels

pred.total$label <- paste(pred.total$stomach.total, sep = "
", 
paste0( "(", round(100 * pred.total$stomach.total / sum(pred.total$stomach.total), 0 ), "%)" ))
    # Label structure will be 'total stomach sampled (% of stomachs)'
    # DO NOT ADJUST THE SPACING AFTER [sep = "..."]. This splits the values into 2 lines. Adding spaces will uncenter second line. 
    # paste() puts spaces between labels
    # paste0() does not have spaces between labels. Which means there is no space between # and % (e.g. 40% vs 40 %)


### Checking Final Database Structure

# str(pred.total)


### Saving Final Database Structure 

# 'data.frame':	4 obs. of  3 variables:
#  $ pred.name    : chr  "Atlantic cod" "Greenland halibut" "Redfish" "Skate"
#  $ stomach.total: num  20 1340 900 40
#  $ label        : chr  "20\n(1%)" "1340\n(58%)" "900\n(39%)" "40\n(2%)"


################### Building and Formatting the Pie Chart ######################

# library(ggrepel)   # formats figure labels so they don't overlap


### Creating the pie chart
ggplot(pred.total, aes(x="", y=stomach.total, fill=pred.name)) +
  coord_polar(theta = "y") +                         # basing the circular coordinates on the y value (stomach.total)
  geom_col(color = "white", linewidth = 0.5) +       # creates white line separations between pieces
  theme_void() +                                     # removes the default theme. Which is grey background with circular gridlines outside the pie
  geom_label(aes(x = 1.65,                           #'x=#' moves labels along 'x-axis'. Goal here is outside the pie chart. 
                 label = label),                     # label = pred.total$label created above. with 'aes()' argument it knows we are in 'pred.total', so we don't need 'pred.total$..'
             size = 2.5,                             # adjusts size of label text
             position = position_stack(vjus =0.5),   # defines 'y-axis' position of label. 'vjus=0.5' is centered. 
             # fill = "white",                       # changes label fill colour. Default colour matches pie chart chunk colours
             #label.padding = unit(1.2, "mm"),       # adjusts space between label name and boarder
             #label.size =  nit(0.2, "mm"),          # adjusts thickness of label boarders
             #label.r = unit(3, "mm"),               # adjusts roundness of label corners
             # family = "montserrat",                # this argument would change the font, with "montserrat" being a font style (would need to import before chart)
             show.legend = FALSE) +                  # Removes geom_label default of adding an 'a' to legend colors.
  theme(legend.title = element_blank(),              # removes legend title
        legend.key.size = unit(3, "mm"),             # adjusts width of legend color symbols
        legend.key.height = unit(3, "mm"),           # adjusts height of legend color symbols 
        legend.key.spacing.y = unit(1, "mm"),)       # defines space between legend items


################################################################################
########################### Additional Notes ###################################
################################################################################

# Notes on different packages not used, or not yet used, are below.


################## Additional Notes on geom_label() ############################

# geom_label creates a box around the label. geom_text does not

# geom_label arguments that were not used because better solutions were found include:

      # hjust = "outward"   # Adjusts position label location and justification. 
                                # Other arguments include "inward", "left", "right", 1, 0, 0.5 

      # nudge_x = 0.6,      # moves the labels away from pie chart center. 'x=#' is much better solution
                                # 'nudge' does not center label in pie chart chunk (i.e. adjust position along 'y axis')
                                #  cannot use 'position' related arguments and nudge at the same time. 
                                    # 'position' arguments can be used to adjust label position along 'y-axis'
                                # formatting in general is tricky because the pie chart is an adjusted coordinate system of the bar charts.
                                # the 'x=#' adjusts label position along the 'x-axis' while allowing other arguments to adjust the 'y-axis'

      # family = "style"    # likely would need to install/load the font style in earlier code 


######################### Notes on geom_label_repel ############################

# replaces geom_label() with geom_label_repel()

# formats labels so they don't overlap.
# Default will push overlapping labels outside out defined x-axis and y-axis range. 
    # I haven't played around enough to solve this issue

# Default lines connecting the labels to center of the pie chart chunks rather than the outside.
    # It looks messy. And sometimes the lines don't actually connect to the right chunk.....

# I haven't played around with formatting to adjust defaults.
    # I'm waiting to have the actual data to see if that's needed.

# One additional argument that sets the minimum segment length is below
    # I don't fully understand this argument
    # min.segment.length = 0.25,



