################################################################################
################################################################################
#                        Figure 3 Pie Chart - Draft Code
################################################################################
################################################################################


# Author: Krista Kenyon
# Date: May 8/2024

# Document Purpose: Saving sources and example code I encountered/used while 
    # creating the Figure 3 Pie Chart in case it's useful in the future.

# Document 'Sections' Include:
    # Base Prey Dataframe coding
    # Base Pred Dataframe coding
    # PLOTYL Pie Chart
        # subsection: plotyl example
    # GGPLOT2 Code for Pie Charts
        # subsection: tutorials
    # Attempted dyplr coding to rename columns


################################################################################
#                        Base Prey Dataframe Coding
################################################################################


### creating depth ranges by multiple criteria

    # https://stackoverflow.com/questions/37321020/adding-new-column-with-conditional-values-using-ifelse
        # I decided that this method was a bit clunky but worked



### Creating depth categories based on numeric ranges

    # (Statistics Globe) https://www.youtube.com/watch?v=6CTwX0SbSxs
        # My first version is based on this
        # It was clunky, so I recoded in dyplr


    # (Stack Overflow) https://stackoverflow.com/questions/12979456/categorize-numeric-variable-into-group-bins-breaks
        # used dplyr example to recode my first depth category version
        # this created smoother coding



### Binning predator length data

    # (data binning) https://www.statology.org/data-binning-in-r/
        # Good overview of different functions for binning data


    # https://universeofdatascience.com/how-to-categorize-numeric-variables-in-r/
        # Tutorial 1
        # Used simpler version to simplify my original clunky manual delineations 
        # binning with cut()


    # (Riffomonas Project) https://www.youtube.com/watch?v=NVym44SdcaE
        # Interested in the dplyr package coding that he uses.
        # Both in this video, and he talks about doing setup in an earlier video
        # In theory linked in the video description



### Reformating Binned Output

    # (Cheat Sheet - stringr) https://rstudio.github.io/cheatsheets/strings.pdf
        # explains code for selecting values to replace
        # I used code from the 'Anchors' section.
        # VERY HELPFUL


    # (Regular Expression Overview) https://www.youtube.com/watch?v=3toJ2LhvEfw
        # Riffomonas Project
        # Very Helpful.
        # Overview and breakdown what 'find and replace' coding is


    # (replacing last character) https://stackoverflow.com/questions/70607331/replace-last-characters-of-a-string-if-it-meets-criteria
        # helped me find code for selecting the last value ("x$")


    # (overview of regular expressions) https://www.programmingr.com/tutorial/gsub-in-r/
        # Helped me understand regular expression options and syntax
        # did not have the answer to selecting the last value that matched particular criteria


    # (reformatting bin output)  https://www.youtube.com/watch?v=kVVilIlsPEg
        # removing '()' from r code


    # (reformatting bin output) https://stackoverflow.com/questions/14718203/removing-particular-character-in-a-column-in-r
        # replacing or removing specified characters



### checking for NAs

    # https://www.codingprof.com/3-ways-to-find-columns-with-nas-in-r-examples/


#############################  Tutorial 1  #####################################


# source https://universeofdatascience.com/how-to-categorize-numeric-variables-in-r/

# description: used a simpler version to simplify my original clunky manual delineations
# Creates categories for numerical ranges

# code

data <- seq(1, 90, 2)
class(data)
## [1] "numeric"
length(data)
## [1] 45

Categories <- cut(data, breaks = c(-Inf,30,60,Inf), labels = c("Low","Medium","High"))
table(Categories)

Categories <- cut(data, breaks = c(-Inf,30,60,Inf), labels = c("Low","Medium","High"))
table(Categories)
## Categories
##    Low Medium   High 
##     15     15     15 

Categories <- cut(data, breaks = 3, labels = c("Low","Medium","High"))
table(Categories)


################################################################################
#                        Base Pred Dataframe Coding
################################################################################

# Removing duplicates:

    # https://stackoverflow.com/questions/52038660/how-to-subset-your-dataframe-to-only-keep-the-first-duplicate
        # QA/QC'd that only duplicates were removed by comparing variable count in R to unique values in excel pivot tables


################################################################################
#                        PLOTLY Pie Chart
################################################################################


# the plotyl() package is fabulous and user friendly for creating functional and 
    # elegent pie charts. It naturally contained all the formatting features I wanted

# HOWEVER - it uses HTML widgets and I couldn't figure out how to remove them, even
    # from the base code.

# These HTML widgets do not work in Word. And cause R Markdown to stop 'knitting'

# So I gave up on using this package for this specific project.

# If creating a pie chart in the future where HTML widgets are okay - USE THIS PACKAGE

# Tutorial website https://plotly.com/r/pie-charts/


######################### PLOTLY Pie Chart Example Code ########################


# code uses data from 2013 trail shrimp dataframe

colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)')

test <- plot_ly(predsp.total, labels = ~pred.name, values = ~stomach.sum, type = 'pie',
                textposition = 'outside',
                textinfo = 'values + percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1)))

#The 'pull' attribute can also be used to create space between the sectors)
# fig <- fig %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
# 
# fig


################################################################################
#                        GGPLOT2 Code for Pie Charts
################################################################################


# ggplot2 is not ideal for pie charts. It is not designed for it.
    # pie charts aren't often used in 'rigorous science' so there aren't many modifications...

# To create a pie chart using ggplot2, we force the coordinate system into a circle format
    # this makes formatting more interesting and finicky
    # the default arguments are for figures in a standard 'grid' format vs 'circle'


########################## links to helpful sources ############################


# My web-browser closed somehow so this isn't a comprehensive list. But should get people started



### Helpful tutorials:

    # GREAT TUTORIAL https://www.youtube.com/watch?v=cF4Z5BmdWk8
        # I used close captions.
        # He breaks the small steps down. 
        # Learned label/legend spacing 

    # GREAT TUTORIAL https://www.youtube.com/watch?v=152i9oCb6Ds
        # Also breaks the small steps down.
        # Where I learned the geom_label(aes(x = #)) code to adjust label x-axis position


    # (pie chart with %) https://r-charts.com/part-whole/pie-chart-percentages-ggplot2/
        # tutorial 2
        # provides % details but also a good over-view

    # (ggplot2 Piechart) https://r-graph-gallery.com/piechart-ggplot2.html 
        # tutorial 3

    # (pie chart by 'Data to Fish' group) https://datatofish.com/pie-chart-r/
        # tutorial 5

    # Riffomonas Project https://www.youtube.com/watch?v=c_IUAw4T3L4
        # goes through common issues with pie charts and how to make them anyway



### Specific questions:

    # (formatting labels for 'count (%)') https://stackoverflow.com/questions/48184645/how-can-i-put-the-labels-outside-of-piechart
        # tutorial 4
        # introduced me to packages plotyl() and geom_label_repel(). 
        # I found a better solution for getting the labels outside the pie chart itself
            # geom_label(aes(x = #)). The x=# argument adjusts x-axis placement


#########################   Tutorial 2 - ggplot2  ##############################


# R Charts Adding % to Pie (https://r-charts.com/part-whole/pie-chart-percentages-ggplot2/)


# Variables
ans <- sample(c("Yes", "No", "N/A"),
              size = 100, replace = TRUE,
              prob = c(0.4, 0.35, 0.25))
gen <- sample(c("Male", "Female"),
              size = 100, replace = TRUE)

# Change the levels of the variable so "Yes" appears first in the legend
ans <- factor(ans, levels = c("Yes", "No", "N/A"))

# Data frame
data <- data.frame(answer = ans,
                   gender = gen)

# install.packages("dplyr")
# install.packages("scales")
library(dplyr)

# Data transformation
df <- data %>% 
  group_by(answer) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

df

# install.packages("ggplot2")
library(ggplot2)

ggplot(df, aes(x = "", y = perc, fill = answer)) +
  geom_col() +
  coord_polar(theta = "y")


########################## Tutorial 3 - ggplot2 ##################################


# page outlining how to convert ggplot2 to pie charts https://r-graph-gallery.com/piechart-ggplot2.html


## One Internet Tutorial

# Compute the position of labels
pred <- predsp.total %>%
  arrange(desc(pred.name)) %>%
  mutate(prop = predsp.total$stomach.sum / sum(predsp.total$stomach.sum) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

# basic pie chart
testB <- ggplot(pred, aes(x="", y=prop, fill=pred.name)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +  # remove background, grid, numeric labels
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = pred.name), color = "white", size = 6) +
  scale_fill_brewer(palette = "Set1")
testB


########################## Tutorial 4 - ggplot2 ##################################


# From StackOverflow (https://stackoverflow.com/questions/48184645/how-can-i-put-the-labels-outside-of-piechart)

# I took this code formatting to create the labels in Fig 3 as: count (%)

# it introduced my to the plotyl() package, which I love but couldn't get to work for this report

# I found a better system to put the labels away from the center of the pie chart (i.e. along x-axis)
    # geom_label(aes(x = #)). The x=# argument adjusts the x-axis placement



### Code from the website

Product <- c("Product1","Product2","Product3","Product4","Product5","Product6","Product7")
Value <- c(1000000,200002,599996,1399994,2199992,2999990,3799988)
df <- data.frame(Product,Value)
df$Label <- paste(Product, paste(round(((df$Value/sum(df$Value))*100),2),"%"), sep="-")

#library(ggplot2)

ggplot(df, aes(x = 1, y = Value, fill = Product)) + geom_bar(stat = "identity") +
  coord_polar(theta = 'y') + 
  theme_void() +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5))



### My version or testing of this code

# creating the percentage portion of the labels
predsp.total$label <- paste(predsp.total$stomachsum, 
                            paste(round(((predsp.total$stomach.sum/sum(predsp.total$sum.count.stomac))*100),0),"%)"), sep="
(")
# pie chart
ggplot(predsp.total, aes(x="", y= stomach.sum, fill = pred.name)) +
  geom_col(color = "white") +               # outlines pie chunks in white
  geom_label(aes(label = label),
             color = "black",             # outlines pie labels in black
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +       # default (true) inserts an 'a' into the legend colours
  coord_polar(theta = "y") +            # changes coordinate system to circular from default square/rectangle x/y systme
  scale_fill_manual(values = c("#BE2A3E", "#EC754A",
                               "#EACF65", "#3C8D53")) +    # manually choose pie colour
  theme_void() +                           # removes default theme (ticks etc)
  theme(legend.title = element_blank())    # In new theme: removes legend title


########################## Tutorial 5 - ggplot2 ##################################


# From Data to Fish group (memorable name): https://datatofish.com/pie-chart-r/
  

# Sample data
  data <- data.frame(
    values = c(15, 25, 40),
    labels = c("X", "Y", "Z")
  )

# Build the pie chart
pie_chart <- ggplot(data, aes(x = "", y = values, fill = labels)) +
  geom_bar(stat = "identity", width = 1.5, color = "white") +
  coord_polar("y") +
  labs(title = "My Pie Chart") +
  theme_minimal() +
  theme(axis.text = element_blank(), 
        axis.title = element_blank(),
        legend.position = "bottom")

# Customizing colors (using html colors) and labels
pie_chart +
  scale_fill_manual(values = c("#fc9f9f", "#98ebb6", "#a5d9f0")) + 
  geom_text(aes(label = paste0(labels, ": ", values, "%")), position = position_stack(vjust = 0.5)) +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.text = element_text(size = 12, face = "italic"),
        axis.line = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))


################################################################################
#                        Attempted dyplr Coding
################################################################################


# I tried the below code to rename with 'dyplr' package to rename column headings but it wasn't working

# predsp.total %>% 
#   rename(
#     pred.name = Group.1, 
#     stomach.sum = x
#     )

# str(predsp.total)


################################################################################
#                 Unused Code for Binning Depth Ranges
################################################################################


# Reformatted the below code into dyplr so that it could all be run at once

pred$depth[pred$Start.Depth >= 100 & pred$End.Depth >= 100 &
             pred$Start.Depth <= 200 & pred$End.Depth <= 200] <- "100-200 m"

pred$depth[pred$Start.Depth >= 200 & pred$End.Depth >= 200 &
             pred$Start.Depth <= 300 & pred$End.Depth <= 300] <- "200-300 m"

pred$depth[pred$Start.Depth >= 300 & pred$End.Depth >= 300 &
             pred$Start.Depth <= 400 & pred$End.Depth <= 400] <- "300-400 m"

pred$depth[pred$Start.Depth >= 400 & pred$End.Depth >= 400 &
             pred$Start.Depth <= 500 & pred$End.Depth <= 500] <- "400-500 m"

pred$depth[pred$Start.Depth >= 500 & pred$End.Depth >= 500 &
             pred$Start.Depth <= 750 & pred$End.Depth <= 750] <- "500-750 m"


################################################################################
#################################################################################
#                 Unused Code for Binning Predator Length
#################################################################################
################################################################################


# I don't like how clunky my current code is and that it's tied to max predator length for annual repeatability.
# If future years sample fish outside the current defined length categories, they will appear as NAs on the chart

# With binning the data, I like that it can be tied to predator max length, and that it's a smoother code
# There are 2 issues:

# Issue 1 with binning: 
# I can't figure out how have the binning say [length < 6] then [length >= 6 & <11] etc.
# That is how the OG report presents the data. And I would like it to be consistent.
# The binning so far goes to say, 0-5, and then 5-10, and then 5-15. Vs 0-5, 6-10, 11-15.

# Issue 2 with binning:
# I figured out how to convert the default binning output (0,6] into the 'range' format 0-6.
# However each bin perfectly boarders each other. 
#(0,5] , (5,10] (10,15] etc. So converted 0-5, 5-10, 10-15 etc
# I would want the ranges to read 0-5, 6-10, 11-15 etc.
# I could probably put some code to replace the first number in the series by 'it's number + 1'.
# That would be something I would have to test out
# But the problem is that those ranges are the data the bins are actually grabbing.
# So then I'm mis-labelling the graph or mis-representing the data

# So I would need to solve 'Issue 1' before I look into 'Issue 2'

# Both would need to be solved to create the figure I would want, but Issue 1 is the key problem


### TESTING HOW TO BEST CREATE PREDATOR LENGTH CATEGORY


########################### Summary of Test ##################################


# testD was success and retained

pred # main OG file - manual breaks.

testA <- pred # first attempt of binning (breaks of 5)
testB <- pred # second attempt of binning (breaks to 6, then breaks of 5)
testC <- pred # third attempt of binning (start at 1, breaks at 5)
testD <- pred # fourth attempt of binning (breaks at 5.99)


########################## Original Attempt #################################


# This code DOES create the desired effect but I did not want to use it because:
# it wasn't tied to max length data, so repeatability across years isn't guaranteed
# it is rather clunky

# Note: any values outside of these defined ranges in future years will appear as NA in the charts.

## creating new column filled with NAs (makes QA easier)
pred$length.range <- NA

# Creating the depth range categories
pred$length.range[pred$Length < 6] <- "0-5" 
pred$length.range[pred$Length >= 6 & pred$Length < 11] <- "6-10"
pred$length.range[pred$Length >= 11 & pred$Length < 16] <- "11-15"
pred$length.range[pred$Length >= 16 & pred$Length < 21] <- "16-20"
pred$length.range[pred$Length >= 21 & pred$Length < 26] <- "21-25"
pred$length.range[pred$Length >= 26 & pred$Length < 31] <- "26-30"
pred$length.range[pred$Length >= 31 & pred$Length < 36] <- "31-35"
pred$length.range[pred$Length >= 36 & pred$Length < 41] <- "36-40"
pred$length.range[pred$Length >= 41 & pred$Length < 46] <- "41-45"
pred$length.range[pred$Length >= 46 & pred$Length < 51] <- "46-50"
pred$length.range[pred$Length >= 51 & pred$Length < 56] <- "51-55"
pred$length.range[pred$Length >= 56 & pred$Length < 61] <- "56-60"
pred$length.range[pred$Length >= 61 & pred$Length < 66] <- "61-65"
pred$length.range[pred$Length >= 66 & pred$Length < 71] <- "66-70"
pred$length.range[pred$Length >= 71 & pred$Length < 76] <- "71-75"
pred$length.range[pred$Length >= 76 & pred$Length < 81] <- "76-80"
pred$length.range[pred$Length >= 81 & pred$Length < 85] <- "81-85"
pred$length.range[pred$Length >= 86 & pred$Length < 91] <- "86-90"
pred$length.range[pred$Length >= 91 & pred$Length < 96] <- "91-95"
pred$length.range[pred$Length >= 96 & pred$Length < 101] <- "96-100"
pred$length.range[pred$Length >= 101 & pred$Length < 106] <- "101-105"
pred$length.range[pred$Length >= 106 & pred$Length < 111] <- "106-110"
pred$length.range[pred$Length >= 111 & pred$Length < 116] <- "111-115"
pred$length.range[pred$Length >= 116 & pred$Length < 121] <- "116-120"
pred$length.range[pred$Length >= 121 & pred$Length < 126] <- "121-125"
pred$length.range[pred$Length >= 126 & pred$Length < 131] <- "126-130"
pred$length.range[pred$Length >= 131 & pred$Length < 136] <- "131-135"
pred$length.range[pred$Length >= 136 & pred$Length < 141] <- "136-140"
pred$length.range[pred$Length >= 141 & pred$Length < 146] <- "141-145"
pred$length.range[pred$Length >= 146 & pred$Length < 151] <- "146-150"
pred$length.range[pred$Length >= 151 & pred$Length < 156] <- "151-155"
pred$length.range[pred$Length >= 156 & pred$Length < 161] <- "156-160"
pred$length.range[pred$Length >= 161 & pred$Length < 166] <- "161-165"
pred$length.range[pred$Length >= 166 & pred$Length < 171] <- "166-170"
pred$length.range[pred$Length >= 171 & pred$Length < 176] <- "171-175"
pred$length.range[pred$Length >= 176 & pred$Length < 181] <- "176-180"
pred$length.range[pred$Length >= 181 & pred$Length < 185] <- "181-185"
pred$length.range[pred$Length >= 186 & pred$Length < 191] <- "186-190"
pred$length.range[pred$Length >= 191 & pred$Length < 196] <- "191-195"
pred$length.range[pred$Length >= 196 & pred$Length < 201] <- "196-200"


colSums(is.na(pred)) 


########################## First Bin Attempt #################################


# Source: https://www.statology.org/data-binning-in-r/ 

# Issue: Below results in 0-4.99, then 5 to 9.99 etc. I want 0-5.99, 6-10.99

# create the bins
testA <- testA %>% 
  mutate(length.range = cut(Length, breaks=seq(from = 0, 
                                               to = max(Length) +5, 
                                               # max(Length) ends bins based on largest Length value. 
                                               # The '+5' ensures the max value itself is included in the bin
                                               by = 5)))
# each bin is 5 units large

# Reformat the bin output (#,#] to range output #-#
testA$length.range <- gsub(',', '-', testA$length.range)
# gsub -> replaces all occurrences of first specified character(s) with the second
testA$length.range <- gsub('[(]', '', testA$length.range) 
# When replacing a bracket (, [, -> the bracket needs to be surrounded by []
testA$length.range <- gsub('[]]', '', testA$length.range)


############################# Second Bin Attempt ##############################


# Trying to solve the issue with the First Bin Attempt :
# Bin it to 6, and then bin every 5 numbers from there
# Hoped that it would split the ranges into:
# [length < 6] then [length >= 6 & <11] etc.

# It did not solve the issue

# create the bins
testB <- testB %>% 
  mutate(length.range = cut(Length, breaks=seq(from = 0, to = 6, by = 6))) %>%
  mutate(length.range = cut(Length, breaks=seq(from = 6, to = max(Length)+5, by = 4)))

# reformat the bin output
testB$length.range <- gsub(',', '-', testB$length.range) 
testB$length.range <- gsub('[(]', '', testB$length.range) 
testB$length.range <- gsub('[]]', '', testB$length.range)

# This did not solve the issue. It just bumped the ranges up by 1 value


######################### Third Bin Attempt ####################################


# Trying to solve the issue with the First Bin Attempt :
# Start bins at 1, and then bin every 5 numbers from there
# Hoped that it would split the ranges into:
# [length < 6] then [length >= 6 & <11] etc.

# It did not solve the issue

# create the bins
testC <- testC %>% 
  mutate(length.range = cut(Length, breaks=seq(from = 1, to = max(Length)+5, by = 5)))

# reformat the bin output
testC$length.range <- gsub(',', '-', testC$length.range) 
testC$length.range <- gsub('[(]', '', testC$length.range) 
testC$length.range <- gsub('[]]', '', testC$length.range)

# This did not solve the issue. It just bumped the ranges up by 1 value


#################### Fourth - Successful - Binning Attempt ###################


# I want 0-5.99, 6-10.99 etc for the bins:

# binning the data
testD <- testD %>% 
  mutate(length.range = cut(Length, breaks=seq(from = 0, to = 5.99, by = 5.99))) %>%
  # establishes the first bin to end at 5.99 (essentially < 6)
  mutate(length.range = cut(Length, breaks=seq(from = 5.99, to = max(Length)+5, by = 5)))
# from that boundary, bin by 5 numbers


# Reformatting the binned outputs 
testD$length.range <- gsub(',', '-', testD$length.range) 
testD$length.range <- gsub("[(]|[]]", '', testD$length.range)
testD$length.range <- gsub("5.99", "6", testD$length.range)

testD$length.range <- str_replace(testDa$length.range, "1$", "0")
testD$length.range <- str_replace(testDa$length.range, "6$", "5")


#################### Comparing the Tests to OG Output ##########################


# creating vectors with the number of observations within each number range
p <- pred %>% count(length.range)

ta <- testA %>% count(length.range)

tb <- testB %>% count(length.range)

tc <- testC %>% count(length.range)

td <- testD %>% count(length.range)

# visually comparing them (I'm sure there's cleaner ways but)
p
ta
tb
tc
td

# the outputs for p and td match.

# the outputs for p and ta/tb/tc do not match


######################### Testing new Code in charts ##########################


ggplot(testB, aes(reorder(length.range, Length))) +  
  
  geom_bar(aes(fill = depth),
           position = "stack",
           stat="count",
           width = 0.4) +    
  
  facet_wrap(~ pred.name, 
             nrow = 5, 
             scale = "free_y",
             axes = "all_x")