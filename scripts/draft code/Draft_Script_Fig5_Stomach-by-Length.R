################################################################################
################################################################################
#               Figure 5 Stomach by Length & Depth - LEARNING CODE
################################################################################
################################################################################

# Author: Krista Kenyon
# Date: June 27/2024

# Document Purpose: Saving sources and example code I encountered/used while 
# creating the Figure 5 Paneled Bar Chart in case it's useful in the future.

# Document 'Sections' Include:
    # Creating figure dataset
        # subsection: tutorials
    # GGPLOT2 Code for Paneled Bar Charts
        # subsection: tutorials
    #  Unused but Interesting ggplot2 arguments


################################################################################
################################################################################
#                        Creating figure dataset
################################################################################
################################################################################


# creating depth ranges by multiple criteria

    # https://stackoverflow.com/questions/37321020/adding-new-column-with-conditional-values-using-ifelse
        # I decided that this method was a bit clunky but worked


# checking for NAs

    # https://www.codingprof.com/3-ways-to-find-columns-with-nas-in-r-examples/


# Creating categories based on numeric ranges

    # (Statistics Globe) https://www.youtube.com/watch?v=6CTwX0SbSxs
        # This is the process that I used
        # It's clunkier than I would like. But it works
        # I would love to find a smoother process for this


# I did not sum the sampled stomachs based on other criteria like I did for Fig 4

    # (Stack Overflow) https://stackoverflow.com/questions/63811380/how-to-make-stacked-bar-chart-with-count-values-on-y-axis
        # I found a geom_bar(stat="count") argument that automatically does that
        # It does not include a '0' data in the figure.
            # Fine for this figure
            # Not desired for Fig 4. So keeping Fig 4 coding


### Interesting Resources to Explore Simplifying Numeric Range to Category Code

# did not use the below code.
# I dislike my current clunky code for this process and think these may help me simplify it
# So something I want to look into further at a later date


    # (Stack Overflow) https://stackoverflow.com/questions/12979456/categorize-numeric-variable-into-group-bins-breaks
        # DID NOT USE - BUT WANT TO LEARN FUNCTIONS MENTIONED
        # findInterval() and cut() functions described
        # I couldn't follow and think of how to use them for my purpose
        # but they seem like they could lead to a simpler code


    # https://universeofdatascience.com/how-to-categorize-numeric-variables-in-r/
        # DID NOT USE - BUT WANT TO LEARN FUNCTIONS MENTIONED 
        # Totorial 1
        # describes different functions to convert numerical ranges to categories


    # (Riffomonas Project) https://www.youtube.com/watch?v=NVym44SdcaE
        # Interested in the dplyr package coding that he uses.
        # Both in this video, and he talks about doing setup in an earlier video
            # In theory linked in the video description

        


############################### Tutorial 1 ####################################

# source https://universeofdatascience.com/how-to-categorize-numeric-variables-in-r/

# description: DID NOT USE
    # Creates categories for numerical ranges
    # Would be interesting in exploring more to simplify my code

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
################################################################################
#                        GGPLOT2 Code for Stacked Faceted Bar Charts
################################################################################
################################################################################


### Helpful tutorials - paneled bar chart:

# (facet wrap code) https://www.youtube.com/watch?v=Er-tXfGkL08
    # main arguments are broken down in a simple manner 
    # face wrap coding (multi-panel)
    # transparent chart color, remove gridlines, labels, theme (black and white)
    # Channel: R programming 101


# (stacked bars within faceted barplot) https://www.youtube.com/watch?v=zwibty-NCnE
    # Tutorial 2
    # goes through dataframe structure and code to create faceted barplots with stacked bars
    # Channel: Statistics Globe

# (R Graph Gallery - Overview) https://r-graph-gallery.com/218-basic-barplots-with-ggplot2.html#width
    # AMANZING OVERVIEW OF ALL THE DIFFERENT GRAPHS
    # GREAT SOURCE FOR COLOUR PALLETTES
    # For Fig 5:
        # Adjusting bar width


# (auto-sum of stomach samples) https://stackoverflow.com/questions/63811380/how-to-make-stacked-bar-chart-with-count-values-on-y-axis
    # geom_bar(stat = "count")
    # described in text vs in code
    # DOWNSIDE is it will not include a column for a value that is '0'
    # Not an issue for this particular Fig
        # Not desired for Fig 4.

  
# (Ordering x-axis) https://stackoverflow.com/questions/65507942/arrange-names-of-bins-by-ascending-order-with-ggplot2
    # Tutorial 10
    # Ordering the x-axis based on the numerical values vs the character text values
  

# () https://rpubs.com/techanswers88/stackedbarcharts
    # OR https://www.youtube.com/watch?v=RPwJ6ExwPbg
    # Tutorial 3
    # Both sources same author and code
    # Basic stacked bar chart
    # rotating axis labels
    # themes, chart labels, data labels on bars,changing axis scales


# (multi-panel formatting) https://www.youtube.com/watch?v=pdBYAxE6xp4
    # Tutorial 4
    # REALLY HELPFUL!!!
    # thorough details on paneled charts. Includes panel rows, columns, scales
    # joining multiple figures together
    # adding panel letters
    # ends with including 2 trend-lines, 2 legends, and 2 r-values in one plot


# (facet_wrap arguments) https://www.rdocumentation.org/packages/ggplot2/versions/3.5.0/topics/facet_wrap


# (specify order of bars) https://www.youtube.com/watch?v=AFll5Auo8wc

# R programming 101 - Advanced ggplot series. 
    # First video is https://www.youtube.com/watch?v=sxknFa1rprY


### Legends


# (legends per facet)  https://stackoverflow.com/questions/14840542/place-a-legend-for-each-facet-wrap-grid-in-ggplot2
    # Tutorial 5


# (legends force entries with 0s)  https://stackoverflow.com/questions/33765710/force-ggplot-legend-to-show-all-categories-when-no-values-are-present
    # Tutorial 6
    # forcing legend to show all categories even if 0 observations


# (Legends) https://cran.r-project.org/web/packages/lemon/vignettes/legends.html
    # Lots of details about legends!
    # Multiple legends, moving legends, legends in facets, etc
    # Have not read it all


### Making the Charts Look Pretty


# (ggplot2 cheat sheet) https://rstudio.github.io/cheatsheets/html/data-visualization.html#x-y-location-scales
    # details ggplot2() arguments


# (Moving Panel Titles) https://r-charts.com/ggplot2/facets/
    # Facet Wrap panel titles adjusted to the top left
    # customizing the panels themselves
    # increasing spacing between panels


# (Re-Naming Panel Titles) https://stackoverflow.com/questions/48860158/changing-ggplot2facet-wrap-title-from-the-default
    # facet wrap - changing panel titles


# (Adjusting margins) https://r-charts.com/ggplot2/margins/
    # Adjusted margins per plot so there was nice spacing when joined


# (Moving axis titles) https://stackoverflow.com/questions/14487188/increase-distance-between-text-and-title-on-the-y-axis
    # moving axis titles using vjust 
        # great reference per desired direction


# (Moving axis titles) https://stackoverflow.com/questions/14487188/increase-distance-between-text-and-title-on-the-y-axis
    # changing distance between axis text and titles
    ### uses patchwork package vs ggarrange to merge charts!! Could look into this


# (Facet Boarders) https://stackoverflow.com/questions/67098543/facet-wrap-labels-as-panel-labels-in-ggplot
    # having boarders around panels
    # moving panel titles or labels inside the boarders


########################## Tutorial 2  ##################################


# From Statistics Globe channel https://www.youtube.com/watch?v=zwibty-NCnE

# goes through how to make stacked bars within grouped barplot


# Create example data frame
set.seed(687532)                  
data <- data.frame(facet = rep(LETTERS[1:5], each = 6),
                   group = c("x", "y"),
                   stack = letters[1:3],
                   value = round(abs(rnorm(30)), 2))

# Install & load ggplot2 package
# install.packages("ggplot2")          
library("ggplot2")


# Draw barplot with grouping & stacking
ggplot(data,                         
       aes(x = group,
           y = value,
           fill = stack)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ facet)


############################### Tutorial 10 ####################################

# source https://stackoverflow.com/questions/65507942/arrange-names-of-bins-by-ascending-order-with-ggplot2

# orders x-axis categorical 'number ranges' by their numerical values vs text values

# test code
data.bins1<-structure(list(Bin = c("400-500", "500-600", "600-700", "700-800", 
                                   "800-900", "900-1000", "1,000-1,100", "1,100-1,200", "1,200-1,300", 
                                   "1,300-1,400", "1,400-1,500", "400-500", "500-600", "600-700", 
                                   "700-800", "800-900", "900-1000", "1,000-1,100", "1,100-1,200", 
                                   "1,200-1,300", "1,300-1,400", "1,400-1,500"), 
                           variable = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                  2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), 
                                                .Label = c("2013", "2014"),  class = "factor"), 
                           value = c(10150, 4252, 5495, 4732, 2105, 2184, 720, 350, 
                                     407, 375, 320, 15404, 8201, 3901, 3469, 2547, 1594, 907, 
                                     515, 420, 354, 300)), row.names = c(NA, -22L), class = "data.frame")

# Code for ordering the x-axis bins

ggplot(data.bins1, aes(reorder(Bin,-value), value)) + 
  
  geom_bar(aes(fill = variable), 
           width = 0.4, 
           position = position_dodge(width=0.5), 
           stat="identity")   

theme(legend.position="top", legend.title =
        element_blank(),axis.title.x=element_blank(),
      axis.title.y=element_blank())+
  scale_color_manual(values = c("#00AFBB","#E7B800"))


# My version

ggplot(pred, aes(reorder(length.range, Length))) + 
  # orders the x-axis (length.range) based on smallest to largest values in 'Length' column
  
  geom_bar(aes(fill = depth), 
           width = 0.4, 
           stat="count") 


########################## Tutorial 3  ##################################


# 2 sources showing the exact same thing (same author?)

# source: https://www.youtube.com/watch?v=RPwJ6ExwPbg
# source: https://rpubs.com/techanswers88/stackedbarcharts

# description stacked bar chart tutorial
    # what's interesting is there is not a 'position' argument in geom_bar()

# code

head(ggplot2::mpg)

pl <- ggplot(data = mpg,aes(x= manufacturer, fill = class))

# auto calculates the count for the graph
pl <- pl + geom_bar(stat="count")

# rotates the x-axis to 90 using the angle = 90
pl <- pl  + theme(axis.text.x = element_text(angle = 90,hjust =0 ))
pl

# There is additional code for themes, chart labels, data labels on bars, 



########################## Tutorial 4  ##################################


# From Peeling Back Data channel https://www.youtube.com/watch?v=pdBYAxE6xp4

# thorough details on paneled charts. Includes panel rows, columns, scales
    # at the end goes over including 2 trendlines, 2 legends, and 2 r values in one plot


# facet_grid and facet_wrap -> allows us to see a relationship across factors

library(ggplot2)

data("diamonds")

diamonds_subset <- diamonds[seq(1, 50000, by = 100),]

# facet_grid
ggplot(data = diamonds_subset, aes(x = carat, y = price, color = cut)) +
  geom_point() +
  facet_grid(~ cut)   # creates different panels based on factors specified after ~

#facet_wrap -> can also specify the number of row and columns within the panels
ggplot(data = diamonds_subset, aes(x = carat, y = price, color = cut)) +
  geom_point() +
  facet_wrap(~ cut,
             nrow = 2,  # specifies the number of rows of panels
            # scales not included = panel axis scales are identical
            # scale = "free")  # panel scales for both axis independent of each other
           # scale = "free_x",  # panel scales for x-axis are independent of each other
            scale = "free_y")  # panel scales for y-axis are independent of each other


# ggarrange -> brings multiple ggplot objects in one image

library(ggpubr) # has ggarrange()

p1 <- ggplot(data = diamonds_subset, aes(x = carat, y = price, color = cut)) +
  geom_point() +
  facet_wrap(~ cut, nrow = 1) 

p2 <- ggplot(data = diamonds_subset, aes(y = price, color = cut)) +
  geom_boxplot() +
  facet_wrap(~ cut, nrow = 1) 

ggarrange(p1, p2, nrow = 2,       # binds p1 and p2 together in 2 rows
          labels = "AUTO")        # upper case letters in top left corner of panels
          # labels = "auto")      # lower case letters in top left corner of panels

?ggarrange


########################## Tutorial 5 ##################################


# From Stack Overflow https://stackoverflow.com/questions/14840542/place-a-legend-for-each-facet-wrap-grid-in-ggplot2

# two methods using the same package to add a legend to each panel

# Dataframe:

x <- data.frame(
  Date = factor(rep(
    c("12/1/2011", "1/2/2012", "2/1/2012", "2/10/2012", "2/13/2012"),
    3)),
  Server = factor(rep(c("A", "B", "C"), each = 5L)),
  FileSystem = factor(c(
    "/", "/var", "tmp", "/db", "/app", "C:", "D:", "F:", "/restore",
    "G:", "/", "/tmp", "/data", "/Storage", "/database")),
  PercentUsed = c(
    60L, 50L, 90L, 86L, 90L, 67L, 67L, 34L, 89L, 56L, 90L, 78L,
    67L, 34L, 12L  ))
x

# OG plot without legends per panel.
ggplot(x, aes(Date, PercentUsed, group=1, colour=FileSystem)) + 
  geom_jitter(size=0.5) + geom_smooth(method="loess", se=T) + 
  facet_wrap(~Server, ncol=1)

# Solution 2 (Which is simpler code)

library(gridExtra)

xs <- split(x,f = x$Server)
p1 <- ggplot(xs$A,aes(x = Date,y = PercentUsed,group = 1,colour = FileSystem)) + 
  geom_jitter(size=0.5) + 
  geom_smooth(method="loess", se=T) + 
  facet_wrap(~Server, ncol=1)

p2 <- p1 %+% xs$B
p3 <- p1 %+% xs$C

grid.arrange(p1,p2,p3)


########################## Tutorial 6 ##################################


# From Stack Overflow https://stackoverflow.com/questions/33765710/force-ggplot-legend-to-show-all-categories-when-no-values-are-present

# forcing legend to show all categories even if 0 observations

# example data OP was asking help with
set.seed(45678)
dat <- data.frame(Row = rep(x = LETTERS[1:5], times = 10), 
                  Col = rep(x = LETTERS[1:10], each = 5),
                  Y = rnorm(n = 50, mean = 0, sd = 0.5),
                  X = rnorm(n = 50, mean = 0, sd = 2))

library(ggplot2)
library(RColorBrewer)
library(dplyr)

dat <- dat %>% mutate(Y1 = cut(Y, breaks = c(-Inf,-3:3,Inf)),
                      X1 = cut(X, breaks = c(-Inf,-3:3,Inf)))

# Figure 2
ggplot(data =  dat, aes(x = Row, y = Col)) +
  geom_tile(aes(fill = Y1), color = "black") +
  scale_fill_manual(values = c("red", "blue", "green", "purple", "pink", "yellow", "orange", "blue"),
                    labels = c("cat1", "cat2", "cat3", "cat4", "cat5", "cat6", "cat7", "cat8"),
                    drop = FALSE) # drop false was the addition


################################################################################
################################################################################
#                        Unused but Interesting ggplot2 arguments
################################################################################
################################################################################


# It's likely smart to save the list of ggplot arguments somewhere, including those I didn't use. I'm beggining that list here.

geom_bar([position not specified],  # turns bar chart into count stacked bar chart
              # using the facet wrap argument is nicer than above argument
         position = fill,          # turns bar chart into percentage stacked bar chart
         alpha = 0.5)             # makes bars transparent (1 = no transparency. Adjust values as desired)

facet_wrap(~ variable) +            # creates side-by-side bar chart by specified variable.
        # facet_wrap: do each of the aes() + geom_bar() multiple times (based on # categories in specified variable), but in each portion do it only by specified variable

scale_y_continuous(expand=c(0,0)) +   # sets y axis limits but I don't understand argument structure. Would need to research

theme_bw()   +                      # black and white theme

theme(legend.position = "none",     # removes legend
      panel.spacing.y = unit(-1, "mm"),               # increases spacing between faceted plots
      plot.margin = margin(0.5, 0.5, 0.5, 0.5,)) +      # Adjusts white space around figure)    

labs(title = 'text') +

scale_fill_discrete(labels = c('text', 'text'))    # Specifies exact wording of legend labels


?theme   # pulls out all the options for defining the theme
?geom_tile()  # found this when trying to get the bars to sit directly on the x-axis. Didn't use it or explore it that much


# moving axis titles - reference vjust directions here https://stackoverflow.com/questions/14487188/increase-distance-between-text-and-title-on-the-y-axis



################################################################################
#################################################################################
#                             Creating a Base Theme for Plots
#################################################################################
################################################################################


# From Stack Overflow: https://stackoverflow.com/questions/14942681/change-size-of-axes-title-and-labels-in-ggplot2

# When creating many graphs and wanting a consistent theme (text size etc.). Save a theme first

My_Theme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 16))

# Then add My_Theme to the graphs

g + My_Theme

g1 + My_Theme