################################################################################
################################################################################
#               Figure 4 Bar Samples per Zone - LEARNING CODE
################################################################################
################################################################################

# Author: Krista Kenyon
# Date: May 10/2024

# Document Purpose: Saving sources and example code I encountered/used while 
# creating the Figure 4 Grouped Bar Chart in case it's useful in the future.

# Document 'Sections' Include:
    # Creating figure dataset
        # subsection: tutorials
    # GGPLOT2 Code for Grouped Bar Charts
        # subsection: tutorials
    #  Unused but Interesting ggplot2 arguments



################################################################################
#                        Creating figure dataset
################################################################################

# Removing duplicates:

    # https://stackoverflow.com/questions/52038660/how-to-subset-your-dataframe-to-only-keep-the-first-duplicate
        # QA/QC'd that only duplicates were removed by comparing variable count in R to unique values in excel pivot tables


# sums data that fit 2 criteria. Creates new dataframe

    # https://stackoverflow.com/questions/25366929/manipulating-seperated-species-quantity-data-into-a-species-abundance-matrix
        # tutorial 1
        # reshape2() package needed
        # creates dataframe with desired information but unhelpful formatting ('wide format')


# reformatting 'wide' dataset to 'long' dataset

    # https://intro2r.com/wrangling-data-frames.html
        # tutorial 2
        # section 3.4.6 'reshaping dataframes'
        # reshape2() package needed
        # I needed the 'long' dataset format to create the grouped bar chart
        


######################### Tutorial 1 ########################

# https://stackoverflow.com/questions/25366929/manipulating-seperated-species-quantity-data-into-a-species-abundance-matrix

df <- data.frame (Site=c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3),
                  species=c("a","a","a","b","b","c","a","b","b","b","c","d","a","b","c","f","f","f"),
                  Quantity=c(3,1,1,2,3,3,5,12,1,2,4,1,5,6,3,1,1,1))

install.packages("reshape2")
library(reshape2) 
res <- dcast(df, Site~species, value.var="Quantity", sum)
colnames(res) <- c("site", paste0("Species", LETTERS[c(1:4,6)]))
res


######################### Tutorial 2 ########################

# section 3.4.6 of https://intro2r.com/wrangling-data-frames.html#reshape

# sample 'wide format' dataset
wide_data <- data.frame(subject = c("A", "B", "C", "D"),
                        sex = c("M", "F", "F", "M"),
                        control = c(12.9, 5.2, 8.9, 10.5),
                        cond1 = c(14.2, 12.6, 12.1, 12.9),
                        cond2 = c(8.7, 10.1, 14.2, 11.9))

wide_data # check 'wide dataset' structure

# convert 'wide' to 'long' dataset
library(reshape2)

my_long_df <- melt(data = wide_data,                                         #  melt() converts from wide format (species spread across rows) to long format (species compiled into one row)
                   id.vars = c("subject", "sex"),                            # id.vars : vector of variables I want to stack
                   measured.vars = c("control", "cond1", "cond2"),           # measured.vars : identifies the columns that have the data I want to stack
                   variable.name = "condition",                              # variable.name : what I want to call the stacked column
                   value.name = "measurement")                               # value.name : name of the column of my stacked measurements in my output data frame
my_long_df



################################################################################
#                        GGPLOT2 Code for Grouped Pie Charts
################################################################################

# I watched a variety of videos. Below list is what I remembered to save vs the exhaustive list

########################## links to helpful sources ############################


### Helpful tutorials - basic bar chart:

# (overview) https://r-charts.com/ranking/bar-plot-ggplot2/
    # tutorial 3
    # general overview of required dataset structure and basic formatting options



### Helpful tutorials - grouped bar chart:

# (dataset structure) https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2
    # tutorial 4
    # required dataset structure for grouped bar charts


# (overview) https://www.youtube.com/watch?v=Er-tXfGkL08
    # main arguments are broken down in a simple manner 
    # includes shifting to stacked bar charts and facet bar charts (multi-panel)
    # transparent chart color, remove gridlines, labels, theme (black and white)


### Helpful tutorials - formatting grouped bar charts:

# GREAT CHANNEL!!  https://www.youtube.com/watch?v=7nwgCxIANXA&list=PLu6UwBFCnlEe4ZiToQVQhHOX3CtDGnoBn&index=5
    # adding data labels directly above bars
    # he has TONS of SHORT videos for specific ggplot2 (and R) things!!!! Really good


# (removing boarders) https://www.youtube.com/watch?v=Rr2p5NCoS8c
    # theme that removed the left/side boarders and placed the bars directly on the x-axis
    # also seems excellent for other arguments/overview, but I this was my last 'missing' issue.
    # I didn't watch the whole video


# (specifying grid lines) https://stackoverflow.com/questions/64517930/how-can-i-change-the-grid-line-spacing-on-a-ggplot2-dotplot
    # I ended up just keeping the major y grid lines so did not use linked argument

# (cheat sheet) https://rstudio.github.io/cheatsheets/html/data-visualization.html#scales
    # technical breakdown of ggplot2 arguments



########################## Tutorial 3  ##################################

# Great overview of for basic bar chart https://r-charts.com/ranking/bar-plot-ggplot2/

### Data with category and count
df <- data.frame(group = c("A", "B", "C"),
                 count = c(3, 5, 6))

# if data already has total sum values
ggplot(df, aes(x = group, y = count)) +
  geom_bar(stat = "identity")

# geom_bar(stat="identity") is the same as geom_col()
ggplot(df, aes(x = group, y = count)) +
  geom_col()


### Only categorical data
df2 <- data.frame(cat = c("A", "A", "A", 
                          "B", "B", "B", "B", "B",
                          "C", "C", "C", "C", "C", "C"))

# if I want the bar chart to sum the observations within each category
ggplot(df2, aes(x = cat)) +
  geom_bar()

# There are many more details for basic adjustments on the webpage


########################## Tutorial 4  ##################################


# https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2

# helpful for me to understand the required dataset structure.

# create a dataset
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
dftest <- data.frame(specie,condition,value)
str(dftest)

head(dftest)

# Grouped
ggplot(dftest, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")



################################################################################
#                        Unused but Interesting ggplot2 arguments
################################################################################

# It's likely smart to save the list of ggplot arguments somewhere, including those I didn't use. I'm beggining that list here.

geom_bar([position not specified],  # turns bar chart into count stacked bar chart
              # using the facet wrap argument is nicer than above argument
         position = fill,          # turns bar chart into percentage stacked bar chart
         alpha = 0.5) +             # makes bars transparent (1 = no transparency. Adjust values as desired)
facet_wrap(~ variable) +            # creates side-by-side bar chart by specified variable.
        # facet_wrap: do each of the aes() + geom_bar() multiple times (based on # categories in specified variable), but in each portion do it only by specified variable
scale_y_continuous(expand=c(0,0)) +   # sets y axis limits but I don't understand argument structure. Would need to research
theme_bw()   +                      # black and white theme
theme(legend.position = "none",     # removes legend
      panel.spacing.y = unit(-1, "mm"),               # increases spacing between faceted plots
      plot.margin = margin(0.5, 0.5, 0.5, 0.5,)) +      # Adjusts white space around figure)    
labs(title = 'text') +


?theme   # pulls out all the options for defining the theme
?geom_tile()  # found this when trying to get the bars to sit directly on the x-axis. Didn't use it or explore it that much
