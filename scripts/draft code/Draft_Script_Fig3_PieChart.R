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
    # PLOTYL Pie Chart
        # subsection: plotyl example
    # GGPLOT2 Code for Pie Charts
    # Attempted dyplr coding to rename columns


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
        # tutorial 1
        # provides % details but also a good over-view

    # (ggplot2 Piechart) https://r-graph-gallery.com/piechart-ggplot2.html 
        # tutorial 2

    # (pie chart by 'Data to Fish' group) https://datatofish.com/pie-chart-r/
        # tutorial 4

    # Riffomonas Project https://www.youtube.com/watch?v=c_IUAw4T3L4
        # goes through common issues with pie charts and how to make them anyway


### Specific questions:

    # (formatting labels for 'count (%)') https://stackoverflow.com/questions/48184645/how-can-i-put-the-labels-outside-of-piechart
        # tutorial 3
        # introduced me to packages plotyl() and geom_label_repel(). 
        # I found a better solution for getting the labels outside the pie chart itself
            # geom_label(aes(x = #)). The x=# argument adjusts x-axis placement


########################## ggplot2 tutorial 1 ##################################

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


########################## ggplot2 tutorial 2 ##################################

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

########################## ggplot2 tutorial 3 ##################################

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

########################## ggplot2 tutorial 4 ##################################

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

## I tried the below code to rename with 'dyplr' package to rename column headings
    # but it wasn't working

# predsp.total %>% 
#   rename(
#     pred.name = Group.1, 
#     stomach.sum = x
#     )

# str(predsp.total)