################################################################################
################################################################################
#                   Figure 6 Pandalus Percent - LEARNING CODE
################################################################################
################################################################################


# Author: Krista Kenyon
# Date: Aug 7/2024

# Document Purpose: Saving sources and example code I encountered/used while 
    # creating Figure 6 in case it's useful in the future.

# Document 'Sections' Include:
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
#                        Creating figure dataset
################################################################################


### Designing Grouped and Stacked Table

    # (inspiration) https://statisticsglobe.com/draw-stacked-bars-within-grouped-barplot-r
        # table design is tricky. This gave inspiration for our solution
        # split out 'other' and stack both categories
            # have % already calculated
        # have the panels be the fish predators
        # format everything so it looks like all one graph


### Multiply a Specific Column

    # (multiply specific column) https://stackoverflow.com/questions/57508123/how-to-multiply-specific-columns-of-a-dataframe#:~:text=You%20need%20to%20use%20double%20square%20brackets%20to,shorten%20your%20code%20a%20bit%3A%20quality%20%2A%3D%20100
        # need [[]] to multiply a specific column


################################################################################
#              Figure Formatting - ggplot2 - facet & stacked
################################################################################


# This figure was faceted, and then formatted to appear as if it was 1 figure


### Facet Panel Formatting

    # (facet titles & position) https://www.statology.org/ggplot-facet-axis-labels/
        # how to move facet (strip) titles location on figure
        # how to rename facet (strip) titles


    # (dynamically wrap labels) https://stackoverflow.com/questions/16654691/how-to-dynamically-wrap-facet-label-using-ggplot2
        # I didn't use this but thought it was neat so I saved it


    # (facet titles overview) https://www.youtube.com/watch?v=v1hTB2b_YkE
        # GREAT overview
        # use facet title as axis title
        # stylizing facet title
        # rename facet (strip) titles


### Formatting Axis

    # (axis overview) https://r-charts.com/ggplot2/axis/
        # GREAT overview!
        # anything plot axis related is here!


    # (move axis labels) https://statisticsglobe.com/adjust-space-between-ggplot2-axis-labels-and-plot-area-in-r
        # how to adjust axis labels vertically and horizontally 


    # (axis padding, limits & labels) https://www.youtube.com/watch?v=UVriK-W1DNg
        # GREAT overview!
        # limits - why use different arguments to define axis limits?
        # labels - incorporating Markdown syntax to get italics and line breaks
        # padding - expand()  !!! such a life saver
            # padding - adjusts white space around figure axis
            # gets labels/titles directly under figure
            # I used for creating space between facets while looking like one figure


### Formatting text size across table

    # (base_size) https://www.youtube.com/watch?v=RfdcjMP2qMM
        # change text size across figure (minus titles)
        # theme_preset(base_size = ##)
        # e.g. theme_minimal(base_size = 16)



### Order or Variables in Stacked Bars

    # (define order) https://www.statology.org/ggplot-reorder-stacked-bars/#:~:text=You%20can%20use%20the%20following%20basic%20syntax%20to,stacked%20bar%20chart%20ggplot%28df%2C%20aes%28x%3Dx_var%2C%20y%3Dy_var%2C%20fill%3Dfill_var%29%29%20%2B
        # I inserted the factor() within the ggplot(aes()) itself
        # ggplot(aes(fill = factor(variable, levels = c(xxx,xxx))))

    # (define order) https://www.youtube.com/watch?v=w4X3o6MQjVA



### Formatting Legend

    # (italics) https://stackoverflow.com/questions/59554096/ggplot2-italics-in-the-legend
        # uses Markdown syntax to insert italics

    # (overview) https://www.youtube.com/watch?v=w4X3o6MQjVA
        # reordering labels
        # using Markdown syntax
        # ordering variables in stacked bar charts

    # (overview) https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/

    # (overview) https://r-charts.com/ggplot2/legend



### Spacing around columns without impacting facet spacing

    # (WINNER!! - expand!) https://www.youtube.com/watch?v=UVriK-W1DNg
        # THIS ONE IS WHAT I CHOSE
        # adds padding on scales (including discrete) within the graph code
        # expand() argument


    # (width of columns) https://stackoverflow.com/questions/12040245/how-to-increase-the-space-between-the-bars-in-a-bar-plot-in-ggplot2
        # width = 1 forces column to touch


    # (add blank column) https://stackoverflow.com/questions/45701456/how-to-control-space-between-stack-bars-in-ggplot2
        # Tutorial 1
        # I experimented with but found a MUCH BETTER solution (expand)
        # this could work, but requires adding blank rows in dataframe to create blank columns
            # besides 'adding data', this bank data causes R to produce a Warning.
            # Markdown would include this warning message
        # it also off-centers the bars from the labels
            # this is fixable, but would require time to solve


    # (unequal space changes) https://stackoverflow.com/questions/36823526/add-different-amount-of-extra-space-on-both-sides-of-discrete-ggplot-x-axis
        # Tutorial 2
        # I didn't use this method
        # details on shrink space on one side of the axis, while expanding on the other



### Figure Borders

    # (borders around figure) https://stackoverflow.com/questions/26191833/add-panel-border-to-ggplot2
        # Border outside of figure: theme(plot.background = )
            # 'size' argument in plot.background() has been replaced with 'linewidth'
        # Border overlaping axis: theme(panel.border = )

    # (panel border) https://dcodesnippet.com/ggplot-add-border-around-plot/

    # copilot suggested ggarrange(figures) + border()

    # (annotate intro) https://www.youtube.com/watch?v=RfdcjMP2qMM
        # draw boxes, text, curved arrows etc on top of figures


### Transparency in labels

# I was trying to sort out why the Facet (strip) Titles disappeared when I moved them immediately below the chart
# At first I thought it was that the x-axis labels - while removed - retained a white background
# So I tried to find a way to make that transparent
# HOWEVER - that was not the case.
# The white space was caused by padding on the y-axis
# once that was removed, the Facet Titles automatically went directly below the bars

# Below are the websites I found discussing making chart sections transparentt

    # https://stackoverflow.com/questions/54241972/transparency-in-x-and-y-labels-ggplot2-r
    # https://genchanghsu.github.io/ggGallery/posts/2021-07-10-post-5-awesome-text-display-with-ggtext/
    # https://r-charts.com/ggplot2/axis/
    # https://www.statology.org/ggplot-transparent-background/



### Values Directly in Bars

    # https://stackoverflow.com/questions/24776200/ggplot-replace-count-with-percentage-in-geom-bar
        # I did not use but thought it was useful and should be saved



### Riffomonas Project Videos Useful for Stacked/Faceted Figures

    # Stylize facet labels: https://www.youtube.com/watch?v=v1hTB2b_YkE

    # Creating stacked bar chart: https://www.youtube.com/watch?v=NVym44SdcaE&t=5s

    # Improving appearance of stacked chart: https://www.youtube.com/watch?v=w4X3o6MQjVA
        # reorder legends
        # reorder variables in stacks

    # Manipulating Axes: https://www.youtube.com/watch?v=UVriK-W1DNg


###########################    Tutorial 1   ###################################


# This method would have worked. But I found a much better method. (scale_y_continuous(expand()))

# I DID NOT USE THIS METHOD

# There's a way to adjust the position of columns over the axis with either a (0, 1, 0.5)
    ## I just forget the actual argument!


# From https://stackoverflow.com/questions/45701456/how-to-control-space-between-stack-bars-in-ggplot2

# Trying to force space between facet columns by creating a blank column within each facet
# I may have to shift over the bars so that they aren't centered to hide the empty space on the right

test <- prey %>%             # create new dataframe based on 'pred' 
  
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

test$other.prey[test$other.prey == FALSE] = "Shrimp"
test$other.prey[test$other.prey == TRUE] = "other"


test$pizza <- test[['pizza']]*100    # format into percentage

# Creating a blank column for table formatting

names(test)

# apple <- data.frame(pred.name1 <- c("Atlantic cod", "Greenland halibut", "Redfish"),
#                    other.prey <- c("apple", "apple", "apple"))
# 
# 
# left_join(pandalus.percent, apple, by=c("pred.name" = "pred.name1"))


test <- test %>%
   ungroup(pred.name)

test <- test %>%
   add_row(pred.name = 'Atlantic cod', other.prey = 'apple')# %>%

## Test figure
  
ggplot(test, 
         aes(x = factor(other.prey, levels = c('Shrimp', 'other')), 
             y = pizza, fill = prey.name)) +
  
  theme_minimal() +
  
  geom_col(position = "stack",
           width = 0.8) +                                      # reduces column width
  
  facet_wrap(~ pred.name,
             ncol = 4,                                         # plots in 4 columns (so all appear in 1 row)
             strip.position = "bottom")  +                      # moves the facet strip to the bottom
  
  scale_x_discrete(expand = c(0.5, 0.5))#,
                   breaks = c("Shrimp", "other"))
  
  # scale_x_discrete(limits = c("apple", "other", "Shrimp"), 
  #                  breaks = c(NA, "other", "Shrimp"), 
  #                  labels = c("", "other", "Shrimp"),
  #                  drop = FALSE) +  

  labs(x = "Predator Species",
       y = "%W") +

  scale_fill_manual(values = c("other" = "grey",               # assigns colour to prey sp. categories
                               "Pandalus" = "#CCEDB1",
                               "borealis" = "#41B7C4",
                               "montagui" ="#FF9999")) +

  coord_cartesian(ylim = c(0, 100)) +

  scale_y_continuous(breaks = seq(0, 100, by = 25)) +

  theme(axis.text.x = #element_text(
    element_blank(),#,                           # removes x-axis labels
 #   vjust = -6),
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


##########################       Tutorial 2    #################################

# I did not use this method or explore it too much.
# I found this when trying to increase padding around the figure panels
  
# It adds a different amount of space on opposite sides of the axis.
  
# Code from https://stackoverflow.com/questions/36823526/add-different-amount-of-extra-space-on-both-sides-of-discrete-ggplot-x-axis


# code submitted for changes
set.seed(0)
L <- sapply(LETTERS, function(x) paste0(rep(x, 10), collapse=""))
x <- data.frame(label=L[1:24], 
                g2 = c("a", "b"),
                y = rnorm(24))
x$g2 <- as.factor(x$g2)
x$xpos2 <- as.numeric(x$g2) + .25

# two groups
ggplot(x, aes(x=g2, y=y)) + 
  geom_boxplot(width=.4) +
  geom_point(col="blue") +
  geom_text(aes(x=xpos2, label=label, hjust=0)) +
  # adjustments
  scale_x_discrete(expand=c(0.1,0),
                   breaks=c("a", "b"),
                   labels=c("a", "b"),
                   limits=c("a", "b", "c"), drop=FALSE)


################################################################################
#                 Figure Formatting - ggarrange package
################################################################################


### Intro

    # (intro) https://www.youtube.com/watch?v=pdBYAxE6xp4
        # labels = "AUTO" give uppercase panel lettering
        # labels = "auto" give lowercase panel lettering

    # (overview) https://www.youtube.com/watch?v=QLAJsHxY3PE
        # height and width of rows
        # how to combine odd number of figures across multiple rows or columns


### Common Legends

    # (common legends) https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
        # share legends across figures: common.legend = TRUE
        # place legend below figures: legend = "bottom" 


### Adding space between figures

    # (figure spacing) https://github.com/kassambara/ggpubr/issues/151
        # there is not a way to adjust space between figures in ggarrange itself
        # need to adjust theme(plot.margin = ) within figures instead

    # (figure spacing) https://stackoverflow.com/questions/64972212/how-to-add-a-border-around-a-chart-created-via-ggarrange-and-increase-space-betw
        # also used theme(plot.margin = )


### Formatting Labels (a,b,c etc)

    # (color, bold, size, text, font) https://stackoverflow.com/questions/65280972/font-size-of-labels-and-values-in-a-ggarranged-plot
        # Modify appearance of labels: font.label = list()
        # R ggarrange() help document provides available options
        # Modify text of labels: labels = c("xxx", "xxx")


### Width & Height of Figures

    # (size of figures by row/columns) https://www.youtube.com/watch?v=QLAJsHxY3PE
        # Modify width of rows/columns: widths = c(1,2)
        # Modify height of rows/columns: heights = c(2,1)


### Combine Odd Number of Figures Across Multiple Rows/Columns in Particular Order
    
    # https://www.youtube.com/watch?v=QLAJsHxY3PE
        # Around 4:50 minute mark
        # use ggarrange() within ggarrange() to specify which figures go where


################################################################################
#                       Unused Code - ggplot2
################################################################################


### Using coord_cartesian() instead of scale_y_continuous()

#scale_y_continuous() is not compatable with scale_fill_manual(breaks =)
    # I needed scale_fill_manual(breaks = ) so I had to remove scale_y_continuous

# originally I used both scale_y_continuous() and coord_cartesian()
    # scale_y_continuous() was not forcing a y-axis label at 100. I'm not sure why
    # coord_cartesian() would force a y-axis label at 100. But breaks aren't specified
    # Because this is a template, I wanted to retain scale_y_continuous() to specify breaks
    # I hope that the breaks will remain the same even by removing scale_y_continuous()

ggplot2(
  scale_y_continuous(breaks = seq(0, 100, by = 25))
  )



### Issue where Facet Wrap Titles (Strips) Were Not Directly Below x-axis

# I had removed the x-axis labels
# The Facet Strip text was not directly below the x-axis even though it was supposed to
# When I moved it up into the empty space, the facet strip text would disappear 
# I thought that maybe the x-axis labels retained the white background even when removed

# I tried using the below theme() arguments but they did not work because they were not the issue
    # The issue was that my Strip Labels were within the y-axis padding.
    # I removed this padding in scale_y_continuous(expand())

ggplot2(
  theme(axis.text.x = element_text(vjust = -10),      
        strip.text = element_text(vjust = 8),
            # I tried pulling the axis text below the strip text
        
        strip.text = element_text(vjust = -10),       # pulls panel titles to the top left
        
        plot.background = element_rect(fill='transparent', color=NA)))
          # make the plot background transparent. 


################################################################################
#                     Unused Code - ggarrange
################################################################################


ggarrange(df1, df2,
  nrow = 1           # have figures in one row
  labels = "auto",   # a/b/c/d etc panel labels in lower case
  labels = "AUTO",   # A/B/C/D etc panel labels in upper case
  vjust = 0.8))


### Add Boarders around Figure 6 if desired

# annotate allows me to draw various items onto figures
# annotate uses the spacial references of the figures themselves.
# I'm not sure how it would fully work with discrete data

annotate_figure(f6,
                'rect',
                xmin = 40,
                xmax = 47,
                ymin = 210,
                ymax = 225,
                col = 'black')


################################################################################
#                    OG Code that Dan Enright Helped Write
################################################################################


# Below is original code Dan coded 

# I have modified and condensed it using dyplr

# I am saving the orignal here in case something I did causes errors

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

# my addition afterwards to format figure data into percentage
pandalus.f6a$pizza <- pandalus.f6a[['pizza']]*100    # format into percentage



################################################################################
#                    Final Figure Formatting Options
################################################################################


# Below are the base code and different formating options for this figure. 

# Base Code

f6 <- ggarrange(f6a, f6b,             # combine figure f6a and f6b into one plot
                ncol = 2,                   # have figures on two columns (nrow = 1 should yield same result)
                labels = "auto",            # label figures in lowercase a, b
                common.legend = TRUE,       # both figures share a common legend
                legend = "bottom",           # place the shared legend on the bottom
                font.label = list(face = "plain")
) + 
theme(plot.margin = margin(1,1,1,1, "cm"))   # adjusts plot margins to define spacing between panels


####################    Option 01 - Outside Border  ############################


# this version does create an outside border
f6 +
  border()

# This also creates an outside border
f6 +
  theme(plot.background = element_rect(colour = "black",     # line color
                                       fill = NA,            # have no fill color
                                       linewidth = 1))       # width of border line


####################    Option 02 - Paneled Borders  ############################


# outside border per panel. Legend below without border

f6ab <- f6a +
  theme(plot.background = element_rect(colour = "black", fill = NA, linewidth = 1))

f6bb <- f6b +
  theme(plot.background = element_rect(colour = "black", fill = NA, linewidth = 1))

ggarrange(f6ab, f6bb,                   # combine figure f6a and f6b into one plot
          ncol = 2,                   # have figures on two columns (nrow = 1 should yield same result)
          labels = "auto",            # label figures in lowercase a, b
          common.legend = TRUE,       # both figures share a common legend
          legend = "bottom",           # place the shared legend on the bottom
          font.label = list(face = "plain")) 


#################    Option 03 - Panel & Legend Borders    #####################


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
          font.label = list(face = "plain")) 


######################    Option 04 - All Borders   ############################


# outside border per panel and outside figure border

ggarrange(f6ab, f6bb,                   # combine figure f6a and f6b into one plot
          ncol = 2,                   # have figures on two columns (nrow = 1 should yield same result)
          labels = "auto",            # label figures in lowercase a, b
          common.legend = TRUE,       # both figures share a common legend
          legend = "bottom",           # place the shared legend on the bottom
          font.label = list(face = "plain")
) +
  theme(plot.background = element_rect(colour = "black", fill = NA, linewidth = 1))
