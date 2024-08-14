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
        # subsection: tutorials
    # Table Code
        # subsection: tutorials
    # Unused but Interesting code......


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


### Coding Category

    # (multiply specific column) https://stackoverflow.com/questions/57508123/how-to-multiply-specific-columns-of-a-dataframe#:~:text=You%20need%20to%20use%20double%20square%20brackets%20to,shorten%20your%20code%20a%20bit%3A%20quality%20%2A%3D%20100
        # need [[]] to multiply a specific column

### Spacing around columns without impacting facet spacing

    # https://stackoverflow.com/questions/36823526/add-different-amount-of-extra-space-on-both-sides-of-discrete-ggplot-x-axis


################################################################################
#                        Table Formatting - gt package
################################################################################


### Facet Panel Formatting

    # (panel position) https://www.statology.org/ggplot-facet-axis-labels/
        # how to move panel positions
        # how to rename panels

    # (axis overview) https://r-charts.com/ggplot2/axis/
        # GREAT overview!
        # anything plot axis related is here!


### Transparency in labels

    # https://stackoverflow.com/questions/54241972/transparency-in-x-and-y-labels-ggplot2-r
    # https://genchanghsu.github.io/ggGallery/posts/2021-07-10-post-5-awesome-text-display-with-ggtext/
    # https://r-charts.com/ggplot2/axis/
    # https://www.statology.org/ggplot-transparent-background/




###########################    Tutorial 1   ###################################

# There's a way to adjsut the position of columns over the axis with either a (0, 1, 0.5)
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
         aes(x = other.prey, y = pizza, fill = prey.name)) +
  
  theme_minimal() +
  
  geom_col(position = "stack",
           width = 0.8) +                                      # reduces column width
  
  facet_wrap(~ pred.name,
             ncol = 4,                                         # plots in 4 columns (so all appear in 1 row)
             strip.position = "bottom")  +                      # moves the facet strip to the bottom
  scale_x_discrete(limits = c("apple", "other", "Shrimp"), 
                   breaks = c(NA, "other", "Shrimp"), 
                   labels = c("", "other", "Shrimp"),
                   drop = FALSE) +  

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


############### Tutorial ##########################

# from https://stackoverflow.com/questions/36823526/add-different-amount-of-extra-space-on-both-sides-of-discrete-ggplot-x-axis

# trying to increase padding around figure

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
#                                 Unused Code
################################################################################


