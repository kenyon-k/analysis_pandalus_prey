################################################################################
################################################################################
#                   Figure 8 - LEARNING CODE
################################################################################
################################################################################


# Author: Krista Kenyon
# Date: Aug 19/2024

# Document Purpose: Saving sources and example code I encountered/used while 
    # creating Figure 8 in case it's useful in the future.

# Document 'Sections' Include:
    # Creating figure dataset
        # subsection: tutorials
    # Unused but Interesting code......


################################################################################
#                        Creating figure dataset
################################################################################


### Coding Category

    # (highlight details) website
        # Tutorial ?
        # What did I learn or use from it?


################################################################################
#                        Figure Formatting - ggplot 2 package
################################################################################


### Coding Category

    # (highlight details) website
        # Tutorial ?
        # What did I learn or use from it?


###########################    Tutorial ?   ###################################


# From ............... channel website

# brief description

################################################################################
#                                 Unused Code
################################################################################



################################################################################
#                    Final Figure Formatting Options
################################################################################


# Below are the base code and different formating options for this figure. 

# Base Code

f8 <- ggarrange(f8a, f8b,                               # combine figure f6a and f6b into one plot
                ncol = 2,                                     # have figures on two columns (nrow = 1 should yield same result)
                labels = "auto",                              # label figures in lowercase a, b
                common.legend = TRUE,                         # both figures share a common legend
                legend = "bottom",                            # place the shared legend on the bottom
                font.label = list(face = "plain")) +
  
  theme(plot.margin = margin(1,1,1,1, "cm"))


####################    Option 01 - Outside Border  ############################


# this version does create an outside border
f8 +
  border()

# This also creates an outside border
f8 +
  theme(plot.background = element_rect(colour = "black",     # line color
                                       fill = NA,            # have no fill color
                                       linewidth = 1))       # width of border line


####################    Option 02 - Paneled Borders  ############################


# outside border per panel. Legend below without border

f8ab <- f8a +
  theme(plot.background = element_rect(colour = "black", fill = NA, linewidth = 1))

f8bb <- f8b +
  theme(plot.background = element_rect(colour = "black", fill = NA, linewidth = 1))

ggarrange(f8ab, f8bb,                   # combine figure f6a and f6b into one plot
          ncol = 2,                   # have figures on two columns (nrow = 1 should yield same result)
          labels = "auto",            # label figures in lowercase a, b
          common.legend = TRUE,       # both figures share a common legend
          legend = "bottom",           # place the shared legend on the bottom
          font.label = list(face = "plain")) 


#################    Option 03 - Panel & Legend Borders    #####################


# outside border per panel with legend border

f8abl <- f8ab +
  theme(legend.background = element_rect(colour = 1))

f8bbl <- f8bb +
  theme(legend.background = element_rect(colour = 1))

ggarrange(f8abl, f8bbl,                   # combine figure f6a and f6b into one plot
          ncol = 2,                   # have figures on two columns (nrow = 1 should yield same result)
          labels = "auto",            # label figures in lowercase a, b
          common.legend = TRUE,       # both figures share a common legend
          legend = "bottom",           # place the shared legend on the bottom
          font.label = list(face = "plain")) 


######################    Option 04 - All Borders   ############################


# outside border per panel and outside figure border

ggarrange(f8ab, f8bb,                   # combine figure f6a and f6b into one plot
          ncol = 2,                   # have figures on two columns (nrow = 1 should yield same result)
          labels = "auto",            # label figures in lowercase a, b
          common.legend = TRUE,       # both figures share a common legend
          legend = "bottom",           # place the shared legend on the bottom
          font.label = list(face = "plain")
) +
  theme(plot.background = element_rect(colour = "black", fill = NA, linewidth = 1))

