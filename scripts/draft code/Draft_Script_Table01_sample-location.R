################################################################################
################################################################################
#                   Table 1 Sample Locations - LEARNING CODE
################################################################################
################################################################################


# Author: Krista Kenyon
# Date: July 19/2024

# Document Purpose: Saving sources and example code I encountered/used while 
    # creating Table 1 in case it's useful in the future.

# Document 'Sections' Include:
    # Creating figure dataset
    # Piping Data into R Markdown
    # Table Formatting - kable & kableExtras
    # Table Formatting - gt & gtExtra
        # subsection: tutorials
    # Unused but Interesting code


################################################################################
#                        Creating figure dataset
################################################################################


# I used code that I learned while making Fig 3-5 to format the dataset



### Export jpeg

    # (export jpeg code) https://www.datamentor.io/r-programming/saving-plot


################################################################################
#                        Piping Data into R Markdown
################################################################################


# source: https://bookdown.org/yihui/rmarkdown-cookbook/option-code.html
    # two options are provided 
    # forces external R code into the Markdown code chunk
    # keeps the Markdown document clean
    # only need to maintain one chunk of code, rather than two
    # cross-referencing still works



### {file=}

# Bookdown document says this is the best way to pipe in code

# generates tables and exports data as if writing into the R Markdown code chunk

# example code
```{r, file='your-script.R'}
```



### 'source ='

# tables called out in source code will not be generated within the R Markdown
# exports within source code will not happen either.

# So if I want to end up using this method I will need to include code within the Markdown chunk that:
    # reveals tables/figures
    # exports any desired codes

# example code
```{r test, echo=FALSE}
source("C:scripts/Script_Table01_Sample-Locations.R")  # pathway within R Project works. Will be transferable
table1
```


################################################################################
#                  Table Formatting - kable & kableExtra
################################################################################


# kable() can generate tables for Word outputs.

# kableExtra() provides formatting options for kable() tables. 
    # ONLY WORKS for pdf or html outputs. NOT WORD 
    # same as flextable() - a different table creation package

# R Markdown will stop knitting word output if kableExtra() code is used 
    


### kable

    # (chunk requirements) https://www.youtube.com/watch?v=DePaF-IkF94
        # Tutorial 1
        # kable requires {r, results='asis'} in Markdown code chunk



### kableExtra

    # (overview) https://bookdown.org/yihui/rmarkdown-cookbook/kableextra.html
        # specifies allowed outputs
        # overview of most common formatting options

    # (only pdf & html) https://stackoverflow.com/questions/47704329/how-to-format-kable-table-when-knit-from-rmd-to-word-with-bookdown
        # likely has other details on kableExtra
        # This is where I learned kableExtras only works on pdf and html outputs

    # (basic formatting) https://www.youtube.com/watch?v=DePaF-IkF94
        # Tutorial 1
        # formatting such as table striping and width

    # (horizontal lines) https://stackoverflow.com/questions/49015578/space-after-every-five-rows-in-kable-output-with-booktabs-option-in-r-markdown
        # removing or adjsuting default white line spaces after every 5 rows in table
        # adding horizontal lines into tables



### flextable

     (overview) https://davidgohel.github.io/flextable/index.html
        # there may be a package that lets me format tables with flextable, and maybe kableExtra for Word
        # officer() and officedown() packages. I NEED TO LOOK INTO THESE



### officer

# LOOK INTO THIS

https://davidgohel.github.io/officer/


###########################    Tutorial 1   ###################################


# From  https://www.youtube.com/watch?v=DePaF-IkF94

# formatting tables with kable() and kableExtra()

# code below
```
{r test1, include=FALSE, results='asis', echo=FALSE}

# results='asis' required for kable. It means that Markdown will pump the results into kable as plain text

### Load Table 1 dataframe
library(flextable)
library(kableExtra)
library(tidyverse)

samp.table <- read.csv('data/processed/2019_T1_sampleLocation_final.csv')

df %>%
  knitr::kable(
    digits = 2,                                              # will round the numbers to 2 decimal places
    caption = 'Table 1: Summary Statistics'                 # table caption
  ) %>%
  kableExtra::kable_styling(bootstrap_options = "striped",  # colored stripes every other row
                            full_width=FALSE)               # table doesn't go the full width
```


################################################################################
#                         Table Formatting - gt & gtExtra
################################################################################


### Trying to Export into Word

    # (gt to Word) https://stackoverflow.com/questions/69400178/how-to-save-a-gt-table-to-either-an-excel-or-word-file
        # Tutorial 2
        # saving to rtf format that is Word compatable
        # still won't keep my desired heading colors beside a spanner



### Making Tables Pretty

    # (overview of gt) https://www.youtube.com/watch?v=jptX745mp7M
        # Tutorial 3
        # Really good overview about table manipulations and formatting in gt package

    # (overview of gtExtra) https://www.youtube.com/watch?v=AJ0_PCIVXD4
        # Tutorial 4
        # Insert distribution graphs within the table
        # a bit on themes

    # (gt OPTIONS OVERVIEW) https://gt.rstudio.com/reference/
        # GREAT RESOURCE
        # most arguments for table customization were quickly available here
        # most below references are branches from this

    # (table parts) https://gt.rstudio.com/articles/gt.html
        # terms for the different table sections

    # (defining column widths) https://gt.rstudio.com/reference/cols_width.html
        # Tutorial 5
        # available arguments

    # (alignment within columns) https://gt.rstudio.com/reference/cols_align.html
        # available arguments. I centered text

    # (colors in present themes) https://gt.rstudio.com/reference/opt_stylize.html
        # available arguments

    # (cell boarders) https://gt.rstudio.com/reference/cell_borders.html
        # available arguments

    # (formatting multiple cells) https://gt.rstudio.com/reference/tab_style.html
        # available arguments



### Changing colors for rows

    # (codes for colours) https://r-charts.com/colors/
        # GREAT RESOURCE
        # commonly used colors & many color gradiants

# I haven't looked at these websites but they are stored here for future reference

    # https://gt.rstudio.com/reference/tab_style.html

    # https://jthomasmock.github.io/gtExtras/reference/gt_highlight_rows.html

    # https://stackoverflow.com/questions/75958889/how-can-i-color-every-alternate-table-row-in-gt-table-in-r


###########################    Tutorial 2   ###################################


# From Stack Overflow https://stackoverflow.com/questions/69400178/how-to-save-a-gt-table-to-either-an-excel-or-word-file

# Trying to find a way to use my gt() tables in R Markdown Word output
# Not successful

# saving to rtf format
# still won't keep my desired heading colors beside a spanner

#install.packages("gtsummary")
library(gtsummary)

table01 %>%
  # gtsummary::as_gt() %>%
  gt::gtsave(., filename = "output/Table1_pretty.rtf")


###########################    Tutorial 3   ###################################


# From Equitable Equations channel https://www.youtube.com/watch?v=jptX745mp7M

# great overview of gt package


library(gt)
library(tidyverse)
#install.packages('modeldata')
library(modeldata)

data(scat)

# Data preprocessing

scat_table1 <- scat %>% 
  select(Species, 
         Site, 
         Length,
         Diameter,
         Mass,
         d13C,
         d15N) %>% 
  group_by(Species) %>% 
  summarize(across(where(is.numeric), 
                   \(x) round(mean(x, na.rm = TRUE), 1)),
            count = n())

scat_table2 <- scat %>% 
  select(Species, 
         Location,
         Site, 
         Length,
         Diameter,
         Mass,
         d13C,
         d15N) %>% 
  group_by(Species, Site) %>% 
  summarize(across(where(is.numeric), 
                   \(x) round(mean(x, na.rm = TRUE), 1)),
            Count = n())


## A great table

# basic
# his had shading in 1 & 3 row - mine doesn't. 
# basic horizontal lines
st <- scat_table1 %>%
  gt()


# Rename Columns
st <- st %>%
  cols_label(count ~ "Count",
             Length ~ "length")

# when adding something to the table, the notation is 'tab_xxxx'


# Adding a Title (known as Table Header)
st <- st %>%
  tab_header(title = "California poopies",
             subtitle = "Morphometric data on scat") 
# two elements to a header: the title and subtitle


# Add some source data at bottom of table
# gt allows you to use markdown notation when writing text (headings, column titles, footnotes, etc)
# this is the md() within the tab_source_note()
# can also use html() to use html text formating
st <- st %>%
  tab_source_note(md("**Source**: Reid, R. E. B. (2015). A morphometric modeling approach to distinguishing among bobcat, coyote and gray fox scats. *Wildlife Biology*, 21(5), 254-262"))
st


# Add a spanner to the table
# spanners = horizontal lines that only go through part of the table
# need to specify 2 things: columns it will cover, and what the labels will be
st <- st %>%
  tab_spanner(label = "Physical averages",
              columns = Length:Mass) 
st

# I can use tidyr select syntext when trying to specify which rows/columns something applies to
# ex: above with tab_spanner(columns = Length:Mass)
# ex: specify columns using numbers, names, using : for a span, helper verbs (includes, starts with)


# offsetting the first column title (as if it's a row name)
# normal pipe with 'st' won't work. I'm guessing it's because it's changing gt() itself
# removes the name of the column, and adds a vertical line between the 1 and 2 column
scat_table1 %>%
  gt(rowname_col = "Species")  # formats Species column as row names


# to add the first column title back in while keeping the 'row title' appearance
scat_table1 %>%
  gt(rowname_col = "Species") %>%
  tab_stubhead(label = "Species")    # adds the row name back and left aligns it


# tab_footnote -> adds a footnote. Look at helpfile for example.
# need to say what the footnote will be, and where it will go (location)


# fmt_xxxx = format commands for various metrics
# this lets you manually format things
# eg: format_percent = # should be %



## A stylized version

st1 <- scat_table1 %>% 
  gt() %>% 
  cols_label(count ~ "Count") %>% 
  tab_header(title = "California poopies",
             subtitle = "Morphometric data on scat") %>% 
  tab_source_note(md("**Source**: Reid, R. E. B. (2015). A morphometric modeling approach to distinguishing among bobcat, coyote and gray fox scats. *Wildlife Biology*, 21(5), 254-262")) %>% 
  tab_spanner(label = "Physical averages",
              columns = Length:Mass) 

# changes style guides
# style 1 is the default
# limited in the color palette - only has 7 colours
st <- st %>%
  opt_stylize(style = 2, color = "red")
st


# I can have more fine-grained controle than pre-created styles.
# opt_xxx packages



# Grouped data
scat_table2 # has 3 groups within species

# basic table with grouped data
# values in grouped variables each have a row of their own
# other variables below that
scat_table2 %>%
  gt()

# If we ungroup first
# it will strip out the group parameter, and there will be unique row for each species observations
scat_table2 %>% 
  ungroup() %>%
  gt()

# we can also let gt() know there are groups on the fly
# we don't have to put a groupby() command if we want a grouped table
scat_table2 %>% 
  ungroup() %>%
  gt(groupname_col = "Species") # identifies which column is grouped
# gt() will always look for the name in the actual dataset, not whatever I label within the table


###########################    Tutorial 4   ###################################


# From R Programming 101 channel https://www.youtube.com/watch?v=AJ0_PCIVXD4

# how to insert distribution graphics within tables
# A bit on themes

library(gtExtras)
library(tidyverse)

# Create a summary of your data

iris %>%
  gt_plt_summary()
    # he likes that it does all the thinking for us. And that it includes missing values

# Insert icons and graphics into tables
view(mtcars) # pulls up the dataset into an R tab

mtcars %>%
  group_by(cyl) %>%   # rows will be the number of cylendars
  
  # summarize will create a collumn called Median, Mean, and Distribution
  summarize(Median = round(median(mpg), 1),
              # rounded the median of mpg to 1 decimal place
            Mean = round(mean(mpg),1),
            Distribution = list(mpg)) %>%
              # list of mpg (so multiple values)
 # view()
  gt() %>% 
    # above data is piped into gt()
  gt_plt_dist(Distribution) %>% 
    # creates a visual distribution of the data within the Distribution variable
    # because it is a list of numbers, it can create the distribution 
  gt_theme_guardian() %>%
    # this is the pre-set look that the table will have
  tab_header(title = "Miles per gallon statstics")
    # table header


###########################    Tutorial 5   ###################################


# From https://gt.rstudio.com/reference/cols_width.html

# arguments for define column widths

df %>%
gt() %>%
cols_width(depth ~ px(130),                # specific column name (within df vs renamed in column)
           ends_with("Z") ~ px(130),       # Any column that ends with 'Z'
           starts_with("S") ~ px(130))     # Any column that starts with 'S'


df %>%
  gt() %>%
cols_width(everything() ~ px(60))          # All columns set to 50 pixels


################################################################################
#                      Unused but Interesting gt() Code
################################################################################


# gt() code that I found but did not use

df %>%
  gt() %>%
  
  # define individual column widths
  cols_width(depth ~ px(130),                    # specific column name (within df vs renamed in column)
             ends_with("Z") ~ px(130),           # Any column that ends with 'Z'
             starts_with("S") ~ px(130))         # Any column that starts with 'S'

  cols_width(everything() ~ px(60))              # All columns set to 50 pixels
  
  # adjusting cell boarders
  tab_style(style = list(                        # multiple styles will be specified
    cell_borders(                                # formatting will be for cell boarders
      sides = c("left", "right"),                # boarders on the left and right side
      color = "black"),                          # boarders are black
      weight = px(30)),                          # boarder thickness
    locations = list(cells_body(),               # styles will apply to the table body
                     cells_column_labels(),      # styles will apply to the table header
                     cells_column_spanners()))   # styles will apply to the table header spanner
