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
    # Table Formatting - flextable & officer
    # Table Formatting - kable & kableExtra
    # Table Formatting - gt & gtExtra
        # subsection: tutorials
    # Unused Tables with kable() 
    # Unused Tables for gt()
    # Unused gt() Code


# I tried a few table packages while creating a Word compatable table with Markdown cross-references:

    # flextable() offered Word compatibility, formatting, and Markdown cross-references
        # this is the package I used

    # kable() offered word compatibility and Markdown references
        # no formatting!
        # kableExtra() provides formatting but isn't Word compatible. Only pdf/html
        # kableExtra() will make Markdown crash

    # gt() provides smooth formating. But only compatable with pdf/html
        # but it didn't keep all formatting even when exported to pdf.....

    # additional non-Word compatible options https://rfortherestofus.com/2019/11/how-to-make-beautiful-tables-in-r/


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
#                  Table Formatting - flextable & officer
################################################################################

# A note is that I found very little youtube or webpage tutorials on flextable.

# The Stacked Overflow pages just pointed to the excellent source book.

# The ONE youtube video I found was an hour long presentation by the creator



### Flextable works in word

    # (export to Word) https://stackoverflow.com/questions/73120996/how-to-export-kableextra-output-in-r-to-word

    # (gitHub Summary) https://davidgohel.github.io/flextable/index.html



### Flextable Sources

    # (flextable source book) https://ardata-fr.github.io/flextable-book/
        # I found everything in this book

    # (video overview) https://www.youtube.com/watch?v=-EuPFZCTnHE
        # Creator explaining package
        # I have not yet listened to the entirety (1.25 hrs)



### officer

    # (github Summary) https://davidgohel.github.io/officer/



# some formatting options in flextable() use the officer() package
    # As of July 23/2024 my completed first draft of Table 1 didn't use any

# I have barely looked into officer or the related officedown package

# It allows for improved manipulation of Word (.docx) and PowerPoint (.pptx) files in R

# One Stacked Overflow post said that if incorporated the larger heavy-lifting components
    # of officer to a large Markdown document, that Markdown knitting would slow or crash

# definitely an interesting package to look into at some point


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

library(gt)                      # Table formatting
library(gtExtras)                # Additional table formatting options


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
#                      Unused Tables with kable()
################################################################################


# used flextable() rather than kable()

# kable() creates tables that can be exported to Word and cross-referenced 
# It has terrible formatting options. Almost unusable without kableExtra()
# kableExtra() adds formatting but doesn't work with Word



### Load Table 1 dataframe

samp.table <- read.csv('data/processed/2019_T1_sampleLocation_final.csv')



### Create Table 1A

table01a <- samp.table %>%
  knitr::kable(                                              # creating kable() table
    align = 'c',                                             # center alignment
    col.names = c("Depth Range", "EAZ", "WAZ", "SFA 4"),     # defines column names
    caption = '*Number of stomach sampling locations within each depth stratum of the Eastern ASsessment Zone (EAZ), Western Assessment Zone (WAZ), and Shrimp Fishing Area 4 (SFA 4)*'
    # caption - will be pulled into the R Markdown output. Not included in code chunk {fig.cap} equivalent like figures
  )                                                          

table01a


################################################################################
#                      Unused Tables with gt()
################################################################################


# flextable() was used for Table 1 

# gt is my favourate package so far to create Tables. 
# if feels intuitive, with good online examples, is easily custamizable
# not all formatting from .jpeg holds in exporting to pdf or word
# But it doesn't work with Word for formatting or cross-referencing

# until I found flextable(), my plan was to: 
    # use kable() table in Markdown
    # export a gt() table .jpeg in /outputs
    # cut kable() table from Word and paste in gt() jpeg


######################    Table 1B - Pretty Version    ########################


# The below code creates a nicely formatted table.
# Table 1c (below) is my favorite but does not retain the spanner formatting upon export.
# The spanner is not really needed, so this version removes the spanner



### Load Table 1 dataframe

samp.table <- read.csv('data/processed/2019_T1_sampleLocation_final.csv')



### Create Table 1B

table01b <- samp.table %>%
  gt() %>%
  cols_label(depth ~ "Depth Stratum (m)",      # change column names
             SFA4 ~ "SFA 4")  %>%              
  
  opt_stylize(style = 1, color = "gray") %>%   # uses preset theme in the 'gray' color
  # I like elements of this theme (boarders and shading)
  # header text and fill color will be adjusted below
  
  cols_width(depth ~ px(160),                  # sets width of the 'depth' column to specified pixel number
             ends_with("Z") ~ px(130),         # sets width of columns ending in 'Z' to specified pixel number
             SFA4 ~ px(130)) %>%               # sets width of the 'SFA4' column to specified pixel number
  
  cols_align(align = c("center"),              # centers text
             columns = everything()) %>%       # alignment applies throughout table
  
  tab_style(                                   # forces styles onto cells
    style = list(                              # applies multiple styles
      cell_fill(color = "lightgray"),          # fill color to 'light gray', because the opt_stylize 'gray' looks black
      cell_text(weight = "bold",               # bold text
                color = "black")),             # make text black
    locations = list(cells_column_labels()))   # formatting applies to the column headers



### Exporting as jpeg

jpeg(filename="output/Table01_pretty.jpeg", width = 480, height = 480)
plot(table01b)
dev.off()

# replace jpeg() with png() if that becomes a desired format


######################    Table 1C - Ideal Version    ########################


# The below code creates my ideal table format & unused gt() arguments
# It only keeps the grey beside the spanner within R itself.



### Load Table 1 dataframe

samp.table <- read.csv('data/processed/2019_T1_sampleLocation_final.csv')



### Create Table 1C - full pretty formatting

table01c <- samp.table %>%
  gt() %>%
  cols_label(depth ~ "Depth Stratum (m)",
             SFA4 ~ "SFA 4")  %>%              # change column names
  
  tab_spanner(label = "Assessment Area",       # add a 'spanner' or sub-heading titled Assessment Area
              columns = EAZ:SFA4) %>%          # spanner is over columns EAZ through SFA4 (goes off column names from df itself - not what you renamed it)
  
  opt_stylize(style = 1, color = "gray") %>%   # uses preset theme in the 'gray' color
  # I like elements of this theme (boarders and shading)
  # header text and fill color will be adjusted below
  
  cols_width(depth ~ px(160),                  # sets width of the 'depth' column to specified pixel number
             ends_with("Z") ~ px(130),         # sets width of columns ending in 'Z' to specified pixel number
             SFA4 ~ px(130)) %>%               # sets width of the 'SFA4' column to specified pixel number
  
  cols_align(align = c("center"),              # centers text
             columns = everything()) %>%       # alignment applies throughout table
  
  tab_style(                                   # forces styles onto cells
    style = list(                              # applies multiple styles
      cell_fill(color = "lightgray"),          # fill color to 'light gray', because the opt_stylize 'gray' looks black
      cell_text(weight = "bold",               # bold text
                color = "black")),             # make text black
    locations = list(cells_column_spanners(),  #   # formatting applies to the column spanner
                     cells_column_labels()))   # formatting applies to the column headers

table01c


################################################################################
#                            Unused gt() Code
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
  

################################################################################
#                      Unused flextable-officer Code
################################################################################


library(officer)    # offers formatting options for Markdown to Word
                    # comments online that when used a lot for long reports, can slow or crash Markdown exports
  
  

# define table formatting defaults for all flextable() tables within an entire document
    # if used, this should be included in Markdown 'setup' chunk when loading libraries etc
set_flextable_defaults(border.color = "darkgray",    # defines color for all boarders
                       border.width = 1.5,           # defines width for all boarders
                       border.style = "solid")       # defines line type for all boarders

# remove defined formatting (from set_flextable_defaults)
init_flextable_defaults()


# create border rules that are required for customizing any border
    # officer package need for belwo function
big_border <- fp_border(color = "darkgray", style = "solid" , width = 1.5)


flextable(samp.table) %>%                          # create flextable of specified dataframe
  
  hline_top(part = "all",                          # horizontal header lines 
            border = big_border) %>%               # style defined in 'big_border' vector from earlier fp_border()   
  
  add_header_row(                                  # adding a second header row (spanner in gt)
    values = c("","Assessment Area"),              # titles for header row  
                             colwidths = c(1,3))   # number of columns it applies to.
                                                   # HAS TO = total number of columns


# I am trying to add striped colors to the table body based on odd/even rows
    # so far flextable hasn't liked any provided equations
    # below is one attempt

# row_odd <- seq_len(nrow(samp.table)) %% 2