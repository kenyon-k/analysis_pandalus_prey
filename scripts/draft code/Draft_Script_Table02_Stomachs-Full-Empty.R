################################################################################
################################################################################
#                   Table 1 Sample Locations - LEARNING CODE
################################################################################
################################################################################


# Author: Krista Kenyon
# Date: July 23/2024

# Document Purpose: Saving sources and example code I encountered/used while 
    # creating Table 1 in case it's useful in the future.

# Document 'Sections' Include:
    # Creating figure dataset
        # subsection: tutorials
    # Table Formatting - flextable
    # Unused Code - Footnotes
    # Unused Code - Add Length Range 0-5
    # Unused Code - Renaming Table Columns
    # Final Formatting Options
        # subsection: options


################################################################################
#                        Creating figure dataset
################################################################################


# In general used the dyplr cheat sheet https://4.files.edl.io/b9e2/07/12/19/142839-a23788fb-1d3a-4665-9dc4-33bfd442c296.pdf



### Subset dataframe by column using dyplr


    # (select multiple columns) https://www.youtube.com/watch?v=kj8u2aDO3Go
        # Riffomonas Project
        # overview of select()

    

### Create New Column with Values Based on Other Columns

# I wanted to create 'Full' and 'Empty' stomach columns whose values were calculated 
# based on Prey_OS_ID values


    #(new columns) https://www.youtube.com/watch?v=RFuJ6mgu-Ug
        # adding new columns where values are calculated from other table columns
        # mutate()


    #(ifelse) https://www.youtube.com/watch?v=ED34ZhpkoPk
        # useful overview of ifelse()


    #(ifelse) https://www.youtube.com/watch?v=zose3lQAN7o
        # second overview of ifelse()



### Summarising Grouped Data

    #(summarise grouped data) https://www.youtube.com/watch?v=XoApM8QrPl8
        # very clean overview of topic
        # group_by() with summarise()


    #(summarising multiple groups) https://www.youtube.com/watch?v=W7sjVML_yXQ
        # creating multiple groups then summarizing


    # (pivot wider by group) https://www.youtube.com/watch?v=YpAdZ4079qs
        # reformat table by columns/groups
        # created final table structure
        # pivot_wider()



### Moving columns

    # (moving columns) https://www.statology.org/dplyr-relocate/#:~:text=You%20can%20use%20the%20relocate%20%28%29%20function%20from,change%20the%20column%20positions%20in%20a%20data%20frame.
        # relocate()
        # used in addition to dypr cheat sheet for helper 'contains'


### Re-Ordering Rows by Fish Size

    # (re-order by size) https://dplyr.tidyverse.org/reference/arrange.html#:~:text=arrange%28%29%20orders%20the%20rows%20of%20a%20data%20frame,once%20per%20data%20frame%2C%20not%20once%20per%20group.
        # arrange() default is smallest to  largest.


    # (add leading zeros) https://www.statology.org/str_pad-in-r/
        # str_pad()
        # used in addition to strigr cheat sheet for regex (regular expressions)



### Replace 0 and NA values with '-' 

# nothing I tried to select and replace across the entire dataframe within dyplr() worked
# I kept getting errors, or having issues I couldn't solve
# It would work if I replaced for 1 column, but I could not do that across the dataset
# Finally I found a solution of selecting all columns in base R before replacing

    # (selecting & replacing across dataframe) https://www.youtube.com/watch?v=xe1bX-Nfxj0
        # code from video located https://www.analyticohub.com/2020/03/21/2020-03-21-replace-strings-in-r-using-str-replace/
        # uses dyplr-stringr to replace characters in one column
        # uses base r-stringr to replace characters across columns
        # LIFE SAVER. I was SO frustrated


# code I tried but couldn't get to work with my dataframe

    # (basic) https://stackoverflow.com/questions/19503266/replace-all-particular-values-in-a-data-frame
        # tutorial 1
        # could not solve error: 'can't convert 'character' to 'double' 


    # (mutate-across) https://stackoverflow.com/questions/75462299/error-in-na-if-cant-convert-y-character-to-match-type-of-x-double
        # this looked promising
        # produced errors I couldn't solve
        # in my experimentation I didn't keep any tutorial code


    # (old code style) https://stackoverflow.com/questions/29271549/replace-all-occurrences-of-a-string-in-a-data-frame
        # tutorial 2
        # funs() has been replaced
        # could not make the replacement functions work


    # (select-contains) https://stackoverflow.com/questions/29018292/select-columns-based-on-multiple-strings-with-dplyr-contains
        # tutorial 3
        # solved the grouping error
        # couldn't solve the coercing error. 
        # maybe it's a structure issue? Num and Int vs Chr?


    # (tutorial 4)
        # other code I experimented with but could not solve the errors
        # sources either not saved, or are from my brain experiments



### Regular Expressions

    # (cheat sheet) https://rstudio.github.io/cheatsheets/strings.pdf
        # stringr cheat sheet
        # includes regular expression cheat sheet


    # (Only select single digit) https://stackoverflow.com/questions/15099150/regex-find-one-digit-number
        # great discussion on options
        # works but didn't end up using: (?<!\\d)\\d[-]
            # (..) in code checks that nothing is in front of the 1 digit followed by the '-'


    # (overview) https://www.datacamp.com/tutorial/regex-r-regular-expressions-guide
        # I used for specifying number of 0s I wanted to target 0{1,} = at least 1 in a row


###########################    Tutorial 1   ###################################


# From https://stackoverflow.com/questions/19503266/replace-all-particular-values-in-a-data-frame

# trying to select and replace across entire dataframe
# basic r code but could not solve error

s1 <- stomach.ratio %>%
  replace(is.na(.), 0) %>%

s1[s1 == 0] <- "-"

# error: ! Can't convert <character> to <double>.

# couldn't figure it out

# other basic code I tried

s2[is.na(stomach.ratio)] <- "-"

# exact same error

###########################    Tutorial 2   ###################################


# From https://stackoverflow.com/questions/29271549/replace-all-occurrences-of-a-string-in-a-data-frame

# trying to select and replace across entire dataframe
# funs() has been replaced
# could not make the replacement functions work


stomach.ratio[stomach.ratio=='0'] <- NA

s3 <- stomach.ratio %>%
  replace(is.na(.), 0)  %>%
  mutate_all(funs(str_replace(.,
                              "(?<!\\S)0(?!\\S)",
                              "-"))) 


###########################    Tutorial 3   ###################################

# From https://stackoverflow.com/questions/29018292/select-columns-based-on-multiple-strings-with-dplyr-contains

# trying to select and replace across entire dataframe


# attempt 1 based on code

stomach.ratio %>%
  mutate(
    select(contains(c('Full', 'Empty', 'Total'))) %>%
      replace(is.na(.), "-"))

# error regarding length.range being grouped

# attempt 2 - ungroup the code

stomach.ratio %>%
  replace(is.na(.), 0)  %>%
  ungroup(length.range)  %>%
  select(contains(c('Full', 'Empty', 'Total')))  %>%
  str_replace(.,
              "(?<!\\S)0(?!\\S)",
              "[-]") 

# error: In stri_replace_first_regex(string, pattern, fix_replacement(replacement),  :argument is not an atomic vector; coercing

# couldn't solve this issue

# at some point in my experimentation I switched 'starts_with' from 'contains'
    # I think this was before I realized the 'grouping' error

stomach.ratio %>%
  replace(is.na(.), 0)  %>%
  ungroup(length.range)  %>%
  select(starts_with(c("Full", "Empty", "Total")))  %>%
  str_replace(.,
              "(?<!\\S)0(?!\\S)",
              "[-]") 

# This produces the same coercing error that I couldn't solve


###########################    Tutorial 4   ###################################


# Sources either not saved or are from my brain experiments

# trying to select and replace across entire dataframe

# attempting to select multiple rows with dyplr

s4 <- stomach.ratio %>%
  replace(is.na(.), 0)  %>%
  mutate(select(contains(c('Full', 'Empty', 'Total')))) %>%
  str_replace(`Full_Atlantic cod`,
              "(?<!\\S)0(?!\\S)",
              "-")

# Error: unused argument ("-")

# could not solve this. Tried [-] and other things



# confirming it would work with selecting 1 row
stomach.ratio[stomach.ratio=='0'] <- NA

s5 <- stomach.ratio %>%
  replace(is.na(.), 0)  %>%
  mutate(`Full_Atlantic cod` = str_replace(`Full_Atlantic cod`,
                                           "(?<!\\S)0(?!\\S)",
                                           "-"))

# selecting one column works


################################################################################
#                        Table Formatting - flextable
################################################################################


# Captions and Footnotes in Markdown are frustrating!

# Markdown ONLY liked either when they were PIPED INTO Markdown.
    # NOT when they were coded within Markdown itself. Knitting execution halted

# Footnotes
    # Markdown did not like footnote(). I could not solve the errors
    # footnote() links auto #s from specified table cells to footnotes
    # I had to use add_footer_lines()
        # it does not have auto numbering.
        # I formatted the footnote with as_paragraph() to insert superscript numbers
        # This means I have to insert superscript numbers into the table.
        # In this case, that was in the caption. Which used same method.
        # Errors when format multiple footnotes in one code line with as_paragraph. Fine when as_paragraph not included
        # This error solved giving each footnote their own add_footer_lines() code
        


### Adding Footnote

    # (overview options) https://ardata-fr.github.io/flextable-book/header-and-footer.html
        # Section 6.3
        # ended up using add_footer_lines()


    # (footnote function) https://ardata-fr.github.io/flextable-book/header-and-footer.html
        # Section 6.4 
        # footnote()
        # Markdown HATED this function. Would stop knitting.
        # I couldn't solve this issue


    # (multiple columns together) https://www.ardata.fr/en/flextable-gallery/2022-06-23-separate-headers/
        # Applying footnote to multiple columns
        # also overview of creating table
        # has spanner lines over just spanner.....


    # (add_footer_lines) https://davidgohel.github.io/flextable/reference/add_footer_lines.html
        # add_footer_lines()
        # Markdown likes this method
        # HOWEVER no internal #ing of footnote and corresponding table section
        # I figured out a work-around. See below


    # (formatting footnotes) https://ardata-fr.github.io/flextable-book/captions-and-cross-references.html
        # Section 12.3 'Formatting captions'
        # enter captions into Markdown {} itself
        # manual formatting caption and footnotes
        # used this as workaround to add #s to add_footer_line Markdown hates footnote()
        # manual numbering only works for template because locations will be consistent



### Adjusting Column Width

    # (multiple columns) https://stackoverflow.com/questions/68761680/flextable-r-how-to-keep-columns-width-after-adding-a-header
        # comments showed defining widths for multiple columns


################################################################################
#                          Unused Code - Footnotes
################################################################################


# I first tried the footnote() argument.
# It works in R but not in the Markdown. 
    # Markdown would stop knitting. I could not solve the errors

# Below is the code that worked in R.
    # puts the reference symbol on any column that contains the word 'Full'
    # I had two footnote arguments to create 2 footnotes.

footnote(i = 2,                                                     # second row
         j = grep("Full", colnames(stomach.ratio), value = TRUE),   # columns in df containing "Full"
         part = "header",                                           # apply symbol to header
         ref_symbols = "1",                                         # specify reference symbol
         value = as_paragraph("A full stomach is any stomach that was not empty and contained prey items other than only parasites and/or only ..."))

  footnote(i = 2,
           j = grep("Empty", colnames(stomach.ratio), value = TRUE),
           part = "header",
           ref_symbols = "2",
           value = as_paragraph("Total length (cm) was used to measure all predators."))

  
# Experiment 1 - trying to fix error
    # thought maybe it was the 'fancy' column reference.
    # what if it was just a regular column?
    # didn't solve the error

footnote(i = 2,                                                     # second row
         j = 3,   # columns in df containing "Full"
         part = "body",                                           # apply symbol to header
         ref_symbols = "1",                                         # specify reference symbol
         value = as_paragraph(
         c("A full stomach is any stomach that was not empty and contained prey items other than only parasites and/or only mucous."))) # %>%                                                        # footnote text


# Other foot note arguments include:
  # add_footer
  # add_footer_row
  # add_footer_lines


# Testing out add_footer_lines
    # captions are compatible with R Markdown
    # I could not combine 2 references in 'as_paragraph' in one argument
        # It works if not 'as_paragraph'
        # I tried 'list(as_paragraph)' and 'c(as_paragraph)' but this didn't work
    # I ended up creating two arguments for the 2 references

add_footer_lines(c(
  as_paragraph(as_sup("1"),
               ", A full stomach is any stomach that was not empty and contained prey items other than only parasites and/or only mucous."),
  as_paragraph(as_sup("2"),
               " Total length (cm) was used to measure all predators.")
))


################################################################################
#                 Unused Code - Add Length Range 0-5
################################################################################


# The 2018 document adds a line for lengths 0-5 despite us not sampling it
# I think it gives the reader the impression that we did sample those lengths

# below is the code I was TESTING (didn't work yet) to try and add that length range into the table
# it would need to go before the df is sorted by length.range

# This is a template, and future data may have fish sampled within a 0-5 category.
# Therefor my aim was to write an 'if' or 'ifelse' statement. Either:
    # If there is no 0-5 category, add one. Or
    # If there is 0-5, do not include it, and if there is not add empty 0-5 category

# of note - Prey_OS_ID 9998 is the code for 'Empty' stomach


# Addempted code that needs more work


# Successful ifelse earlier within Table 2 formatting 
    # used as 'template'

mutate(Full = ifelse(Prey_OS_ID == 9998,     # create new column 'Full' with values based on logical check
                     0,                      # value if logical check is TRUE
                     1)) #%>%                 # value if logical check is FALSE


# attempt A 
    # R didn't like my ELSE statement. I didn't know how to fix that.
ifelse(length.range != "0-5",                 # condition statement: length.range does not have "0-5"
       add_row(length.range = '0-5'))         # condition = TRUE, add a row where length.range = "0-5" 
0)) %>%                                       # Not sure how to propperly add, condition=FALSE, do nothing

  


# second version
    # thoughts were maybe I needed to ungroup them
    # maybe if I left the ELSE statement empty it would do nothing
    # R did not like those ideas

t4 <- stomach.ratio %>%
  ungroup(length.range) %>%
  ifelse(length.range != "0-5",  
         add_row(length.range = '0-5'))
)


# third version
    # trying an if statement vs ifelse
    # couldn't get this to work
if(length.range != '0-5'){
  add_row(length.range = '0-5')}
t4$length.range[645]


################################################################################
#                   Unused Code - Renaming Table Columns
################################################################################


# RESULTS = Full1, Full2, Full3, Full4, 
# I will still need to rename these, so I did not use this method

stomach.ratio <- stomach.ratio %>%
   rename(Full = contains("Full"))


################################################################################
#                    Final Formatting Options
################################################################################


# Below are the different formating options for this table. 

# Base Code

t1 <- flextable(stomach.ratio) %>%
  add_header_row(top = TRUE,
                 values = c("", "Atlantic cod", "Greenland halibut", "Redfishes", "Skates"),
                 colwidths = c(1, 3, 3, 3, 3)) %>%
  
  set_header_labels(length.range = "Size Class",        # renames columns
                    Full_Atlantic.cod = "Full",
                    Empty_Atlantic.cod = "Empty",
                    Total_Atlantic.cod = "Total",
                    Full_Greenland.halibut = "Full",
                    Empty_Greenland.halibut = "Empty",
                    Total_Greenland.halibut = "Total",
                    Full_Redfish = "Full",
                    Empty_Redfish = "Empty",
                    Total_Redfish = "Total",
                    Full_Skate = "Full",
                    Empty_Skate = "Empty",
                    Total_Skate = "Total") %>%
  
  bg(bg = "lightgray", part = "header") %>%             # defines header colour
  bold(part = "header") %>%                             # header text bold
  
  align(part = "all", align = "center")  %>%             # centers the entire document
  
  width(j=1, width = 18, unit = "mm") %>%               # define column width as narrow as possible
  width(j=c(2, 5, 8, 11), width = 12, unit = "mm") %>%   
  width(j=c(3, 6, 9, 12), width = 16, unit = "mm") %>%
  width(j=c(4, 7, 10, 13), width = 13, unit = "mm") %>%
  
  # manual caption formatting to insert footnote numbering
  set_caption(caption = as_paragraph(                   # allows manual formatting in caption
    as_i("Number of full"),                             # italics
    as_i(as_sup("1")),                                  # superscript 1
    as_i(" empty, and total (full and emtpy) stomachs collected for Atlantic Cod ("), # spacing within "" matters!
    "Gadus morhua",
    as_i("), Greenland Halibut ("),
    "Reinhardtius hippoglossides",
    as_i("), redfishes ("),
    "Sebastes ",
    as_i("sp.), and skates ("),
    "Rajidae",
    as_i(") within each size class"),
    as_i(as_sup("2")),
    as_i(".")
  ))  %>%                        
  
  # add footnotes compatible to Markdown
  add_footer_lines(as_paragraph(                     # allows manual formatting in footnote
    as_sup("1"),                                     # superscript 1
    " A full stomach is any stomach that was not empty and contained prey items other than only parasites and/or only mucous.")) %>%
  
  # add second footnote
  add_footer_lines(as_paragraph(                     # will auto insert in new line
    as_sup("2"),                                     
    " Total length (cm) was used to measure all predators."))


######################## Option 01 - Color Total ###############################


# adds pale gray colour to the 'Total' columns
# aiming to create visual distinctions between sp. without vertical lines

t1 %>%
  bg(j=c(4, 7, 10, 13), bg = "#EFEFEF", part = "body")


###################### Option 02 - Color Columns ###############################


# adds pale gray colour to the 'Size Classes' and 'Total' columns
# aiming to create visual distinctions between sp. without vertical lines
# creates more evenly patterned/spaced coloured columns

t1 %>%
  bg(j=c(1, 4, 7, 10, 13), bg = "#EFEFEF", part = "body")


###################### Option 03 - Vertical Lines ##############################


# adds vertical lines to separate predator species

t1 %>%
  vline(j=c(1, 4, 7, 10), part = "all")               # vertical lines throughout entire table beside specified columns


###################### Option 04 - Horizontal Shading ##############################


# adds vertical lines to separate predator species
# adds shaded rows to every second line

t1 %>%
  vline(j=c(1, 4, 7, 10), part = "all") %>%               # vertical lines throughout entire table beside specified columns
  
  bg(i = seq(from = 2, to = nrow(stomach.ratio), by = 2), # select every 2nd row until the max row number 
     bg = "#EFEFEF",                                      # color
     part = "body")                                       # table part affected


