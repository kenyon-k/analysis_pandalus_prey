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
    # Table Code
        # subsection: tutorials
    # Unused but Interesting code......


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
        # tutorial 10
        # funs() has been replaced
        # could not make the replacement functions work

    # (select-contains) https://stackoverflow.com/questions/29018292/select-columns-based-on-multiple-strings-with-dplyr-contains
        # tutorial 20
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



################################################################################
#                        Table Formatting - gt package
################################################################################


### Coding Category

    # (highlight details) website
        # Tutorial ?
        # What did I learn or use from it?


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

###########################    Tutorial 10   ###################################


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


###########################    Tutorial 20   ###################################

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
#                                 Unused Code
################################################################################


