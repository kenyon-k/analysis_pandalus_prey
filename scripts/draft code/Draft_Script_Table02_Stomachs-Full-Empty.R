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


###########################    Tutorial ?   ###################################


# From ............... channel website

# brief description

################################################################################
#                                 Unused Code
################################################################################


