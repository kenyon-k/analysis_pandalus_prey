################################################################################
################################################################################
#                   Table 3 Prey Percentages by Taxon - LEARNING CODE
################################################################################
################################################################################


# Author: Krista Kenyon
# Date: Aug 27/2024

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


### Filtering (Subsetting) Data

    # (retaining NAs) https://stackoverflow.com/questions/46378437/how-to-filter-data-without-losing-na-rows-using-dplyr
        # Filter will automatically remove any rows with NA's in the specified column
        # people have created a work-around with the replace_na() function
        # inserting the %>% replace_na() within the filter

    # (retaining NAs) https://stackoverflow.com/questions/40446165/how-to-subset-data-in-r-without-losing-na-rows
        # multiple methods of retaining NAs

    # (%in%) https://www.statology.org/r-filter-in/
        # does not exclude 'NA's
        # filters by condition vs logical true-false arguments


### Testing out regular expressions/regex for stringr()

    # str_view() shows you what your code highlights
        # I don't know where I found this. But it was useful

### Converting NA to 0

    # (overview) https://www.statology.org/dplyr-replace-na-with-zero/
        # NA to 0 for entire df
        # NA to 0 for particular column


################################################################################
#                        Table Formatting - flextable package
################################################################################


### Grouped Data

    # (basic structure) https://stackoverflow.com/questions/71661066/is-there-a-function-in-flextable-to-group-a-few-rows-in-a-table-together-under-a
        # data setup for grouped table
        # flextable() for grouped table


###########################    Tutorial ?   ###################################


# From ............... channel website

# brief description

################################################################################
#                   Unused Code - Filter() - Retaining NA's
################################################################################

# filter(logical expression) will automatically filter out NA's
    # NA's are considered blank and can't match or meet any logical expression

# I found a couple different options for retaining NA's from:
    # A) https://stackoverflow.com/questions/46378437/how-to-filter-data-without-losing-na-rows-using-dplyr
    # B) https://stackoverflow.com/questions/40446165/how-to-subset-data-in-r-without-losing-na-rows
    # C) https://www.statology.org/r-filter-in/

# I used the following to examine parameters within the filtered dataframes:
colSums(is.na(apple))                 # shows which columns have NAs

n_distinct(prey$Prey_OS_ID)           # number of unique values within column
                                      # 2019 prey$Prey_OS_ID has 80

### Base Filter - Prey_OS_ID has no 'NA's

# I originally had:

filter(
  Prey_OS_ID != 9998 |
  Prey_OS_ID != 9981 |
  Prey_OS_ID != 9982)  #etc

# but the multiple or (|) arguments weren't working today.... but the and (,) was....


# below is the original filter to remove empty stomachs/debris
# I've swiched that to account for filter removing 'NA's
    # this should never be an issue with Prey_OS_ID, but in case it is one year

apple <- prey %>%                        # manipulate 'prey' df and save to new Table 2 Base df 

  filter(                           # selects rows that do not (!=) contain:
    Prey_OS_ID != 9998,                # empty, and (,)
    Prey_OS_ID != 9981,                # sand
    Prey_OS_ID != 9982,                # stone
    Prey_OS_ID != 9983,                # shells
    Prey_OS_ID != 9987,                # plant material
    Prey_OS_ID != 10757) # %>%          # mud


# below are different methods to retain 'NA' values with filter.
# Each has pros/cons



### Source A: replace_na(TRUE)

# this works, but needs to be added within each filter argument
    # so it's fine if I'm just filtering one line
    # but is not ideal when I'm filtering for multiple logical tests

pizza <- apple %>%
  filter((Prey_Detail_Code != 40) %>%         # Prey_Detail_Code of 40 = parasitic content
           replace_na(TRUE))                  # retains NA's that filter would omit. BRACKET PLACEMENT IS IMPORTANT 


# it replace_na(TRUE) is used as a separate line, we get an error
apple %>%
  filter(Prey_Detail_Code != 40) %>%
  replace_na(TRUE)


# I don't know what this version does specifically, but it doesn't return the correct amount of rows
apple %>%
  filter(Prey_Detail_Code != 40 & replace_na(TRUE))



### Source B: is.na(column)

# this version has similar limitations to Source A:
    # it does not work with an 'and' condition - 0 rows will be retained
    # it will work with an 'or' condition
    # my filter list for Prey_OS_ID != stopped working with 'or' conditions
    # so if I used this verison, it would need to be added to EACH condition.....

skip <- apple %>%
filter(Prey_Detail_Code != 40| is.na(Prey_Detail_Code))


# no rows are selected if it's used with an 'and' (,) condition
apple %>%
  filter(Prey_Detail_Code != 40,
         is.na(Prey_Detail_Code))


# the 'and' (,) condition works with other values
apple %>%
  filter(Prey_Detail_Code != 40,
         Prey_Detail_Code != 81)



### Source C & B: %in%
    
# I ended up using this version when I had multiple filter specifications

# this version searches for what matches instead of using a logical true-false equation
    # therefore it doesn't drop 'NA's when you are removing rows
apple %>%
  filter(!Prey_Detail_Code %in% c('40'))


