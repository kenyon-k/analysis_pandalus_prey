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

### Formatting Text

    # (italic body text) https://ardata-fr.github.io/flextable-book/define-visual-properties.html
        # flextable book
        # details all of flextable


###########################    Tutorial ?   ###################################


# From ............... channel website

# brief description


dat <- data.frame(
  wool = c("A", "B"),
  L = c(44.56, 28.22), 
  M = c(24, 28.77), 
  H = c(24.56, 18.78)
)



flextable(dat)  %>% 
  style(i = 1, 
        pr_t = fp_text_default(
          italic = TRUE, 
          color = "red")) %>% 
  style(i = 2, j = 3:4, 
        pr_t = fp_text_default(
          shading.color = "yellow"),
        pr_p = fp_par(
          text.align = "center", padding = 1))

?fp_par


###########################    Tutorial ?   ###################################

df <- structure(list(
  type = c("glop glop", "glop glop" , "glop glop", "pas glop pas glop", "pas glop pas glop"), 
  what = c("Group", "Age", "Residence", "Smoker", "Europe"), 
  `1` = c(63, 25, 25, 15, 15), 
  `2` = c(23, 53, 53, 74, 11),
  `3` = c(85, 22, 43, 13, 15)
), 
row.names = c(NA, -5L), 
class = c("data.frame"))

dfg <- as_grouped_data(df, groups = "type") # %>% 
  as_flextable() %>% 
  add_footer_lines("Observed event") %>%
  set_header_labels(what = "") %>%      # removing the header for column what
  color(part = "footer", color = "#800000") %>%
  bold( bold = TRUE, part="header") %>% 
  align(i = ~ !is.na(type), align = "center") %>% 
  bold(i = ~ !is.na(type))
  
  
  
##### trying with copilot
  
  gf <- data.frame(
    Group1 = rep(c("A", "B"), each = 5),
    Group2 = rep(c("X", "Y"), times = 5),
    Value = rnorm(10)
  ) 
  
  gf <- gf %>% group_by(Group1, Group2) # %>%
    summarise(Value = mean(Value))

  grouped_gf <- as_grouped_data(gf, groups = c("Group1", "Group2"))  
  
flextable(gf) %>%
  merge_v(j = c("Group2")) # %>%
  align(j = c("Group1", "Group2"), align = "left", part = "all")
  
as_flextable(grouped_gf) %>%
  merge_v(j = c("Group1", "Group2"))
  set_header_labels(Value = "Random Value") %>%
  theme_vanilla()


#### copilot attempt 2

flextable(gf) %>%
  merge_h_range(j1 = "Group1", j2 = "Group2") %>%
  set_header_labels(Value = "Random Value") %>%
  theme_vanilla()
  

#####
flextable(gf) %>%
  merge_v(j=c("Group1", "Group2")) %>%
  align(j = c("Group1", "Group2"), align = "center", part = "all")

#### more copilot

df <- data.frame(
  Group1 = rep(c("A", "B"), each = 5),
  Group2 = rep(c("X", "Y"), times = 5),
  Character = sample(letters, 10),
  Numeric = rnorm(10)
)

df <- df %>% group_by(Group1, Group2)

df_with_labels <- df %>%
  mutate(Group1 = ifelse(duplicated(Group1), "", Group1),
         Group2 = ifelse(duplicated(Group2), "", Group2))

flextable(df_with_labels) %>%
  merge_v(j = c("Group1", "Group2"))


library(gt)

df %>%
  group_by(Group1, Group2) %>%
  gt() %>%
  row_group_order(groups = c("Group1", "Group2"))

?groupname_col  

?tab_row_group
 


gtcars |>
  dplyr::select(model, year, hp, trq) |>
  dplyr::slice(1:8) |>
  gt(rowname_col = "model") |>
  tab_row_group(
    label = "numbered",
    rows = matches("^[0-9]")
  )  |>
  row_group_order(groups = c(NA, "numbered"))



###### Copilot with GT

df <- data.frame(
  Group1 = rep(c("A", "B"), each = 5),
  Group2 = rep(c("X", "Y"), times = 5),
  CharacterVar = sample(letters, 10),
  NumericVar = rnorm(10)
)
  
df_grouped <- df %>%
  group_by(Group1, Group2) %>%
  summarise(
    CharacterVar = paste(CharacterVar, collapse = ", "),
    NumericVar = mean(NumericVar)
  )

df_grouped %>%  # so far this is the best structure....
  gt() %>%
  tab_header(
    title = "Grouped Table Example",
    subtitle = "With Character and Numeric Variables"
  ) #  %>%
  row_group_order(
    groups = c("Group1", "Group2")
  ) #%>%
  row_group(
    group = "Group1",
    rows = Group1
  ) %>%
  row_group(
    group = "Group2",
    rows = Group2
  )

## Attempt 2

df_grouped %>%
  gt() %>%
  tab_header(
    title = "Grouped Table Example",
    subtitle = "With Character and Numeric Variables"
  ) %>%
  tab_row_group(
    label = "Group1",
    rows = Group1 == "A"
  ) %>%
  tab_row_group(
    label = "Group2",
    rows = Group2 == "X"
  )

##

df_grouped %>%
  gt() %>%
  tab_header(
    title = "Grouped Table Example",
    subtitle = "With Character and Numeric Variables"
  ) %>%
  cols_merge(
    columns = vars(Group2, CharacterVar),
    pattern = "{1}: {2}"
  ) %>%
  cols_label(
    Group2 = "Group2 and CharacterVar"
  )

## More attempts


d <- data.frame(
  Group1 = rep(c("ANNELIDA", "ARTHROPODA", "FISHES"), each = 3),
  Group2 = c("Polychaeta", "Crustacea", "Malacostraca", "Decapoda", "Amphipoda", "Themisto sp.", "Gadidae", "Benthosema glaciale", "Unidentified Fishes"),
  CharacterVar = c("-", "-", "-", "0.1", "-", "<0.1", "17.1", "4.2", "2.8"),
  NumericVar = c("-", "-", "-", "-", "-", "-", "19.4", "0.8", "1.4")
)

d %>%
  gt() %>%
  tab_header(
    title = "Relative Contribution of Different Prey Taxa",
    subtitle = "Expressed as Percent by Weight (%W) and Percent by Number (%N)"
  ) %>%
  tab_row_group(
    label = "ANNELIDA",
    rows = Group1 == "ANNELIDA"
  ) %>%
  tab_row_group(
    label = "ARTHROPODA",
    rows = Group1 == "ARTHROPODA"
  ) %>%
  tab_row_group(
    label = "FISHES",
    rows = Group1 == "FISHES"
  ) %>%
  cols_label(
    Group2 = "Prey/Taxon",
    CharacterVar = "Percent by Weight (%W)",
    NumericVar = "Percent by Number (%N)"
  ) %>%
  fmt_missing(
    columns = everything(),
    missing_text = "-"
  )

# attempts continue

d <- d %>%
  mutate(DisplayGroup = ifelse(duplicated(Group1), Group2, Group1))

d %>%
  gt() %>%
  tab_header(
    title = "Relative Contribution of Different Prey Taxa",
    subtitle = "Expressed as Percent by Weight (%W) and Percent by Number (%N)"
  ) %>%
  cols_label(
    DisplayGroup = "Prey/Taxon",
    CharacterVar = "Percent by Weight (%W)",
    NumericVar = "Percent by Number (%N)"
  ) %>%
  fmt_missing(
    columns = everything(),
    missing_text = "-"
  )# %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = c(DisplayGroup),
      rows = !duplicated(df$Group1)
    )
  )
  
### Trying flextable after giving the impage to copilot
  
  data <- data.frame(
    Group1 = c("ANNELIDA", "", "ARTHROPODA", "", "", "", "", "", "", "", "", "", "PISCES", "", "", "", "UNIDENTIFIABLE MATERIAL"),
    Group2 = c("", "none", "", "Crustacea", "Unidentifiable Crustacea", "Amphipoda", "Themisto sp.", "Decapoda", "Unidentifiable Decapoda", "Pandalus borealis", "Pandalus montagui", "Pandalus sp.", "", "Unidentifiable Pisces", "Unidentifiable Gadidae", "Benthosema glaciale", ""),
    Character_Vector = c("", "Unidentifiable Polychaeta", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),
    Percent_by_Weight = c("", "0.2", "", "", "0.1", "", "< 0.1", "", "4.0", "68.9", "1.1", "17.1", "", "4.2", "2.8", "0.4", "1.2"),
    Percent_by_Number = c("", "", "", "", "", "", "", "", "", "76.4", "1.4", "19.4", "", "", "1.4", "1.4", "")
  )
  
  
  ft <- flextable(data)
  
  ft <- compose(ft, j = "Group1", value = as_paragraph(
    as_chunk("ANNELIDA", props = fp_text(bold = TRUE)),
    as_chunk("", props = fp_text()),
    as_chunk("ARTHROPODA", props = fp_text(bold = TRUE)),
    as_chunk("", props = fp_text()),
    as_chunk("", props = fp_text()),
    as_chunk("", props = fp_text()),
    as_chunk("", props = fp_text()),
    as_chunk("", props = fp_text()),
    as_chunk("", props = fp_text()),
    as_chunk("", props = fp_text()),
    as_chunk("", props = fp_text()),
    as_chunk("", props = fp_text()),
    as_chunk("PISCES", props = fp_text(bold = TRUE)),
    as_chunk("", props = fp_text()),
    as_chunk("", props = fp_text()),
    as_chunk("", props = fp_text()),
    as_chunk("UNIDENTIFIABLE MATERIAL", props = fp_text(bold = TRUE))
  ))
  
  ft <- compose(ft, j = "Group2", value = as_paragraph(
    as_chunk("", props = fp_text()),
    as_chunk("none", props = fp_text(indent = 1)),
    as_chunk("", props = fp_text()),
    as_chunk("Crustacea", props = fp_text(indent = 1)),
    as_chunk("Unidentifiable Crustacea", props = fp_text(indent = 2)),
    as_chunk("Amphipoda", props = fp_text(indent = 2)),
    as_chunk("Themisto sp.", props = fp_text(indent = 2)),
    as_chunk("Decapoda", props = fp_text(indent = 1)),
    as_chunk("Unidentifiable Decapoda", props = fp_text(indent = 2)),
    as_chunk("Pandalus borealis", props = fp_text(indent = 2)),
    as_chunk("Pandalus montagui", props = fp_text(indent = 2)),
    as_chunk("Pandalus sp.", props = fp_text(indent = 2)),
    as_chunk("", props = fp_text()),
    as_chunk("Unidentifiable Pisces", props = fp_text(indent = 1)),
    as_chunk("Unidentifiable Gadidae", props = fp_text(indent = 1)),
    as_chunk("Benthosema glaciale", props = fp_text(indent = 1)),
    as_chunk("", props = fp_text())
  ))
  

  
  
  #################### tutorial selecting duplicate rows    ##################  
  
 ?distinct 
  
  df <- data.frame(
    id = c(1, 2, 20, 3, 4, 40, 5),
    value = c("apple", "banana", "apple", "cherry", "banana", "date", "apple")
  )
  
  # Remove duplicates based on the 'value' column, keeping the first occurrence
  df_unique <- df %>% distinct(value, .keep_all = TRUE)
  
  str(df)
  
  
###################### tutorial 2 selecting duplicate rows ###################
  
https://stackoverflow.com/questions/50839185/identify-duplicates-in-a-list-ignoring-na-values-in-r#:~:text=I%20would%20like%20to%20identify%20whether%20there%20are%20duplicates%20in
  

  df <- data.frame(
    Column1 = c(1,2,1,2),
    Column2 = c(2,1,2,NA),
    Column3 = c(NA,1,1,NA),
    Column4 = c(NA,NA,2,1),
    stringsAsFactors = FALSE
  )

  
  df %>%
    rowwise() %>%
    mutate(dups = anyDuplicated(na.omit(c(Column1,Column2,Column3,Column4))))   %>%
    ungroup()  %>%
    mutate(index = row_number())  %>%
    filter(dups > 0) # %>%
    .$index
  
    burnt.toast %>%
      ungroup() %>%
      rowwise() %>%
      mutate(dups = anyDuplicated(na.omit(c(Phylum, Order, ScientificName_W, taxa.weight)))) #  %>%
    ungroup()  %>%
      mutate(index = row_number())  %>%
      filter(dups > 0)  %>%
      .$index
    
    
burnt.toast %>%
  

    ?rowwise    
    
?anyDuplicated
anyDuplicated(jam.toast$Phylum)
  
jam.toast <- as_grouped_data(burnt.toast, groups = c("Phylum", "Order"))

### Below identifies duplicate rows I want deleted with a TRUE
duplicated(jam.toast$Phylum, incomparables = NA)

butter <- jam.toast %>%
  mutate(dups = duplicated(Phylum, incomparables = NA)) %>%
  mutate(test = ifelse(dups == TRUE,
         "1",
         "0")) %>%
  mutate(index = row_number()) %>%
  filter(test == "1") %>%
  .$index

#####################   Testing arrange and group by shenanagins

cone   # final clean version after data processing. It's ready for final flextable formats

# first step would be sorting the columns alphebetically
cone <- cone %>%
  arrange(Phylum, Order, by_group = TRUE)

# assigning which rows that I will want to delete within the table
acorn <- as_grouped_data(cone, groups = c("Phylum", "Order")) # this data structure matches what flextable will produce naturally

nut <- acorn %>%
  mutate(dups = duplicated(Phylum, incomparables = NA)) %>%
  # mutate(test = ifelse(dups == TRUE,
  #                      "1",
  #                      "0")) %>%
  mutate(index = row_number()) %>%
  filter(dups == TRUE) %>%
  .$index

str_detect(cone$Phylum, pattern = 'A')

as_flextable(cone,
             hide_grouplabel = TRUE) %>%              # removes labels flextables adds onto each group (keeps group name as is within dataset)
  
  set_header_labels(ScientificName_W = "Prey/Taxon",
                    taxa.weight = "Percent by Weight (%W)") %>% 
  delete_rows(i = nut, part = 'body') %>%
  delete_rows(i = ~ str_detect(Order, pattern = 'A')) %>%
  

bg(bg = "lightgray", part = "header") %>%               # defines header colour
  bold(bold = TRUE, part = "header") %>%              # header text bold
  
  bold(i = ~ !is.na(Phylum), j = 1) %>%                      # phylum text is bold
  bold(i = ~ !is.na(Order), j = 1) %>%                       # Order text is bold
  style(i = ~ str_detect(ScientificName_W,                 # making only scientific names italic by:
                         pattern = 'Unidentifiable',       # string to search for is 'Unidentifiable'
                         negate = TRUE),                   # select any row that DOES NOT contain 'Unidentifiable'
        j = 1,
        pr_t = fp_text_default(italic=TRUE)) %>%                          # make them italic
  
  style(i = ~ !is.na(ScientificName_W),                       # Scientific Names in 1st column
        j = 1,                                                # first column only
        pr_p = fp_par(text.align = "left", padding.left = 15)) %>%    # add padding to left of selected text
  
  
  border(i = ~!is.na(Phylum),                                 # horizontal border per Phylum
         border.top = fp_border(color = "black"),             # placing the border on top of the row
         part = "body") %>%                                   # within the body of the table
  
  
  width(j = 1, width = 55, unit = "mm") %>%
  width(j = 2, width = 30, unit = "mm") # %>%



########################


burnt.toast %>%
  ungroup() %>%
  rowwise() %>%
  mutate(dups = anyDuplicated(na.omit(c(Phylum, Order, ScientificName_W, taxa.weight)))) #  %>%
ungroup()  %>%
  mutate(index = row_number())  %>%
  filter(dups > 0)  %>%
  .$index
### 

burnt.toast %>%
  mutate(dups = ifelse())

##
mutate(prey.name = ifelse(Prey_OS_ID == 8111,         # create new column 'prey.name' with values based on logical check
                          "borealis",                        # value if logical check is TRUE
                          ifelse(Prey_OS_ID == 8112,          # if logical check is FALSE, begin second logical test
                                 "montagui",                  # value if second logical test is TRUE
                                 ifelse(Prey_OS_ID == 8110,   # if second logical test is FALSE, being third logical test
                                        "Pandalus",           # value if third logical test is TRUE
                                        "other") 
#################### Code SAving for Quick Clean to send to Dan #############
  
  ### Creating Table 3A Specific Dataframe
  
  # colSums(is.na(apple))
  # colSums(is.na(prey))
  # 
  # apple %>%
  #   replace(is.na())
  
  
  #####
  # names(apple)
  # 
  # table(apple$pred.name, useNA = 'always')
  
  t3a <- apple  %>%     # manipulate 'apple' df and save to new Table 3a df 
    
    # Step 1: converts NAs within PreyWt to 0s 

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


