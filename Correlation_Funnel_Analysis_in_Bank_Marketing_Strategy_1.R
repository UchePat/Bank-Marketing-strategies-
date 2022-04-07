# BANK MARKETING ANALYSIS PROCEDURE using R. We have already done it using Excel(in d bank_term_deposit_marketing_analysis.xlsx worksheet) now we want to do it using R
# Taking a Business Process from EXCEL TO R & using THE CORRELATION FUNNEL for Analysis

# We are analyzing d parameters(which are from 4th to 7th sheets) in d workbook that greatly influence enrollment/subscription in Term Deposit(which is a feature provided by the Bank) in Subcription History sheet/Term Deposit column

# Libraries
library(tidyverse)
library(readxl)
library(recipes)
library(tidyquant)
library(ggrepel)


# 1.0 READ EXCEL SHEETS ----
path   <- "bank_term_deposit_marketing_analysis.xlsx"  # This is excel dataset that contains d dataset

sheets <- excel_sheets(path)  # displays all d sheet names in d workbook
sheets


# 2.0 INVESTIGATE DATA FOR EACH SHEET ----
# Displays all d data in each sheet in d workbook
sheets %>%
  map(~ read_excel(path  = path, sheet = .)) %>%  # using sheet = . will displays all d data in each sheet
  set_names(sheets)


# 3.0 LETS PERFORM VLOOKUP EQUIVALENT ----\
# Our dataset parameters are in sheets 4 to 7 so we select only 4th to 7th sheet and combine all their data into 1 sheet
data_joined_tbl <- sheets[4:7] %>%   # d 4th to 7th sheets contain d main info parameters that we are going to do analysis on
  map(~ read_excel(path = path, sheet = .)) %>%
  reduce(left_join)   

data_joined_tbl 



# 4.0 ANALYZE ----
# 4.1 Prepare Data ----
data_joined_tbl %>% glimpse()

# using recipe() 
recipe_obj <- recipe(~ ., data = data_joined_tbl) %>%  # we are using all d columns are independent columns
  step_rm(ID) %>%                       # we delete ID column since it is not useful
  step_discretize(all_numeric(), options = list(min_unique = 1)) %>%   
  step_dummy(all_nominal(), one_hot = TRUE, naming = partial(dummy_names, sep = "__")) %>% # creates dummy variables from all numerical variables/columns and and all columns will have numeric binary values.
  prep()  

data_transformed_tbl <- data_joined_tbl %>%
  bake(recipe_obj, new_data = .) 

data_transformed_tbl %>% glimpse()  # The columns will be divided and renamed based on d unique values in them and all columns will have numeric binary values



# 4.2 Correlation Analysis ----
# Prepare Correlations
correlation_tbl <- data_transformed_tbl %>%  
  cor(y = data_transformed_tbl$TERM_DEPOSIT__yes) %>%  
  as_tibble(rownames = "feature") %>%  
  rename(TERM_DEPOSIT__yes = V1) %>%  
  separate(feature, into = c("feature", "bin"), sep = "__") %>%  
  filter(!is.na(TERM_DEPOSIT__yes)) %>%    # removes all NA values
  filter(!str_detect(feature, "TERM_DEP")) %>%  
  arrange(abs(TERM_DEPOSIT__yes) %>% desc()) %>%  # sort absolute values in d stated column in descending order
  mutate(feature = as_factor(feature) %>% fct_rev())  # convert d stated column to a factor datatype

correlation_tbl


# Visualize d Correlations Funnel using scatterplot chart
correlation_tbl %>%
  ggplot(aes(TERM_DEPOSIT__yes, y = feature, text = bin)) + 
  geom_vline(xintercept = 0, linetype = 2, color = "red") + 
  geom_point(color = "#2c3e50") +   # creates datapoints in d charts
  geom_text_repel(aes(label = bin), size = 3, color = "#2c3e50") +  # adds text values to d datapoints based on d unique values of d columns at y-axis
  expand_limits(x = c(-0.4, 0.4)) +  # increasing d range of values in x-axis
  theme_tq() +
  labs(title = "Bank Marketing Analysis",
       subtitle = "Correlations to Enrollment in Term Deposit",
       y = "", x = "Correlation to Term Deposit")



# 4.3 Lets Interpret the Correlations ----
# We have bins as values for Duration, Balance, Campaign, Day and Age in d chart and we need to know what values in dem bins?
recipe_obj %>% tidy()   # the bins were created in recipe_obj object using recipe() much earlier

bins_tbl <- recipe_obj %>% tidy(2)
bins_tbl               # here we see d true values for each bins for d columns
View(bins_tbl)

bins_tbl %>% filter(terms == "DURATION")  # here we see d true values in each bins in Duration only. -inf means -infinite



# 5.0 STRATEGY ----
# WE FOCUS ON value- BIN4 in DURATION column and value- success in POUTCOME column since they are d top columns that affects Term Deposit enrollment and their stated values are d only values at far RHS
strategy_tbl <- data_joined_tbl %>%  # we are using d data that we got when we merged d data from 4th sheet to 7th sheet into 1 full dataset(before we used recipe()) 
  select(DURATION, POUTCOME, TERM_DEPOSIT) %>%  # remember Term Deposit is our dependent variable
  mutate(POTENTIAL = case_when(DURATION > 319 ~ "High Potential",
                               POUTCOME == "success" ~ "High Potential",  # if DURATION > 319 or POUTCOME == "success"input High Potential, else input Normal
                               TRUE ~ "Normal")) %>%    # create a new column with d stated conditions from d existing columns
  group_by(POTENTIAL) %>%  # displays unique values in d stated column
  count(TERM_DEPOSIT) %>%  
  mutate(prop = n / sum(n)) %>%  # creates a new column- prop whose values would be gotten using d stated formula
  ungroup() %>%
  mutate(label_text = str_glue("n: {n}
                                 prop: {scales::percent(prop)}"))  # creates a new column- label_text with values taken from n and prop columns

strategy_tbl



# 6.0 REPORT RESULTS ----
# creating a stacked Bar chart
strategy_tbl %>%
  ggplot(aes(POTENTIAL, prop, fill = TERM_DEPOSIT)) + 
  geom_col() +
  geom_label(aes(label = label_text), fill = "white", color = "#2c3e50") +  # displays values in each stacked section of d bar
  scale_fill_tq() +                      # changes d color of d stacked bars 
  scale_y_continuous(labels = scales::percent_format()) +  # changes d y-axis values from decimals to percentages
  theme_tq() +
  labs(title = "Bank Marketing Strategy",
       subtitle = str_glue("Targeting customers that haven't been contacted in 319 days 
                             or those with prior enrollments yields 32% vs 4.3%"))