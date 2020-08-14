### develop and debug methods
###
###

### Operators to be user selected
file_name <- "example_data.xlsx"  #file selection in shiny
sheet_name <- "Another Data Sheet" #drop down selection in shiny
bin_min <- 15 #bin width in minutes, integer entry in shiny
date_time_col <- "different_date_time_col" #drop down selection in shiny, populated from xlsx
value_col <- "Value_2" #drop down selection in shiny, populated from xlsx

###read file
dataDF <- read.xlsx(file_name, sheet_name)

#turn column names into symbols for tidy
date_time_col <- sym(date_time_col)
value_col <- sym(value_col)

#select min date time rounded down to 15 minutes
start_date_time_selection <- dataDF %>%
  pull(!!date_time_col) %>%
  min() %>% convertToDateTime() %>%
  floor_date("15 minutes")

statDF <- dataDF %>%
  select(!!date_time_col, !!value_col) %>%
  #fix date time
  mutate_at(vars(!!date_time_col), convertToDateTime) %>%
  #add na row at start_date_time_selection for binning
  bind_rows(
    tibble(!!date_time_col := start_date_time_selection, !!value_col := NA)
  ) %>% 
  arrange(!!date_time_col) %>%
  #bin data
  mutate(start_time = cut(!!date_time_col, breaks = paste(bin_min, "mins"))) %>%
  #convert to date time
  mutate_at(vars(start_time), ymd_hms) %>%
  mutate(end_time = start_time + minutes(bin_min)) %>%
  #generate stats grouped by bin start time
  group_by(start_time) %>%
  mutate(max_value = max(!!value_col, na.rm = TRUE), min_value = min(!!value_col, na.rm = TRUE)) %>%
  ungroup() %>%
  #label rows as max or min
  mutate(
    is_max = ifelse(!!value_col==max_value, TRUE, FALSE),
    is_min = ifelse(!!value_col==min_value, TRUE, FALSE)
  )

#generate outDF for display and export
outDF <- left_join(
  statDF %>%
    filter(is_max) %>%
    select(start_time, end_time, max_value, time_of_max_value = !!date_time_col),
  statDF %>%
    filter(is_min) %>%
    select(start_time, end_time, min_value, time_of_min_value = !!date_time_col),
  by = c("start_time", "end_time")
)
