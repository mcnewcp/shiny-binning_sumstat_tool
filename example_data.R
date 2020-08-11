### example dataset for testing
###
###
if(!require(pacman)) install.packages('pacman')
p_load(tidyverse, lubridate, openxlsx)

write.xlsx(
  list(
    "Data Sheet 1" = tibble(
      date_time = seq.POSIXt(ymd_hms("2020-08-09 10:13:50"), length.out=2000, by="10 sec"),
      Value = rlnorm(2000, 4)
    ),
    "Another Data Sheet" = tibble(
      different_date_time_col = seq.POSIXt(ymd_hms("2020-08-09 10:13:50"), length.out=2000, by="10 sec"),
      Value_2 = rlnorm(2000, 4)
    )
  ),
  "example_data.xlsx"
)
