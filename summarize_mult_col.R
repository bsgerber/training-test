library(tidyr)
library(dplyr)

df <- data.frame(
  id = c(1000, 1001, 1002, 1003, 1004),
  x1 = c(1, 0, 1, 0, 0),
  x2 = c(0, 1, 0, 0, 1),
  x3 = c(1, 0, 0, 1, 0)
)

df

# Convert multiple binary columns to single variable (note will have 
# multiple rows per id if not mutually exclusive)

df %>%
  pivot_longer(cols = starts_with("x"), names_to = "var", values_to = "val") %>%
  filter(val == 1) %>%
  select(-val)
