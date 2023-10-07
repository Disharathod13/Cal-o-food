library(tidyverse)
data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-09-04/fastfood_calories.csv")
fastfoodcalories <- data %>%
  mutate(
    carb_percent = (total_carb / calories) * 100,
    protein_percent = (protein / calories) * 100,
    fiber_percent = (fiber / calories) * 100,
    vit_a_percent = (vit_a / calories) * 100,
    vit_c_percent = (vit_c / calories) * 100,
    calcium_percent = (calcium / calories) * 100,
    cholesterol_percent = (cholesterol / calories) * 100,
    sugar_percent = (sugar / calories) * 100,
    sodium_percent = (sodium / calories) * 100,
    sat_fat_percentage = (sat_fat / calories) * 100
  )


usethis::use_data(fastfoodcalories, overwrite = TRUE)
