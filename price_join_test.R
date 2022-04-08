library(tidyverse)
library(stringdist)
library(furrr)

# Add column of cleaned items
daily_table_prices <- daily_table_prices %>%
  mutate(ItemCleaned=str_replace_all(Item, "[[:punct:]]", " "),
         ItemCleaned=tolower(ItemCleaned)
  )

# This ugliness splits each ingredient line into separate columns. Runs on a random recipe for development
tibble(my_plate_recipes[sample.int(1073)[[1]],][['Ingredients']][[1]]) %>%
  mutate(Qty=str_extract(., "^\\d* *\\d+\\/*\\d*"), 
         UOM=str_match(., "^\\d* *\\d+\\/*\\d* (cup|can|package|slices|dash|pinch|tablespoon|teaspoon|pound|ounce)")[,2],
         Item=str_match(., "(?<=pound |pounds |can |cans |cups |cup |tablespoon |tablespoons |slices |slice |teaspoon |ounce |ounces |pound |pounds |teaspoons )[\\w\\d \\(\\),-\\.\\/%]+$")[,1],
         Item=if_else(is.na(Item), str_match(., "^\\d* *\\d+\\/*\\d*([\\w\\d \\(\\),-\\.\\/%])")[,2], Item),
         Item=if_else(is.na(UOM), ., Item),
         UOM=if_else(is.na(UOM), "each", UOM)
         ) %>%
  rename(Original=".") %>%
  select(Item)
  
# Reformat into table with one row per ingredient -------------------------



# Fuzzy matching ----------------------------------------------------------

# Concept: for each ingredient, calculate a score that counts the fraction of 
# words in each daily table item name that are present in the ingredient name.
# Report the maximum score and its corresponding match

match_ingredient <- function(ingredient) {
  
  ingredient <- unlist(str_split(ingredient, "\\W+"))
  
  daily_table_prices %>%
    mutate(MatchScore = map(ItemCleaned, function(x) sum(ain(ingredient, unlist(str_split(x, "\\W+")), method="lv", maxDist=1))),
           MatchScore = unlist(MatchScore)
    ) %>%
    filter(MatchScore > 0) %>%
    slice_max(MatchScore) %>%
    distinct()
}

# Check to see how many ingredients we can match

# Need to run multithreaded, otherwise way too slow. 
# Change depending on the number of cores your computer has.
# CAUTION: Takes 90 seconds on an M1 Max

## TODO: There are many issues with the matching (usually overmatching). Need to debug.

plan(multisession, workers = 10)

test <- my_plate_recipes %>%
  unnest(cols=Ingredients) %>%
  rename(Ingredient=".") %>%
  mutate(
    Ingredient = str_replace_all(Ingredient, "\\d+", ""),
    Ingredient = str_replace_all(Ingredient, "[[:punct:]]", " ")
         ) %>%
  mutate(Match=future_map(Ingredient, match_ingredient))
