library(tidyverse)
library(stringdist)
library(furrr)
library(tm)

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
  rename(Original=".")
  
# Reformat into table with one row per ingredient -------------------------



# Fuzzy matching ----------------------------------------------------------

# Concept: for each ingredient, calculate a score that counts the fraction of 
# words in each daily table item name that are present in the ingredient name.
# Report the maximum score and its corresponding match

force_matches <- tibble(
  WrongMatch = c
)

match_ingredient <- function(ingredient) {
  
  ingredient <- unlist(str_split(ingredient, "\\W+"))
  
  daily_table_prices %>%
    mutate(MatchScore = map(ItemCleaned, function(x) sum(ain(ingredient, unlist(str_split(x, "\\W+")), method="lv", maxDist=1))/sapply(strsplit(x, " "), length)),
           MatchScore = unlist(MatchScore)
    ) %>%
    filter(MatchScore > 0) %>%
    slice_max(MatchScore) %>%
    distinct() %>%
    slice_min(Price, with_ties = F) %>%
    pull(ItemCleaned)
}

# Check to see how many ingredients we can match

## TODO: There are many issues with the matching (usually overmatching). Need to debug.

# Plan: Match My Plate ingredients to Daily table cleaned items. Filter out Daily Table recipes that aren't matched.

extra_stopwords <- c(
  "low",
  "chopped",
  "sliced",
  "cups",
  "pounds",
  "cup",
  "pound",
  "ounces",
  "ounce",
  "oz",
  "tablespoon",
  "teaspoon",
  "teaspoons",
  "tablespoons",
  "use",
  "diced",
  "cut",
  "chunks",
  "peeled",
  "grated",
  "minced",
  "melted",
  "slices",
  "cooked",
  "tsp",
  "divided",
  "ground",
  "fresh",
  "uncooked",
  "trimmed",
  "shredded",
  "finely",
  "quartered",
  "boiled",
  "small",
  "medium",
  "large",
  "lengthwise",
  "rinsed",
  "removed",
  "melted",
  "thinly",
  "thickly",
  "cut",
  "piece",
  "pieces",
  "optional",
  "crushed",
  "taste",
  "inch",
  "inches",
  "half",
  "third",
  "halvess",
  "thirds",
  "used",
  "use",
  "mashed",
  "mash",
  "serve",
  "served",
  "peeled",
  "pinch",
  "dash",
  "added",
  "finely",
  "fine",
  "may",
  "part",
  "drain",
  "drained",
  "baked",
  "portion",
  "portions",
  "take"
)
full_stopwords <- c(stopwords("english"), extra_stopwords)


# Need to run multithreaded, otherwise way too slow. 
# Change depending on the number of cores your computer has.
# CAUTION: Takes 90 seconds on an M1 Max
plan(multisession, workers = 10)

recipes_matched <- my_plate_recipes %>%
  unnest(cols=Ingredients) %>%
  rename(OrigIngredient=".") %>%
  mutate(
    CleanIngredient = str_replace_all(OrigIngredient, "\\d+", ""),
    CleanIngredient = str_replace_all(CleanIngredient, "[[:punct:]]", " "),
    CleanIngredient = tolower(CleanIngredient),
    CleanIngredient = removeWords(CleanIngredient, full_stopwords),
    CleanIngredient = str_squish(CleanIngredient)
         ) %>%
  mutate(Match=future_map(CleanIngredient, match_ingredient)) %>%
  mutate(Qty=str_extract(OrigIngredient, "^\\d* *\\d+\\/*\\d*"), 
         UOM=str_match(OrigIngredient, "^\\d* *\\d+\\/*\\d* (cup|can|package|slices|dash|pinch|tablespoon|teaspoon|pound|ounce)")[,2],
         Item=str_match(OrigIngredient, "(?<=pound |pounds |can |cans |cups |cup |tablespoon |tablespoons |slices |slice |teaspoon |ounce |ounces |pound |pounds |teaspoons )[\\w\\d \\(\\),-\\.\\/%]+$")[,1],
         Item=if_else(is.na(Item), str_match(OrigIngredient, "^\\d* *\\d+\\/*\\d*([\\w\\d \\(\\),-\\.\\/%])")[,2], Item),
         Item=if_else(is.na(UOM), OrigIngredient, Item),
         UOM=if_else(is.na(UOM), "each", UOM)
  )

# See how many recipes have all matched ingredients
recipes_matched %>%
  unnest(Match, keep_empty = T) %>%
  group_by(Name) %>%
  filter(!any(is.na(Match))) %>%
  distinct(Name) %>%
  ungroup() %>%
  summarize(n())


# Export formatted csv of recipe constraints ------------------------------

# Convert everything to either grams or each. Assume a density of 1.
recipes_matched %>%
  select(UOM) %>%
  distinct(UOM)

mixedToFloat <- function(x){
  # https://stackoverflow.com/a/10676800
  is.integer  <- grepl("^\\d+$", x)
  is.fraction <- grepl("^\\d+\\/\\d+$", x)
  is.mixed    <- grepl("^\\d+ \\d+\\/\\d+$", x)
  # stopifnot(all(is.integer | is.fraction | is.mixed))
  
  numbers <- strsplit(x, "[ /]")
  
  ifelse(is.integer,  as.numeric(sapply(numbers, `[`, 1)),
         ifelse(is.fraction, as.numeric(sapply(numbers, `[`, 1)) /
                  as.numeric(sapply(numbers, `[`, 2)),
                as.numeric(sapply(numbers, `[`, 1)) +
                  as.numeric(sapply(numbers, `[`, 2)) /
                  as.numeric(sapply(numbers, `[`, 3))))
}

recipes_matched %>%
  unnest(Match, keep_empty = T) %>%
  group_by(Name) %>%
  filter(!any(is.na(Match))) %>%
  mutate(Name=unlist(Name)) %>%
  select(Name, Servings, OrigIngredient, Match, Qty, UOM) %>%
  mutate(
    Qty = mixedToFloat(Qty),
    Qty = if_else(is.na(Qty), 1, Qty),
    SI_Qty = if_else(str_detect(UOM, "tablespoon"), 14.78672 * Qty, NA_real_),
    SI_Qty = if_else(str_detect(UOM, "teaspoon"), 4.928906 * Qty, SI_Qty),
    SI_Qty = if_else(str_detect(UOM, "cup"), 236.5875 * Qty, SI_Qty),
    SI_Qty = if_else(str_detect(UOM, "pound"), 453.5924 * Qty, SI_Qty),
    SI_Qty = if_else(str_detect(UOM, "ounce"), 28.34952 * Qty, SI_Qty),
    SI_Qty = if_else(str_detect(UOM, "dash|pinch"), 1.0 * Qty, SI_Qty),
    SI_Qty = if_else(str_detect(UOM, "each|can|package|slice"), as.numeric(Qty), SI_Qty),
    SI_UOM = if_else(str_detect(UOM, "tablespoon|teaspoon|cup|pound|ounce|dash|pinch"), "g", UOM),
    SI_UOM = if_else(str_detect(UOM, "each|can|package|slice"), "each", SI_UOM)
  ) %>%
  mutate(
    SI_Qty_Per_Serving = SI_Qty / as.numeric(Servings)
  ) %>%
  select(Name:UOM, SI_Qty_Per_Serving, SI_UOM) %>%
  write_csv("recipes_constr.csv")


# Export price list -------------------------------------------------------

daily_table_prices %>%
  distinct(Units)

daily_table_prices %>%
  mutate(Qty = if_else(is.na(Qty), 1, Qty)) %>%
  mutate(SI_Qty = if_else(str_detect(Units, "lb"), 453.5924 * Qty, NA_real_),
         SI_Qty = if_else(str_detect(Units, "each|ct"), 1.0 * Qty, SI_Qty),
         SI_Qty = if_else(str_detect(Units, "oz"), 28.34952 * Qty, SI_Qty),
         SI_Qty = if_else(str_detect(Units, "gal"), 3785.4 * Qty, SI_Qty),
         SI_Qty = if_else(str_detect(Units, "fl oz"), 28.41306 * Qty, SI_Qty),
         SI_UOM = if_else(str_detect(Units, "lb|oz|gal|fl oz"), "g", "each")
         ) %>%
  write_csv("price_ing.csv")


# Recipe nutritional info -------------------------------------------------

recipes_matched %>%
  unnest(Match, keep_empty = T) %>%
  group_by(Name) %>%
  filter(!any(is.na(Match))) %>%
  mutate(Name=unlist(Name)) %>%
  distinct(Name, NutritionalInfo) %>%
  unnest(NutritionalInfo) %>%
  filter(Nutrients!="Nutrients", Nutrients!="Added Sugars included", Nutrients!="Minerals", Nutrients!="Vitamins") %>%
  ungroup() %>%
  distinct() %>%
  mutate(
    Qty = str_extract(Amount, "\\d+.?\\d*"),
    UOM = str_extract(Amount, "mg|mcg|g"),
    UOM = if_else(str_detect(Nutrients, "Calories"), "kcal", UOM),
    Qty = as.numeric(Qty),
    Qty = if_else(str_detect(Amount, "N/A"), 0, Qty),
    UOM = if_else(is.na(UOM), "mg", UOM), # Some are missing units, all appear to be mg,
    Qty = if_else(is.na(Qty), 0, Qty)
  ) %>%
  mutate(
    Nutrients = paste0(Nutrients, " (", UOM, ")"),
    Qty = if_else(
      Nutrients=="Copper (mg)", Qty*1000, Qty
    ),
    Qty = if_else(
      Nutrients=="Iron (g)", Qty*1000, Qty
    ),
    Qty = if_else(
      Nutrients=="Phosphorus (mcg)", Qty/1000, Qty
    ),
    Qty = if_else(
      Nutrients=="Selenium (mg)", Qty*1000, Qty
    ),
    Qty = if_else(
      Nutrients=="Vitamin B12 (mg)", Qty*1000, Qty
    ),
    Qty = if_else(
      Nutrients=="Vitamin D (mg)", Qty*1000, Qty
    ),
    Qty = if_else(
      Nutrients=="Zinc (mcg)", Qty/1000, Qty
    ),
    Nutrients = str_replace(Nutrients, 
                            c("Copper (mg)", "Iron (g)", "Phosphorus (mcg)", "Selenium (mg)", "Vitamin B12 (mg)", "Vitamin D (mg)", "Zinc (mcg)"), 
                            c("Copper (mcg)", "Iron (mg)", "Phosphorus (mg)", "Selenium (mcg)", "Vitamin B12 (mcg)", "Vitamin D (mcg)", "Zinc (mg)")
                            )
  ) %>%
  select(Name, Nutrients, Qty) %>%
  pivot_wider(names_from = Nutrients, values_from = Qty, values_fn = max, values_fill = 0) %>%
  write_csv("data_recipes.csv")

# Get list of unique nutrients
recipes_matched %>%
  unnest(Match, keep_empty = T) %>%
  group_by(Name) %>%
  filter(!any(is.na(Match))) %>%
  mutate(Name=unlist(Name)) %>%
  distinct(Name, NutritionalInfo) %>%
  unnest(NutritionalInfo) %>%
  filter(Nutrients!="Nutrients", Nutrients!="Added Sugars included", Nutrients!="Minerals", Nutrients!="Vitamins") %>%
  ungroup() %>%
  distinct() %>%
  mutate(
    Qty = str_extract(Amount, "\\d+.?\\d*"),
    UOM = str_extract(Amount, "mg|mcg|g"),
    UOM = if_else(str_detect(Nutrients, "Calories"), "kcal", UOM),
    Qty = as.numeric(Qty),
    Qty = if_else(str_detect(Amount, "N/A"), 0, Qty),
    UOM = if_else(is.na(UOM), "mg", UOM), # Some are missing units, all appear to be mg,
    Qty = if_else(is.na(Qty), 0, Qty)
  ) %>%
  mutate(
    Nutrients = paste0(Nutrients, " (", UOM, ")")
  ) %>%
  select(Nutrients) %>%
  distinct() %>%
  write_csv("nutrients.csv")
