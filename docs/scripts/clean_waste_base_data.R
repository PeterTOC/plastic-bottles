# script to clean combined waste data

# Author: Peter Boshe
# Version: 2022-05-27

# Packages
library(tidyverse)
library(janitor)
library(glue)
library(lubridate)
library(visdat)
library(skimr)


# Parameters
  #input
file_raw <- here::here("data/combined_waste_base_data.rds")

  #output
file_out <- here::here("data/clean_waste_base_data.rds")

number_pattern <- "^\\d\\s?\\.?\\d*"
unit_pattern <- "\\D+$"
water_pattern <- "water|agua|aqua|água|mineral|maji|drop of zanzibar|ice drop|eau de source|amaryllis"
carbonated_pattern <- "twist|vimto gas[ei]ficado|afiya|chungwa|malto|malt flavoured|fursana berry|soft  drink|azam passion|apple malt|apple punch|pepsi|fanta|tonic|fiesta|coke|cola|sparletta|sprite|soda|novida|fizz|kombucha|coca|carbonated|morango soft drink"
juice_pattern <- "juice|fruit drink|orange drink|tropika drink|quencher|blend concentrate|de fruta|tropika drink fruta tropical|delight\\.? mango|tunda|summer fruits|frooty|mala drink|jembe|fruticana|refresco abacaxi|baobab fruit drink|minute maid|dairy fruit blend|ukwaju|sobo drink|agashya|flavou?red drink"
domestic_pattern <- "cleaner|dish|body wash|brake|ice cream|saniti[zs]er|belair|skin|engine oil|perfume|oleo ouro|oleo puro|lubricant|mouthwash|relaxer|sunlight liquid|hygienix|glue|handwash|cleanse|bleach|detergent|shower|lotion|soap|hand gel|wash|oleo refinado"
food_pattern <- "seasoning|shake \\'n|snack|mayonnaise|jam|mchuzi mix|farm fresh|soya oil|red gold|maize|vegetable oil|oleo vegetal|oleo alimentar|tomato sauce|food|butter|margarin?e|maziwa|milk|chocolate|salt|yoghurt|vinegar|seed oil|coconut oil|cooking|sunflower oil|óleo de soja refinado"
hard_liquor_pattern <- "whisky|brandy|fortified wine|alcoholic|gin$|smart gi|k-vant|premium spirit"
beer_pattern <- "beer|castle|heineken|redd's|lager|savanna|chibuku"
energy_drink_pattern <- "energy|mo xtra|lucozade"
carbonated_pattern_2 <- "izy|mirinda|fizz|pepsi|frozy|bigtree|fanta|sprite|fiesta|malt drink|krest"

# REVIEW regex could be optimized further

# ============================================================================

# Code

combined_df <- read_rds(file_raw)

skim(combined_df)

vis_miss(combined_df)

categorized_df <- combined_df |>
  select(batch,
         product_label,
         product_size,
         brand_name,
         manufacturer_country,
         scan_country,
         bottle_count) |>
  mutate_if(is.character,str_to_lower) |>
  # TODO remove the special characters like dashes inbetween words
  mutate(batch = fct_inorder(batch)) |>
  mutate(product_category = case_when(
    str_detect(product_label, water_pattern) ~ "water",
    str_detect(product_label, carbonated_pattern) ~ "carbonated drink",
    str_detect(product_label, juice_pattern) ~ "juice",
    str_detect(product_label, domestic_pattern) ~ "domestic products",
    str_detect(product_label, food_pattern) ~ "food products",
    str_detect(product_label, hard_liquor_pattern) ~ "hard liquor",
    str_detect(product_label, beer_pattern) ~ "beer",
    str_detect(product_label, energy_drink_pattern) ~ "energy drink",
    str_detect(brand_name, "minute maid|ceres") ~ "juice",
    str_detect(brand_name, carbonated_pattern_2) ~ "carbonated drink",
    str_detect(brand_name, "dettol|colgate|sunlight")~ "domestic products",
    str_detect(brand_name,"mistolin|biowash") ~ "domestic products",
    str_detect(product_label, "mango") & str_detect(brand_name, "afya|pride|u-fresh") ~ "juice",
    str_detect(product_label, "lemonade") & str_detect(brand_name, "safari") ~ "juice",
    str_detect(brand_name,"double kick") ~ "hard liquor",
    str_detect(brand_name, "lucozade")~ "energy drink",
    str_detect(brand_name, "brook fresh")~ "energy drink",
    TRUE ~ "unknown"
  )) |>
  select(1,2,8,3,4,5,6,7) |> #filter(product_category == "") |> group_by(product_label,brand_name) |> summarise(total=sum(bottle_count)) |> arrange(desc(total)) |>  view() for further cleaning
# TODO to clean the product label further
  mutate(product_size_extracted = as.numeric(str_extract(product_size, number_pattern))) |>
  mutate(units = str_trim(str_extract(product_size, unit_pattern),
                          side = "both")) |>
  mutate(units = case_when(
    str_detect(units,"litres?|ltrs?") ~ "l",
    str_detect(units, "m") ~ "ml",
    TRUE ~ units
  )) |>
  mutate(amount = case_when(
      units == "l" ~ product_size_extracted * 1000,
      units == "kg" ~ product_size_extracted * 1000,
# REVIEW the amount parsing has a lot of diet supplements allocated as ml instead of grams
# REVIEW the amount parsing has a lot of water items of 8 litres left as they are
      TRUE ~ product_size_extracted
    )) |>
  mutate(units = case_when(
    str_detect(units,"l") ~ "ml",
    str_detect(units,"kg") ~ "g",
    TRUE ~ units
  )) |>
  na.omit(units) |>
  select(batch,
         product_category,
         product_label,
         brand_name,
         manufacturer_country,
         scan_country,
         amount,
         units,
         bottle_count)

## cleaned category count

# 1 beer                121
# 2 carbonated drink   2745
# 3 domestic products   946
# 4 energy drink        404
# 5 food products      1009
# 6 hard liquor         124
# 7 juice              1108
# 8 unknown            1531
# 9 water              2766




skim(categorized_df)
 # TODO cut the capacity amount variable in the beginning
# TODO to find a scaleable way of recoding, probably get the month and year from filename directly

## Recoding categorized_df$batch into categorized_df$batch_rec
categorized_df$batch_rec <- categorized_df$batch %>%
  fct_recode(
    "2021/03" = "1",
    "2021/04" = "2",
    "2021/05" = "3",
    "2021/06" = "4",
    "2021/07" = "5",
    "2021/08" = "6",
    "2021/09" = "7",
    "2021/10" = "8",
    "2021/11" = "9",
    "2021/12" = "10",
    "2022/01" = "11",
    "2022/02" = "12",
    "2022/03" = "13",
    "2022/04" = "14"
  )

#add month and year

categorized_df |>
  separate(batch_rec,
           c("year","month")) |>
   mutate(year = as.factor(year),
          month = as.factor(month),
          batch = as.factor(batch),
          units = as.factor(units),
          manufacturer_country = as.factor(manufacturer_country),
          scan_country = as.factor(scan_country),
          brand_name = as.factor(brand_name),
          product_category = as.factor(product_category)) |>
  select(batch,
         year,
         month,
         product_category,
         product_label,
         brand_name,
         manufacturer_country,
         scan_country,
         amount,
         units,
         bottle_count) |>
  write_rds(file_out)



















