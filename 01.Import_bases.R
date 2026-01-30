library(readxl)
library(arrow)
library(janitor)
library(tidyverse)

# Import Onglet All ----

All <- read_excel("data/district-ela-results-2018-2025-public.xlsx", 
                                                    sheet = "ELA - All") |> 
  clean_names() |> 
  select(!c(starts_with("percent_level_"), "number_level_3_4", "category")) |> # suppression des pourcentages et variables agrégées 
  rename(MSC = "mean_scale_score") |> 
  rename_with(~ sub("^number_level","level", .x), starts_with("number_")) |> # renommage pour supprimer le remplacement de # par number_   
  rename_with(~ str_glue("All_{.x}"), .cols = number_tested:level_4)  # renommage pour ajouter un préfixe All_ à toutes les variables qui varient d'un fichier à l'autre 

names(All)

# Import onglet SWD-----

SWD <- read_excel("data/district-ela-results-2018-2025-public.xlsx", 
                  sheet = "ELA - SWD") |> 
  clean_names() |> 
  select(!c(starts_with("percent_level_"), "number_level_3_4")) |>   
  rename(MSC = "mean_scale_score") |> 
  rename_with(~ sub("^number_level","level", .x), starts_with("number_")) |>
  # rename_with(~ str_glue("SWD_{.x}"), .cols = number_tested:level_4) |> 
  mutate(category = if_else(category == "Not SWD", "not_SWD", category)) |> 
  pivot_wider(
    id_cols = c(district, grade, year), 
    names_from = category, 
    names_glue = "{category}_{.value}",
    # values_from = c( "SWD_number_tested", "SWD_MSC", "SWD_level_1", "SWD_level_2", "SWD_level_3", "SWD_level_4" )
    values_from = c("number_tested", "MSC", "level_1", "level_2", "level_3", "level_4" )
  )

names(SWD)


# Import ongle Ethnicity --------

Ethnicity <- read_excel("data/district-ela-results-2018-2025-public.xlsx", 
                        sheet = "ELA - Ethnicity") |> 
  clean_names() |> 
  select(!c(starts_with("percent_level_"), "number_level_3_4")) |>   
  rename(MSC = "mean_scale_score") |> 
  rename_with(~ sub("^number_level","level", .x), starts_with("number_")) |> 
  mutate(
    category = case_when(
      category == "Multi-Racial" ~"Multi", 
      category == "Native American" ~"Natives", 
      .default = category
    )) |> 
  pivot_wider(
    id_cols = c(district, grade, year), 
    names_from = category, 
    names_glue = "{category}_{.value}",
    values_from = c("number_tested", "MSC", "level_1", "level_2", "level_3", "level_4" )
  )

names(Ethnicity)

# Import onglet Gender-----

Gender <- read_excel("data/district-ela-results-2018-2025-public.xlsx", 
                        sheet = "ELA - Gender") |> 
  clean_names() |> 
  select(!c(starts_with("percent_level_"), "number_level_3_4")) |>   
  rename(MSC = "mean_scale_score") |> 
  rename_with(~ sub("^number_level","level", .x), starts_with("number_")) |> 
  mutate(
    category = if_else(category == "Neither Female nor Male", "NonBinaire", category
    )) |> 
  pivot_wider(
    id_cols = c(district, grade, year), 
    names_from = category, 
    names_glue = "{category}_{.value}",
    values_from = c("number_tested", "MSC", "level_1", "level_2", "level_3", "level_4" )
  )

# Import onglet Statut économique ----------

EconomicStatus <- read_excel("data/district-ela-results-2018-2025-public.xlsx", 
                             sheet = "ELA - Econ Status") |> 
  clean_names() |> 
  select(!c(starts_with("percent_level_"), "number_level_3_4")) |>   
  rename(MSC = "mean_scale_score") |> 
  rename_with(~ sub("^number_level","level", .x), starts_with("number_")) |> 
  mutate(
    category = case_when(
      category == "Econ Disadv" ~"Pauvres", 
      category == "Not Econ Disadv" ~"NonPauvres",
      .default = category
    )) |> 
  pivot_wider(
    id_cols = c(district, grade, year), 
    names_from = category, 
    names_glue = "{category}_{.value}",
    values_from = c("number_tested", "MSC", "level_1", "level_2", "level_3", "level_4" )
  )

names(EconomicStatus)

# Fusion onglet ELL-----

ELL <- read_excel("data/district-ela-results-2018-2025-public.xlsx", 
                             sheet = "ELA - ELL") |> 
  clean_names() |> 
  select(!c(starts_with("percent_level_"), "number_level_3_4")) |>   
  rename(MSC = "mean_scale_score") |> 
  rename_with(~ sub("^number_level","level", .x), starts_with("number_")) |> 
  mutate(
    category = case_when(
      category == "Current ELL" ~"CurrentELL", 
      category == "Ever ELL" ~"EverELL",
      category == "Never ELL" ~"NeverELL",
      .default = category
    )) |> 
  pivot_wider(
    id_cols = c(district, grade, year), 
    names_from = category, 
    names_glue = "{category}_{.value}",
    values_from = c("number_tested", "MSC", "level_1", "level_2", "level_3", "level_4" )
  )

# Merge des différents onglets + ajouts des variables communes--------

district <- All |> 
  left_join(SWD, by = c("district", "grade", "year")) |> 
  left_join(Ethnicity, by = c("district", "grade", "year")) |> 
  left_join(Gender, by = c("district", "grade", "year")) |> 
  left_join(EconomicStatus, by = c("district", "grade", "year")) |> 
  left_join(ELL, by = c("district", "grade", "year")) |> 
  mutate(
    across(where(is.character), ~ na_if(.x, "s")),
    across(where(is.factor),    ~ factor(na_if(as.character(.x), "s"))), # je supprime les "s" lorsque les cases n'étaient pas significatives 
    phase = case_when(
      district %in% c("05", "11", "12", "14", "16", "19", "20", "21", "22", "23", "25", "26",  "29", "30" , "32") ~"1", 
      district %in% c("01", "02", "03", "04", "06", "07", "08", "09", "10", "13", "15", "17", "18", "24", "27", "28", "31") ~"2", 
      .default = str_glue("{district}_pb")), 
    
    book = case_when(
      district %in% c("04", "05", "08", "09", "10", "12", "14", "15", "16", "17",  "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32") ~"into", 
      district %in% c("02", "03", "18", "19") ~"wit",
      district %in% c("01",  "06", "07", "11", "13") ~"ELE", 
      .default = str_glue("{district}_pb")), 
    
    borough = case_when(
      district %in% c("01", "02", "03", "04", "05", "06") ~"MANHATTAN", 
      district %in% c("07", "08", "09", "10", "11", "12") ~"BRONX", 
      district %in% c("13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "32") ~"BROOKLYN",
      district %in% c("24", "25", "26", "27", "28", "29", "30") ~"QUEENS", 
      district %in% c("31") ~"STATEN",
      .default = str_glue("{district}_pb")) , 
    
    across(where(is.character) & !c(district, grade, book, borough), as.numeric)  # je remets toutes les variables qui devraient être en numérique en numérique 
  ) |> 
  relocate(phase, book, borough, .after = year)

# Export en parquet ----    
write_parquet(district, "data/district.parquet")


# quelques vérif -----  
table(district$phase)
table(district$book)
table(district$borough)
str(district)
names(district)
