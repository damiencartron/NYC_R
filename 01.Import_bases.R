# Ce programme sert à importer tous les onglets du fichier excel des districts. Ce fichier n'a pas besoin de tourner à chaque fois puisqu'il écrit une base qui est appelée dans 1 et T01. 
# TO DO : 
# OK- En fait il faut conserver toutes les variables (et ne pas sélectionner comme j'ai fait ; ça ne sert à rien et on va avoir besoin des variables)
# OK - Pour les quartiles il faut les calculer par année pour que ça ait du sens ! (là c'est toutes les années mélangées) 
# OK - faire une variable de quartile du MSC  pour All 
# OK - faire une variable de quartile du score au test MSC inter la caractéristique du fichier (par exemple, savoir dans quel quartile de réussite se trouvent les enfants Black du 3ème district 3Grade)
# OK - supprimer la ligne "All Grades" qui doit mettre le bazar dans le calcul des quartiles 


library(readxl)
library(arrow)
library(janitor)
library(tidyverse)
library(gtsummary)
library(gt)

# Import de la base district ----
## Import onglet All ----

All <- read_excel("data/district-ela-results-2018-2025-public.xlsx", 
                  sheet = "ELA - All") |> 
  clean_names() |> 
  filter(grade != "All Grades") |> 
  rename(
    MSC = "mean_scale_score", 
    level_34 = number_level_3_4,
    pct_level_34 = percent_level_3_4)  |> 
  rename_with(~ sub("^number_level","level", .x), starts_with("number_")) |> # renommage pour supprimer le remplacement de # par number_   
  rename_with(~ str_replace(.x, "^percent", "pct"), starts_with("percent_")) |> # renommage de percent en pct pour le début des variables 
  rename_with(~ str_glue("All_{.x}"), .cols = c(number_tested:pct_level_34)) |>  # renommage pour ajouter un préfixe All_ à toutes les variables qui varient d'un fichier à l'autre 
  mutate(
    QAll_MSC = str_glue("Q{ntile(All_MSC, 4)}"),
    QAll_34  = str_glue("Q{ntile(All_pct_level_34, 4)}"),
    across(starts_with("All_pct_level_"), \(x) round(x, 1)), 
    .by = year
  )  

names(All)

## Import onglet SWD-----

SWD <- read_excel("data/district-ela-results-2018-2025-public.xlsx", 
                  sheet = "ELA - SWD") |> 
  clean_names() |> 
  filter(grade != "All Grades") |> 
  rename(
    MSC = "mean_scale_score", 
    level_34 = number_level_3_4,
    pct_level_34 = percent_level_3_4) |> 
  rename_with(~ sub("^number_level","level", .x), starts_with("number_")) |>
  rename_with(~ str_replace(.x, "^percent", "pct"), starts_with("percent_")) |> 
  mutate(
    category = if_else(category == "Not SWD", "not_SWD", category), 
    MSC = round(MSC, 2)
    ) |> 
  mutate(
    QMSC = str_glue("Q{ntile(MSC, 4)}"), # Calcul du quantile entre les SWD et les pas SWD avant le pivot_wider
    QMSC = if_else(QMSC == "QNA", NA_character_, QMSC),
    .by = c(year, grade)
  ) |> 
  pivot_wider(
    id_cols = c(district, grade, year), 
    names_from = category, 
    names_glue = "{category}_{.value}",
    values_from = !c(district, grade, year, category)
  ) |> 
  mutate(
    txSWD = round(SWD_number_tested / (not_SWD_number_tested + SWD_number_tested) * 100, 1)) |> 
  mutate(
    QSWD = str_glue("Q{ntile(txSWD, 4)}"), 
    QSWD_34 = str_glue("Q{ntile(SWD_pct_level_34, 4)}"), 
    QSWD_MSC = str_glue("Q{ntile(SWD_MSC, 4)}"),
    across(contains("SWD_pct_level_"), \(x) round(x, 1)), 
    .by = year
  )

names(SWD)

## Import onglet Ethnicity --------

Ethnicity <- read_excel("data/district-ela-results-2018-2025-public.xlsx", 
                        sheet = "ELA - Ethnicity") |> 
  clean_names() |> 
  filter(grade != "All Grades") |> 
  rename(
    MSC = "mean_scale_score",
    level_34 = number_level_3_4,
    pct_level_34 = percent_level_3_4) |> 
  rename_with(~ sub("^number_level","level", .x), starts_with("number_")) |> 
  rename_with(~ str_replace(.x, "^percent", "pct"), starts_with("percent_")) |> 
  mutate(
    category = case_when(
      category == "Multi-Racial" ~"Multi", 
      category == "Native American" ~"Natives", 
      .default = category
    ), 
    across(where(is.character), ~ na_if(.x, "s")),  # je vire les "s" du non significatif ; remplacé par des NA
    MSC = round(as.numeric(MSC), 2),
  ) |> 
  mutate(
    QMSC = str_glue("Q{ntile(MSC, 4)}"), # Calcul du quantile inter-race avant le pivot_wider
    QMSC = if_else(QMSC == "QNA", NA_character_, QMSC),
    .by = c(year, grade)
  ) |> 
  pivot_wider(
    id_cols = c(district, grade, year), 
    names_from = category, 
    names_glue = "{category}_{.value}",
    values_from = !c(district, grade, year, category)
  )|> 
  mutate(
    tested = Asian_number_tested + Black_number_tested + Hispanic_number_tested + Multi_number_tested + Natives_number_tested + White_number_tested, 
    txAsian = round(Asian_number_tested / tested * 100, 1), 
    txBlack = round(Black_number_tested / tested * 100, 1),
    txHispanic = round(Hispanic_number_tested / tested * 100, 1),
    txMulti = round(Multi_number_tested / tested * 100, 1), 
    txNatives = round(Natives_number_tested / tested * 100, 1), 
    txWhite = round(White_number_tested / tested * 100, 1), 
    txNonWhite = 100 - txWhite, 
    QNonWhite = str_glue("Q{ntile(txNonWhite, 4)}"), 
    across(contains("_pct_level_"), \(x) as.numeric(x)),
    across(contains("_pct_level_"), \(x) round(x, 1)),
  ) |> 
  select(!tested) |> 
  mutate(
    QBlack = str_glue("Q{ntile(txBlack, 4)}"), 
    QBlack_34 = str_glue("Q{ntile(Black_pct_level_34, 4)}"), 
    QHispanic = str_glue("Q{ntile(txHispanic, 4)}"),
    QHispanic_34 = str_glue("Q{ntile(Hispanic_pct_level_34, 4)}"),
    QAsian = str_glue("Q{ntile(txAsian, 4)}"),
    QAsian_34 = str_glue("Q{ntile(Asian_pct_level_34, 4)}"),
    QWhite  = str_glue("Q{ntile(txWhite, 4)}"),
    QWhite_34  = str_glue("Q{ntile(White_pct_level_34, 4)}"),
    .by = year
  )

names(Ethnicity)
# Ethnicity |> select(starts_with("Black")) |> names()
# Ethnicity |> select(contains("_pct_level_")) |> names()
# Ethnicity |> select(contains("_pct_level_")) |> str()


## Import onglet Gender-----

Gender <- read_excel("data/district-ela-results-2018-2025-public.xlsx", 
                        sheet = "ELA - Gender") |> 
  clean_names() |> 
  filter(grade != "All Grades") |> 
  rename(
    MSC = "mean_scale_score", 
    level_34 = number_level_3_4,
    pct_level_34 = percent_level_3_4) |> 
  rename_with(~ sub("^number_level","level", .x), starts_with("number_")) |> 
  rename_with(~ str_replace(.x, "^percent", "pct"), starts_with("percent_")) |> 
  mutate(
    category = if_else(category == "Neither Female nor Male", "NonBinaire", category),
    across(where(is.character), ~ na_if(.x, "s")),  # je vire les "s" du non significatif ; remplacé par des NA
    MSC = round(as.numeric(MSC), 2)
    ) |> 
  mutate(
    QMSC = str_glue("Q{ntile(MSC, 4)}"), # Calcul du quantile inter-gender avant le pivot_wider
    QMSC = if_else(QMSC == "QNA", NA_character_, QMSC),
    .by = c(year, grade)
  ) |> 
  pivot_wider(
    id_cols = c(district, grade, year), 
    names_from = category, 
    names_glue = "{category}_{.value}",
    values_from = !c(district, grade, year, category)
  ) |> 
  rowwise() |> # un rowwise pour le pourcentage d'hommes et de femmes par district (pour vérifier s'il y a des districts d'hommes comme de blancs)
  mutate(
    txFemale = round(Female_number_tested / sum(Female_number_tested, Male_number_tested, NonBinaire_number_tested, na.rm = TRUE) * 100, 1) , 
    txMale  = round(Male_number_tested / sum(Female_number_tested, Male_number_tested, NonBinaire_number_tested, na.rm = TRUE) * 100, 1) ,
    txNBinaire = round(NonBinaire_number_tested / sum(Female_number_tested, Male_number_tested, NonBinaire_number_tested, na.rm = TRUE) * 100, 1)) |> 
  ungroup() |> 
  mutate(
    QFemale = str_glue("Q{ntile(txFemale, 4)}"), # là c'est vraiment le quartile du pourcentage de femme par district que je calcule 
    across(contains("_pct_level_"), \(x) as.numeric(x)),
    across(contains("_pct_level_"), \(x) round(x, 1)),
    .by = year
  ) |> # je fais le choix de laisser des NA pour les non binaires lorsqu'il n'y en a pas ; et des 0 lorsqu'il y en a mais qu'ils ne sont pas calculables
  ungroup()


names(Gender)
# str(Gender)

## Import onglet Statut économique ----------
## rem : la variable QMSC est faite sur des quantiles pondérés mais après test, il n'y a aucun écart de résultat entre les deux variables ; la pondération était-elle inutile ?  

EconomicStatus <- read_excel("data/district-ela-results-2018-2025-public.xlsx", 
                             sheet = "ELA - Econ Status") |> 
  clean_names() |> 
  filter(grade != "All Grades") |> 
  rename(
    MSC = mean_scale_score, 
    level_34 = number_level_3_4,
    pct_level_34 = percent_level_3_4) |> 
  rename_with(~ sub("^number_level","level", .x), starts_with("number_")) |> 
  rename_with(~ str_replace(.x, "^percent", "pct"), starts_with("percent_")) |> 
  mutate(
    category = case_when(
      category == "Econ Disadv" ~"Pauvres", 
      category == "Not Econ Disadv" ~"NonPauvres",
      .default = category
    ),
    MSC = round(as.numeric(MSC), 2)) |> 
  group_by(year, grade) |> 
  mutate(
    # Calculer les seuils des quartiles pondérés
    Q1 = Hmisc::wtd.quantile(MSC, weights = number_tested, probs = 0.25, na.rm = TRUE)[1],
    Q2 = Hmisc::wtd.quantile(MSC, weights = number_tested, probs = 0.50, na.rm = TRUE)[1],
    Q3 = Hmisc::wtd.quantile(MSC, weights = number_tested, probs = 0.75, na.rm = TRUE)[1],
    
    # Assigner chaque observation à son quartile
    QMSC = case_when(
      is.na(MSC) ~ NA_character_,
      MSC <= Q1 ~ "Q1",
      MSC <= Q2 ~ "Q2",
      MSC <= Q3 ~ "Q3",
      MSC >  Q3 ~ "Q4"),
      
    QMSC2 = str_glue("Q{ntile(MSC, 4)}"), # Calcul du quantile inter-statuts économiques avant le pivot_wider
    QMSC2 = if_else(QMSC == "QNA", NA_character_, QMSC),

    ) |> 
      ungroup() |> 
  pivot_wider(
    id_cols = c(district, grade, year), 
    names_from = category, 
    names_glue = "{category}_{.value}",
    values_from = !c(district, grade, year, category)
  ) |> 
  mutate(
    txPauvres = round(Pauvres_number_tested  /(Pauvres_number_tested + NonPauvres_number_tested) * 100, 1),
    QPauvres = str_glue("Q{ntile(txPauvres, 4)}"), 
    across(contains("_pct_level_"), \(x) as.numeric(x)),
    across(contains("_pct_level_"), \(x) round(x, 1)),
    .by = year
  )
  

names(EconomicStatus)
# table(EconomicStatus$QPauvres)

## Import onglet ELL-----

ELL <- read_excel("data/district-ela-results-2018-2025-public.xlsx", 
                  sheet = "ELA - ELL", 
                  col_types = "text") |>  # les colonnes étaient vues comme numériques alors qu'il y avait des "s" dans les lignes 1600 ce qui mettait le bazar ; j'importe tout en texte
  clean_names() |> 
  filter(grade != "All Grades") |> 
  rename(
    MSC = mean_scale_score, 
    level_34 = number_level_3_4,
    pct_level_34 = percent_level_3_4) |> 
  rename_with(~ sub("^number_level","level", .x), starts_with("number_")) |> 
  rename_with(~ str_replace(.x, "^percent", "pct"), starts_with("percent_")) |> 
  mutate(
    category = case_when(
      category == "Current ELL" ~"CurrentELL", 
      category == "Ever ELL" ~"EverELL",
      category == "Never ELL" ~"NeverELL",
      .default = category
    ), 
    across(where(is.character), ~ na_if(.x, "s")), 
    across(c(year, number_tested, MSC, level_1, level_2, level_3, level_4), as.numeric), # je remets les colonnes en numérique
    MSC = round(as.numeric(MSC), 2)) |> 
  mutate(
    QMSC = str_glue("Q{ntile(MSC, 4)}"), # Calcul du quantile inter-ELL avant le pivot_wider
    QMSC = if_else(QMSC == "QNA", NA_character_, QMSC),
    .by = c(year, grade)
  ) |> 
  pivot_wider(
    id_cols = c(district, grade, year), 
    names_from = category, 
    names_glue = "{category}_{.value}",
    values_from = !c(district, grade, year, category)
  ) |> 
  mutate(
    txCurrentELL = round(CurrentELL_number_tested / (CurrentELL_number_tested + EverELL_number_tested + NeverELL_number_tested) * 100, 1), 
    txEverELL = round(EverELL_number_tested / (CurrentELL_number_tested + EverELL_number_tested + NeverELL_number_tested) * 100, 1), 
    txNeverELL = round(NeverELL_number_tested / (CurrentELL_number_tested + EverELL_number_tested + NeverELL_number_tested) * 100, 1), 
    QNonNativeSpeaker = str_glue("Q{ntile(100-txNeverELL, 4)}"), 
    across(contains("_pct_level_"), \(x) as.numeric(x)),
    across(contains("_pct_level_"), \(x) round(x, 1)),
    .by = year
  )


names(ELL)
# str(ELL)


## Merge des différents onglets + ajouts des variables communes--------

district <- All |> 
  left_join(SWD, by = c("district", "grade", "year")) |> 
  left_join(Ethnicity, by = c("district", "grade", "year")) |> 
  left_join(Gender, by = c("district", "grade", "year")) |> 
  left_join(EconomicStatus, by = c("district", "grade", "year")) |> 
  left_join(ELL, by = c("district", "grade", "year")) |> 
  mutate(
    across(where(is.character), ~ na_if(.x, "s")),
    # across(where(is.factor),    ~ factor(na_if(as.character(.x), "s"))), # je supprime les "s" lorsque les cases n'étaient pas significatives 
    phase = case_when(
      district %in% c("05", "11", "12", "14", "16", "19", "20", "21", "22", "23", "25", "26",  "29", "30" , "32") ~"1", 
      district %in% c("01", "02", "03", "04", "06", "07", "08", "09", "10", "13", "15", "17", "18", "24", "27", "28", "31") ~"2", 
      .default = str_glue("{district}_pb")), 
    
    phase = factor(phase, 
                          levels = c(1, 2), 
                          labels = c("phase_1", "phase_2")),
    
    book = case_when(
      district %in% c("04", "05", "08", "09", "10", "12", "14",  "16", "17",  "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32") ~"into", 
      district %in% c("02", "03", "15", "18", "19") ~"wit",
      district %in% c("01",  "06", "07", "11", "13") ~"ELE", 
      .default = str_glue("{district}_pb")), 
    
    borough = case_when(
      district %in% c("01", "02", "03", "04", "05", "06") ~"Manhattan", 
      district %in% c("07", "08", "09", "10", "11", "12") ~"Bronx", 
      district %in% c("13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "32") ~"Brooklyn",
      district %in% c("24", "25", "26", "27", "28", "29", "30") ~"Queens", 
      district %in% c("31") ~"Staten",
      .default = str_glue("{district}_pb")) , 
    
    borough = factor(borough, 
                     levels = c("Manhattan", "Brooklyn", "Staten", "Queens", "Bronx")),

    across(
      where(\(x) is.character(x) & !inherits(x, "glue")) & 
        !c(district, grade, book, borough, starts_with("Q")),
      as.numeric
    )  # je remets toutes les variables qui devraient être en numérique en numérique (mais pas toutes celles que j'ai créées en Q1, Q2, etc. )
  ) |> 
  relocate(phase, book, borough, .after = year)

## Export en parquet ----    
write_parquet(district, "data/district.parquet")

## Suppression des fichiers temporaires ----
rm(All)
rm(EconomicStatus)
rm(ELL)
rm(Gender)
rm(SWD)
rm(Ethnicity)


# Import de la base school-----
# Je vais essayer de récupérer au maximum le script de district et de l'appliquer à school qui est en fait très proche (deux variables en plus et bcp de lignes supplémentaires)

## Import onglet All (school)----

All_s <- read_excel("data/school-ela-results-2018-2025-public.xlsx", 
                    sheet = "ELA - All") |> 
  clean_names() |> 
  filter(grade != "All Grades") |> 
  rename(
    MSC = "mean_scale_score", 
    level_34 = number_level_3_4,
    pct_level_34 = percent_level_3_4)  |> 
  rename_with(~ sub("^number_level","level", .x), starts_with("number_")) |> # renommage pour supprimer le remplacement de # par number_   
  rename_with(~ str_replace(.x, "^percent", "pct"), starts_with("percent_")) |> # renommage de percent en pct pour le début des variables 
  rename_with(~ str_glue("All_{.x}"), .cols = c(number_tested:pct_level_34)) |>  # renommage pour ajouter un préfixe All_ à toutes les variables qui varient d'un fichier à l'autre 
  mutate(
    across(where(is.character), ~ na_if(.x, "s")),     # je vire les "s" du non significatif ; remplacé par des NA
    across(
      where(\(x) is.character(x)) & 
        !c(dbn, school_name, grade, category),
      as.numeric
    )  
  ) |> 
  mutate(
    QAll_MSC = str_glue("Q{ntile(All_MSC, 4)}"),
    QAll_MSC = if_else(QAll_MSC == "QNA", NA_character_, QAll_MSC),
    QAll_34  = str_glue("Q{ntile(All_pct_level_34, 4)}"),
    QAll_34  = if_else(QAll_34 == "QNA", NA_character_, QAll_34),
    across(starts_with("All_pct_level_"), \(x) round(x, 1)), 
    .by = year
  )  

names(All_s)
# str(All_s)
# table(All_s$QAll_34)


## Import onglet SWD (school) -----

SWD_s <- read_excel("data/school-ela-results-2018-2025-public.xlsx", 
                  sheet = "ELA - SWD") |> 
  clean_names() |> 
  filter(grade != "All Grades") |> 
  rename(
    MSC = "mean_scale_score", 
    level_34 = number_level_3_4,
    pct_level_34 = percent_level_3_4) |> 
  rename_with(~ sub("^number_level","level", .x), starts_with("number_")) |>
  rename_with(~ str_replace(.x, "^percent", "pct"), starts_with("percent_")) |> 
  mutate(
    across(where(is.character), ~ na_if(.x, "s")),     # je vire les "s" du non significatif ; remplacé par des NA
    across(
      where(\(x) is.character(x)) & 
        !c(dbn, school_name, grade, category),
      as.numeric),
  
    category = if_else(category == "Not SWD", "not_SWD", category), 
    MSC = round(MSC, 2)
  ) |> 
  mutate(
    QMSC = str_glue("Q{ntile(MSC, 4)}"), # Calcul du quantile entre les SWD et les pas SWD avant le pivot_wider
    QMSC = if_else(QMSC == "QNA", NA_character_, QMSC),
    .by = c(year, grade)
  ) |> 
  pivot_wider(
    id_cols = c(dbn, school_name, grade, year), 
    names_from = category, 
    names_glue = "{category}_{.value}",
    values_from = !c(dbn, school_name, grade, year, category)
  ) |> 
  mutate(
    txSWD = round(SWD_number_tested / (not_SWD_number_tested + SWD_number_tested) * 100, 1)) |> 
  mutate(
    QSWD = str_glue("Q{ntile(txSWD, 4)}"), 
    QSWD = if_else(QSWD == "QNA", NA_character_, QSWD),
    QSWD_34 = str_glue("Q{ntile(SWD_pct_level_34, 4)}"), 
    QSWD_34 = if_else(QSWD_34 == "QNA", NA_character_, QSWD_34),
    QSWD_MSC = str_glue("Q{ntile(SWD_MSC, 4)}"),
    QSWD_MSC = if_else(QSWD_MSC == "QNA", NA_character_, QSWD_MSC),
    
    across(contains("SWD_pct_level_"), \(x) round(x, 1)), 
    .by = year
  )

names(SWD_s)
# str(SWD_s)
# table(SWD_s$QSWD)

## Import onglet Ethnicity (school) --------

Ethnicity_s <- read_excel("data/school-ela-results-2018-2025-public.xlsx", 
                        sheet = "ELA - Ethnicity") |> 
  clean_names() |> 
  filter(grade != "All Grades") |> 
  rename(
    MSC = "mean_scale_score",
    level_34 = number_level_3_4,
    pct_level_34 = percent_level_3_4) |> 
  rename_with(~ sub("^number_level","level", .x), starts_with("number_")) |> 
  rename_with(~ str_replace(.x, "^percent", "pct"), starts_with("percent_")) |> 
  mutate(
    category = case_when(
      category == "Multi-Racial" ~"Multi", 
      category == "Native American" ~"Natives", 
      .default = category
    ), 
    across(where(is.character), ~ na_if(.x, "s")),  # je vire les "s" du non significatif ; remplacé par des NA
    MSC = round(as.numeric(MSC), 2),
  ) |> 
  mutate(
    QMSC = str_glue("Q{ntile(MSC, 4)}"), # Calcul du quantile inter-race avant le pivot_wider
    QMSC = if_else(QMSC == "QNA", NA_character_, QMSC),
    .by = c(year, grade)
  ) |> 
  pivot_wider(
    id_cols = c(dbn, school_name, grade, year), 
    names_from = category, 
    names_glue = "{category}_{.value}",
    values_from = !c(dbn, school_name, grade, year, category)
  )|> 
  mutate(
    tested = Asian_number_tested + Black_number_tested + Hispanic_number_tested + Multi_number_tested + Natives_number_tested + White_number_tested, 
    txAsian = round(Asian_number_tested / tested * 100, 1), 
    txBlack = round(Black_number_tested / tested * 100, 1),
    txHispanic = round(Hispanic_number_tested / tested * 100, 1),
    txMulti = round(Multi_number_tested / tested * 100, 1), 
    txNatives = round(Natives_number_tested / tested * 100, 1), 
    txWhite = round(White_number_tested / tested * 100, 1), 
    txNonWhite = 100 - txWhite, 
    QNonWhite = str_glue("Q{ntile(txNonWhite, 4)}"), 
    across(contains("_pct_level_"), \(x) as.numeric(x)),
    across(contains("_pct_level_"), \(x) round(x, 1)),
  ) |> 
  select(!tested) |> 
  mutate(
    QBlack = str_glue("Q{ntile(txBlack, 4)}"), 
    QBlack_34 = str_glue("Q{ntile(Black_pct_level_34, 4)}"), 
    QHispanic = str_glue("Q{ntile(txHispanic, 4)}"),
    QHispanic_34 = str_glue("Q{ntile(Hispanic_pct_level_34, 4)}"),
    QAsian = str_glue("Q{ntile(txAsian, 4)}"),
    QAsian_34 = str_glue("Q{ntile(Asian_pct_level_34, 4)}"),
    QWhite  = str_glue("Q{ntile(txWhite, 4)}"),
    QWhite_34  = str_glue("Q{ntile(White_pct_level_34, 4)}"),
    .by = year
  )

names(Ethnicity_s)
# Ethnicity_s |> select(starts_with("Black")) |> names()
# Ethnicity_s |> select(contains("_pct_level_")) |> names()
# Ethnicity_s |> select(contains("_pct_level_")) |> str()

# REM : il y a deux lignes de moins à Ethnicity qu'aux autres fichiers ? ?  ?

## Import onglet Gender (school)-----

Gender_s <- read_excel("data/school-ela-results-2018-2025-public.xlsx", 
                     sheet = "ELA - Gender") |> 
  clean_names() |> 
  filter(grade != "All Grades") |> 
  rename(
    MSC = "mean_scale_score", 
    level_34 = number_level_3_4,
    pct_level_34 = percent_level_3_4) |> 
  rename_with(~ sub("^number_level","level", .x), starts_with("number_")) |> 
  rename_with(~ str_replace(.x, "^percent", "pct"), starts_with("percent_")) |> 
  mutate(
    category = if_else(category == "Neither Female nor Male", "NonBinaire", category),
    across(where(is.character), ~ na_if(.x, "s")),  # je vire les "s" du non significatif ; remplacé par des NA
    MSC = round(as.numeric(MSC), 2)
  ) |> 
  mutate(
    QMSC = str_glue("Q{ntile(MSC, 4)}"), # Calcul du quantile inter-gender avant le pivot_wider
    QMSC = if_else(QMSC == "QNA", NA_character_, QMSC),
    .by = c(year, grade)
  ) |> 
  pivot_wider(
    id_cols = c(dbn, school_name, grade, year), 
    names_from = category, 
    names_glue = "{category}_{.value}",
    values_from = !c(dbn, school_name, grade, year, category)
  ) |> 
  rowwise() |> # un rowwise pour le pourcentage d'hommes et de femmes par district (pour vérifier s'il y a des districts d'hommes comme de blancs)
  mutate(
    txFemale = round(Female_number_tested / sum(Female_number_tested, Male_number_tested, NonBinaire_number_tested, na.rm = TRUE) * 100, 1) , 
    txMale  = round(Male_number_tested / sum(Female_number_tested, Male_number_tested, NonBinaire_number_tested, na.rm = TRUE) * 100, 1) ,
    txNBinaire = round(NonBinaire_number_tested / sum(Female_number_tested, Male_number_tested, NonBinaire_number_tested, na.rm = TRUE) * 100, 1)) |> 
  ungroup() |> 
  mutate(
    QFemale = str_glue("Q{ntile(txFemale, 4)}"), # là c'est vraiment le quartile du pourcentage de femme par district que je calcule 
    across(contains("_pct_level_"), \(x) as.numeric(x)),
    across(contains("_pct_level_"), \(x) round(x, 1)),
    .by = year
  ) |> # je fais le choix de laisser des NA pour les non binaires lorsqu'il n'y en a pas ; et des 0 lorsqu'il y en a mais qu'ils ne sont pas calculables
  ungroup()


names(Gender_s)
# str(Gender)

## Import onglet Statut économique (school) ----------
## rem : la variable QMSC est faite sur des quantiles pondérés mais après test, il n'y a aucun écart de résultat entre les deux variables ; la pondération était-elle inutile ?  

EconomicStatus_s <- read_excel("data/school-ela-results-2018-2025-public.xlsx", 
                             sheet = "ELA - Econ Status") |> 
  clean_names() |> 
  filter(grade != "All Grades") |> 
  rename(
    MSC = mean_scale_score, 
    level_34 = number_level_3_4,
    pct_level_34 = percent_level_3_4) |> 
  rename_with(~ sub("^number_level","level", .x), starts_with("number_")) |> 
  rename_with(~ str_replace(.x, "^percent", "pct"), starts_with("percent_")) |> 
  mutate(
    category = case_when(
      category == "Econ Disadv" ~"Pauvres", 
      category == "Not Econ Disadv" ~"NonPauvres",
      .default = category
    ),
    across(where(is.character), ~ na_if(.x, "s")),  # je vire les "s" du non significatif ; remplacé par des NA
    MSC = round(as.numeric(MSC), 2)) |> 
  group_by(year, grade) |> 
  mutate(
    # Calculer les seuils des quartiles pondérés
    Q1 = Hmisc::wtd.quantile(MSC, weights = number_tested, probs = 0.25, na.rm = TRUE)[1],
    Q2 = Hmisc::wtd.quantile(MSC, weights = number_tested, probs = 0.50, na.rm = TRUE)[1],
    Q3 = Hmisc::wtd.quantile(MSC, weights = number_tested, probs = 0.75, na.rm = TRUE)[1],
    
    # Assigner chaque observation à son quartile
    QMSC = case_when(
      is.na(MSC) ~ NA_character_,
      MSC <= Q1 ~ "Q1",
      MSC <= Q2 ~ "Q2",
      MSC <= Q3 ~ "Q3",
      MSC >  Q3 ~ "Q4"),
    
    QMSC2 = str_glue("Q{ntile(MSC, 4)}"), # Calcul du quantile inter-statuts économiques avant le pivot_wider
    QMSC2 = if_else(QMSC == "QNA", NA_character_, QMSC),
    
  ) |> 
  ungroup() |> 
  pivot_wider(
    id_cols = c(dbn, school_name, grade, year), 
    names_from = category, 
    names_glue = "{category}_{.value}",
    values_from = !c(dbn, school_name, grade, year, category)
  ) |> 
  mutate(
    txPauvres = round(Pauvres_number_tested  /(Pauvres_number_tested + NonPauvres_number_tested) * 100, 1),
    QPauvres = str_glue("Q{ntile(txPauvres, 4)}"), 
    across(contains("_pct_level_"), \(x) as.numeric(x)),
    across(contains("_pct_level_"), \(x) round(x, 1)),
    .by = year
  )


names(EconomicStatus_s)
# str(EconomicStatus_s)
# table(EconomicStatus$QPauvres)


## Import onglet ELL (school)-----

ELL_s <- read_excel("data/school-ela-results-2018-2025-public.xlsx", 
                  sheet = "ELA - ELL", 
                  col_types = "text") |>  # les colonnes étaient vues comme numériques alors qu'il y avait des "s" dans les lignes 1600 ce qui mettait le bazar ; j'importe tout en texte
  clean_names() |> 
  filter(grade != "All Grades") |> 
  rename(
    MSC = mean_scale_score, 
    level_34 = number_level_3_4,
    pct_level_34 = percent_level_3_4) |> 
  rename_with(~ sub("^number_level","level", .x), starts_with("number_")) |> 
  rename_with(~ str_replace(.x, "^percent", "pct"), starts_with("percent_")) |> 
  mutate(
    category = case_when(
      category == "Current ELL" ~"CurrentELL", 
      category == "Ever ELL" ~"EverELL",
      category == "Never ELL" ~"NeverELL",
      .default = category
    ), 
    across(where(is.character), ~ na_if(.x, "s")), 
    across(c(year, number_tested, MSC, level_1, level_2, level_3, level_4), as.numeric), # je remets les colonnes en numérique
    MSC = round(as.numeric(MSC), 2)) |> 
  mutate(
    QMSC = str_glue("Q{ntile(MSC, 4)}"), # Calcul du quantile inter-ELL avant le pivot_wider
    QMSC = if_else(QMSC == "QNA", NA_character_, QMSC),
    .by = c(year, grade)
  ) |> 
  pivot_wider(
    id_cols = c(dbn, school_name, grade, year), 
    names_from = category, 
    names_glue = "{category}_{.value}",
    values_from = !c(dbn, school_name, grade, year, category)
  ) |> 
  mutate(
    txCurrentELL = round(CurrentELL_number_tested / (CurrentELL_number_tested + EverELL_number_tested + NeverELL_number_tested) * 100, 1), 
    txEverELL = round(EverELL_number_tested / (CurrentELL_number_tested + EverELL_number_tested + NeverELL_number_tested) * 100, 1), 
    txNeverELL = round(NeverELL_number_tested / (CurrentELL_number_tested + EverELL_number_tested + NeverELL_number_tested) * 100, 1), 
    QNonNativeSpeaker = str_glue("Q{ntile(100-txNeverELL, 4)}"), 
    across(contains("_pct_level_"), \(x) as.numeric(x)),
    across(contains("_pct_level_"), \(x) round(x, 1)),
    .by = year
  )


names(ELL_s)
str(ELL_s)


## Merge des différents onglets + ajouts des variables communes (school) --------

school <- All_s |> 
  left_join(SWD_s,            by = c("dbn", "school_name", "grade", "year")) |> 
  left_join(Ethnicity_s,      by = c("dbn", "school_name", "grade", "year")) |> 
  left_join(Gender_s,         by = c("dbn", "school_name", "grade", "year")) |> 
  left_join(EconomicStatus_s, by = c("dbn", "school_name", "grade", "year")) |> 
  left_join(ELL_s,            by = c("dbn", "school_name", "grade", "year")) |> 
  mutate(
    district = str_sub(dbn, 1, 2), 
    across(where(is.character), ~ na_if(.x, "s")),
    # across(where(is.factor),    ~ factor(na_if(as.character(.x), "s"))), # je supprime les "s" lorsque les cases n'étaient pas significatives 
    phase = case_when(
      district %in% c("05", "11", "12", "14", "16", "19", "20", "21", "22", "23", "25", "26",  "29", "30" , "32") ~"1", 
      district %in% c("01", "02", "03", "04", "06", "07", "08", "09", "10", "13", "15", "17", "18", "24", "27", "28", "31") ~"2", 
      .default = str_glue("{district}_pb")), 
    
    phase = factor(phase, 
                   levels = c(1, 2), 
                   labels = c("phase_1", "phase_2")),
    
    book = case_when(
      district %in% c("04", "05", "08", "09", "10", "12", "14",  "16", "17",  "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32") ~"into", 
      district %in% c("02", "03", "15", "18", "19") ~"wit",
      district %in% c("01",  "06", "07", "11", "13") ~"ELE", 
      .default = str_glue("{district}_pb")), 
    
    borough = case_when(
      district %in% c("01", "02", "03", "04", "05", "06") ~"Manhattan", 
      district %in% c("07", "08", "09", "10", "11", "12") ~"Bronx", 
      district %in% c("13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "32") ~"Brooklyn",
      district %in% c("24", "25", "26", "27", "28", "29", "30") ~"Queens", 
      district %in% c("31") ~"Staten",
      .default = str_glue("{district}_pb")) , 
    
    borough = factor(borough, 
                     levels = c("Manhattan", "Brooklyn", "Staten", "Queens", "Bronx")),
    
    across(
      where(\(x) is.character(x) & !inherits(x, "glue")) & 
        !c(dbn, school_name, district, grade, book, borough, starts_with("Q")),
      as.numeric
    )  # je remets toutes les variables qui devraient être en numérique en numérique (mais pas toutes celles que j'ai créées en Q1, Q2, etc. )
  ) |> 
  relocate(phase, book, borough, .after = year) |> 
  relocate(district, .before = 1) |> 
  select(!category)

## Export en parquet ----    
write_parquet(school, "data/school.parquet")

## Suppression des fichiers temporaires ----
rm(All_s)
rm(EconomicStatus_s)
rm(ELL_s)
rm(Gender_s)
rm(SWD_s)
rm(Ethnicity_s)