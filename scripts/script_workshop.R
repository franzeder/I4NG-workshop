# 000. Configuration----


# install package manager
install.packages("pacman")

pacman::p_load(MASS, # statistical modeling and machine learning
         tidymodels, # statistical modeling and tidy output
         tidyverse, # manipulating and visualizing data
         showtext, # using non-standard fonts
         Cairo, # embedding fonts into graphs
         sjlabelled, # using SPSS/Stata data
         dataverse # downloading dataset
         )


# 010. loading and manipulating data----

## Specifying the API Token from AUSSDA
Sys.setenv("DATAVERSE_KEY" = "84ea49a4-7d80-4e7f-8f41-1fc073e74055")

df_evs <-
  get_dataframe_by_name(
    filename    = "10048_da_en_v1_0-1.tab",
    dataset     = "10.11587/C4YBOT",
    .f          = haven::read_dta,
    original    = TRUE,
    server      = "data.aussda.at")

# 012. save data to data folder----
write_csv(df_evs, "./data/df_evs.csv")

# 015. select variables----
df <- df_evs |> select(year = S002EVS,
                       sex = X001,
                       homosexuality = F118)

tail(df)
