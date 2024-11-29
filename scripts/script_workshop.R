# 000. Configuration----


# install package manager
install.packages("pacman")

pacman::p_load(MASS, # statistical modeling and machine learning
         tidymodels, # statistical modeling and tidy output
         tidyverse, # manipulating and visualizing data
         showtext, # using non-standard fonts
         Cairo, # embedding fonts into graphs
         sjlabelled, # using SPSS/Stata data
         ggtext, # for coloring title in plots
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

# 020. analysis----

# 021. linear model----

# fitting the model
lm_fit <- lm(homosexuality ~ year + sex, data = df)

# summary of the model
summary(lm_fit)

# tidy output of the model
tidy(lm_fit)


# 022. ordered logistic regression----

# create a new df and mutate var homosexuality to factor var
df1 <- df |> mutate(homosexuality = as_factor(homosexuality))

polr_fit <- polr(homosexuality ~ year + sex, data = df1)

# print in tidy format
tidy(polr_fit, exponentiate = TRUE,
     conf.int = TRUE, p.values = TRUE)

# 030. plots----------

# define font families for plots
# https://fonts.google.com/share?selection.family=Roboto+Condensed:ital,wght@0,100..900;1,100..900|Roboto:ital,wght@0,100;0,300;0,400;0,500;0,700;0,900;1,100;1,300;1,400;1,500;1,700;1,900

fontfamily1 <- "Roboto"
fontfamily2 <- "Roboto Condensed"

# 031. line-plot----

# saving plot with Cairo to embed fonts
png(filename = "./plots/plot_line_homosexuality.png", 
    width = 21.7, 
    height = 10.2, 
    units = "in", 
    res = 300,
    bg = "#ffffff", 
    type = "cairo-png" # embed the font in the file
)

df |> filter(homosexuality == 10 | homosexuality == 1) |> # only take extremes
  pivot_longer(cols = c(homosexuality)) |> # convert to a long table
  group_by(year, value) |> 
  summarise(n = n()) |> # summarise values per year
  mutate(N = max(cumsum(n)), freq = n/N) |> # calculate cumulative sums and frequencies per group
  ggplot(aes(x = as_label(year), y = freq, group = as_label(value), color = as_label(value))) + # use as_label function to get label values
  scale_color_manual(values = c("#005c8b", "#E69F00")) +
  geom_line(linewidth = 2) +
  geom_point(size = 4) +
  geom_point(size = 2, color = "white") +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(x = "", y = "") +
  labs(title = "<b>Austrians have become more tolerant over time</b>") +
  labs(subtitle = "Q: Please tell me whether you think homosexuality can <b><span style = 'color: #E69F00;'>always be justified</span></b>, <b><span style = 'color: #005c8b;'>never be justified</span></b> or something in between.") +
  labs(caption = "Source: European Values Study 1990-2018; Austria Longitudinal Data") +
  theme_minimal() +
  theme(text = element_text(size = 14, family = fontfamily1),
        plot.title = element_text(size = 18, family = fontfamily1), 
        plot.subtitle =  element_markdown(size = 14, family = fontfamily2, margin = ggplot2::margin(1, 0, 1, 0)),
        axis.text.x = element_text(size = 12, family = fontfamily1), 
        axis.text.y = element_text(size = 12, family = fontfamily1), 
        plot.caption = element_text(size = 10, family = fontfamily1, colour = "darkgrey")) +
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        legend.position="none") 

dev.off()



# 032. enhanced dotplot----

png(filename = "./plots/plot_dots_homosexuality-final.png", 
    width = 21.7, 
    height = 10.2, 
    units = "in", 
    res = 300,
    bg = "#ffffff", 
    type = "cairo-png"
)

df |> filter(!is.na(homosexuality)) |> # get rid of NAs
  group_by(sex, year) |> 
  mutate(mean_homosexuality = mean(homosexuality, na.rm = TRUE)) |> # create new variable mean_homosexuality per sex and year
  ggplot(aes(as_label(year), as_label(homosexuality), colour = as_label(sex))) +
  scale_color_manual(values = c("#005c8b", "#E69F00")) +
  geom_jitter(alpha = .3) +
  geom_point(aes(y = mean_homosexuality, color = as_label(sex)), size = 5) +
  geom_point(aes(y = mean_homosexuality), size = 2, color = "white") +
  labs(x = "", y = "") +
  labs(title = "<b>Austrian <span style = 'color: #E69F00;'>women</span> lead the way for <span style = 'color: #005c8b;'>men</span> towards more tolerance</b>") +
  labs(subtitle = "Q: Please tell me whether you think homosexuality can always be justified, never be justified</span> or something in between.") +
  labs(caption = "Source: European Values Study 1990-2018; Austria Longitudinal Data") +
  theme_minimal() +
  theme(text = element_text(size = 14, family = fontfamily1),
        plot.title = element_text(size = 18, family = fontfamily1), 
        plot.subtitle =  element_markdown(size = 14, family = fontfamily2, margin = ggplot2::margin(1, 0, 1, 0)),
        axis.text.x = element_text(size = 12, family = fontfamily1), 
        axis.text.y = element_text(size = 12, family = fontfamily1), 
        plot.caption = element_text(size = 10, family = fontfamily1, colour = "darkgrey")) +
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown(),
        plot.caption = element_markdown(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        legend.position="none") 

dev.off()
