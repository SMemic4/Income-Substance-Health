library(Hmisc)
library(janitor)
library(naniar)
library(sessioninfo)
library(car)
library(mosaic)
suppressPackageStartupMessages(library(here))
library(gt)
library(glue)
library(patchwork)
library(broom)
library(rstatix)
library(tidyverse)



theme_set(theme_bw())





data_url <- "https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2023_0.csv"

chr_2023_raw <- read_csv(data_url, skip = 1, guess_max = 4000,show_col_types = FALSE)


chr_2023_raw <- chr_2023_raw %>% filter(county_ranked == 1)

dim(chr_2023_raw)



chr_2023 <- chr_2023_raw %>% mutate(state = factor(state))

chr_2023 %>% count(state)

nrow(chr_2023)


chr_2023 <- chr_2023 %>% select(fipscode, 
                                county,
                                state,
                                county_ranked,
                                v002_rawvalue,
                                v063_rawvalue,
                                v011_rawvalue,
                                v009_rawvalue,
                                v143_rawvalue) 

chr_2023 <- chr_2023 %>% rename(Poor_or_fair_health = v002_rawvalue,
                                Median_household_income = v063_rawvalue,
                                Adult_obesity = v011_rawvalue,
                                adult_smoking = v009_rawvalue,
                                insufficent_sleep = v143_rawvalue) %>% mutate(Poor_or_fair_health = Poor_or_fair_health * 100,
                                                                              Median_household_income = Median_household_income / 1000,
                                                                              Adult_obesity = Adult_obesity * 100,
                                                                              adult_smoking = adult_smoking * 100,
                                                                              insufficent_sleep = insufficent_sleep * 100)



# Compute quantile cutoffs
chr_2023 %>%
  summarise(q40 = quantile(adult_smoking, c(0.4), na.rm = TRUE),  # Bottom 40% cutoff
            q60 = quantile(adult_smoking, c(0.6), na.rm = TRUE))  # Top 40% cutoff

# Categorize adult smoking into "Low", "Medium", and "High" groups
chr_2023 <- chr_2023 %>%
  mutate(adult_smoking_grp = case_when(
    adult_smoking <= 18.9 ~ "Low",
    adult_smoking > 18.9 & adult_smoking < 20.9 ~ "Medium",
    adult_smoking >= 20.9 ~ "High")) %>%
  mutate(adult_smoking_grp = factor(adult_smoking_grp, levels = c("Low", "Medium", "High")))

# Count occurrences in each category
chr_2023 %>% count(adult_smoking_grp)

chr_2018_raw <- read_csv(suppressWarnings(here("data/raw/chr_2018.csv")), guess_max = 4000, show_col_types = FALSE)


chr_2018_raw <- chr_2018_raw %>% mutate(fipscode = as.character(fipscode))

chr_2018 <- chr_2018_raw %>% select(fipscode, v143_rawvalue)

chr_2023 <- left_join(chr_2023, chr_2018, by = "fipscode")

chr_2023 <- chr_2023 %>% rename(insufficent_sleep_2018 = v143_rawvalue) %>% 
  mutate(insufficent_sleep_2018 = insufficent_sleep_2018 * 100) %>% 
  rename(insufficent_sleep_2023 = insufficent_sleep)



chr_2023 <- chr_2023 %>% select(fipscode, state, county, Poor_or_fair_health, Median_household_income, Adult_obesity, adult_smoking_grp, adult_smoking, insufficent_sleep_2023, insufficent_sleep_2018, county_ranked)

write_csv(chr_2023, here("data/processed/chr_2023.csv"))
saveRDS(chr_2023, here("data/processed/chr_2023.rds"))


chr_2023

describe(chr_2023)

gg_miss_var(chr_2023)

# Calculate the percentage of missing values for each variable
chr_2023 %>%
  summarise(missing_percent = mean(is.na(insufficent_sleep_2018)) * 100)


sapply(chr_2023, function(x) n_distinct(x))

s1 <- mosaic::favstats(~ Median_household_income, data = chr_2023)
s2 <- mosaic::favstats(~ Poor_or_fair_health, data = chr_2023)
s3 <- bind_rows(list(Median_Household_Income = s1, Poor_or_fair_health = s2), .id = "id") 
s3 <- s3 %>% rename(Variable = id)
s3 %>% gt() %>% fmt_number(decimals = 2)

# Compute Pearson correlation
income_cor <- glue("Pearson correlation = {round(cor(chr_2023$Median_household_income, 
                                                      chr_2023$Poor_or_fair_health, use = 'complete.obs'), 3)}")

# Plot
ggplot(chr_2023, aes(x = Median_household_income, y = Poor_or_fair_health)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm", formula = y ~ x, col = "red", se = TRUE) +
  geom_smooth(method = "loess", formula = y ~ x, col = "steelblue", se = FALSE) + 
  theme_bw() + 
  geom_text(aes(x = 175, y = 35, label = income_cor), hjust = 1, size = 4) +  
  labs(title = "Association between Household Income and Poor or Fair Health Outcomes",
       subtitle = "Fitted with Linear Model and Loess Regression Curves",
       x = "Median Household Income (Thousands of Dollars)",
       y = "Percentage of Poor or Fair Health Outcomes",
       caption = "Source: 2023 County Health Rankings")



boxCox(chr_2023$Poor_or_fair_health ~ chr_2023$Median_household_income)

powerTransform(chr_2023$Poor_or_fair_health ~ chr_2023$Median_household_income)


# Compute Pearson correlation
income_cor <- glue("Pearson correlation = {round(cor(chr_2023$Median_household_income, 
                                                      log(chr_2023$Poor_or_fair_health), use = 'complete.obs'), 3)}")

ggplot(chr_2023, aes(x = Median_household_income, y = log(Poor_or_fair_health))) + 
  geom_point(size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, col = "red", se = FALSE) +
  geom_smooth(method = "loess", formula = y~x, col = "steelblue", se = FALSE) + 
  geom_text(aes(x = 155, y = 5, label = income_cor), hjust = 1, size = 4) +  
  theme_bw() + 
  labs(title = "Association between Household income vs. Log of Poor or fair health outcomes",
       x = "Median Household Income (Thousands of Dollars)",
       y = "Log Percentage of Poor or Fair Health Outcomes",
       subtitle = "Source: 2023 County Health Rankings")

p1 <- ggplot(chr_2023, aes(x = Median_household_income, y = Poor_or_fair_health)) + geom_point(size = 2) + 
  geom_smooth(method = "lm", formula = y ~ x, col = "red", se = FALSE) +
  geom_smooth(method = "loess", formula = y~x, col = "steelblue", se = FALSE) + theme_bw() + 
  labs(title = "Household income vs. Poor or fair health outcomes",
       subtitle = "Fitted with Linear Model and Loess Regression Curves",
       x = "Median Household Income (Thousands of Dollars)",
       y = "% Poor Health Outcomes")

p2 <- ggplot(chr_2023, aes(x = log(Median_household_income), y = log(Poor_or_fair_health))) + geom_point(size = 2) + 
  geom_smooth(method = "lm", formula = y ~ x, col = "red", se = FALSE) +
  geom_smooth(method = "loess", formula = y~x, col = "steelblue", se = FALSE) + theme_bw() + 
  labs(title = "Log Household income vs. Log Poor or fair health outcomess",
       subtitle = "Fitted with Linear Model and Loess Regression Curves",
       x = "Log Median Household Income (Thousands of Dollars)",
       y = "Log % Poor Health Outcomes")


p1 / p2



m1 <- lm(log(Poor_or_fair_health) ~ log(Median_household_income), data = chr_2023)

tidy(lm(log(Poor_or_fair_health) ~ log(Median_household_income), data = chr_2023),conf.int = TRUE, conf.level = 0.90) %>%
  gt() %>% fmt_number(decimals = 3)


glance(lm(log(Poor_or_fair_health) ~ log(Median_household_income), data = chr_2023)) %>% select(r.squared, sigma, nobs) %>% gt() %>% fmt_number(decimals = c(3))


par(mfrow=c(1,2)); plot(m1, which = 1:2); par(mfrow = c(1,1))

chr_2023_aug <- augment(m1, newdata = chr_2023)

chr_2023_aug %>% select(state, county, Poor_or_fair_health, Median_household_income, .fitted, .resid) %>%
  mutate(std_resid_manual = .resid / sd(.resid, na.rm = TRUE)) -> cc_high

cc_high %>% arrange(desc(std_resid_manual)) %>% head(n = 2)


smoke <- chr_2023 %>% filter(complete.cases(Adult_obesity, adult_smoking, adult_smoking_grp)) %>% select(fipscode:county, Adult_obesity, adult_smoking, adult_smoking_grp)

mosaic::favstats(Adult_obesity ~ adult_smoking_grp, data = smoke) %>% gt() %>% fmt_number(decimals = 2)



smoke %>% ggplot(aes(x = adult_smoking_grp, y = Adult_obesity)) + geom_violin(fill = "ivory") + 
  geom_boxplot(width = 0.3, fill = "steelblue", outlier.size = 2, notch = TRUE) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 2, fill = "black") +
  labs(title = "Adult Obesity Rates for Adult Smoker Groups",
       subtitle = "Data from 611 Counties",
       x = "Smoking Group",
       y = "Adult Obesity Percentage",
       caption = "Source: 2023 County Health Rankings") + coord_flip() + theme_bw()


p1 <- smoke %>% filter(adult_smoking_grp == "Low") %>% ggplot(aes(sample = Adult_obesity)) +
  geom_qq() +
  geom_qq_line(col = "red") + 
  theme(aspect.ratio = 1) +
  labs(title = "Low Smoking Group",
       x = "Expectation for Standard Normal ",
       y = "Adult Obesity Percentage")

p2 <- smoke %>% filter(adult_smoking_grp == "Medium") %>% ggplot(aes(sample = Adult_obesity)) +
  geom_qq() +
  geom_qq_line(col = "red") + 
  theme(aspect.ratio = 1) +
  labs(title = "Medium Smoking Group",
       x = "Expectation for Standard Normal ",
       y = "Adult Obesity Percentage")


p3 <- smoke %>% filter(adult_smoking_grp == "High") %>% ggplot(aes(sample = Adult_obesity)) +
  geom_qq() +
  geom_qq_line(col = "red") + 
  theme(aspect.ratio = 1) +
  labs(title = "High Smoking Group",
       x = "Expectation for Standard Normal ",
       y = "Adult Obesity Percentage")

p1 + p2 + p3

smoke %>% group_by(adult_smoking_grp) %>% summarise(n = n(), Variance = var(Adult_obesity)) 

anova_results <- oneway.test(Adult_obesity ~ adult_smoking_grp, 
                             data = smoke, 
                             var.equal = FALSE)  # Welch’s ANOVA


anova_results




games_howell_results <- games_howell_test(Adult_obesity ~ adult_smoking_grp, data = smoke)

games_howell_results %>% gt() %>% fmt_number(decimals = 2)



slp <- chr_2023 %>% filter(complete.cases(insufficent_sleep_2023, insufficent_sleep_2018)) %>% select(fipscode:county, insufficent_sleep_2023, insufficent_sleep_2018)

slp <- slp %>% mutate(sleep_diff = insufficent_sleep_2023 - insufficent_sleep_2018)

sleep1 <- mosaic::favstats(~ insufficent_sleep_2023, data = slp)
sleep2 <- mosaic::favstats(~ insufficent_sleep_2018, data = slp)
sleep4 <- mosaic::favstats(~ sleep_diff, data = slp)
sleep3 <- bind_rows(list(insufficent_sleep_2023 = sleep1, insufficent_sleep_2018 = sleep2,sleep_diff = sleep4 ))
sleep3$Variable <- c("insufficent sleep 2023", "insufficent sleep 2018", "Sleep Difference" )
sleep3 <- sleep3 %>% select(Variable, min:missing)
sleep3 %>% gt() %>% fmt_number(decimals = 2)


p1 <- slp %>% ggplot(aes(sample = sleep_diff)) +
  geom_qq() +
  geom_qq_line(col = "steelblue") +
  theme(aspect.ratio = 1) +
  labs(title = "Normal Q-Q Plot",
       x = "Expectation under Standard Normal",
       y = "Sleep 2023 - Sleep 2018") + theme_bw()


p2 <- slp %>% ggplot(aes(x = sleep_diff)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 15, fill = "steelblue", col = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(slp$sleep_diff, na.rm = TRUE),
                                         sd = sd(slp$sleep_diff, na.rm = TRUE)),
                col = "red", lwd = 1.5) +
  labs(title = "Histogram of Sleep Difference",
       subtitle = "Imposed with Normal Density Function",
       x = "Change in Sleep Percentage",
       y = "Density") + theme_bw() + scale_y_continuous(expand = c(0,0))


p3 <- slp %>% ggplot(aes(x = sleep_diff, y = "")) +
  geom_violin(fill = "ivory") +
  geom_boxplot(fill = "steelblue", width = 0.5, outlier.size = 2, notch = TRUE) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 2, fill = "black") +
  labs(title = "Boxplot of Sleep Difference",
       x = "Change in Sleep Percentage",
       y = "") + theme_bw()


p1 + (p2 / p3 + plot_layout(heights = c(4,1))) +
  plot_annotation(title = "Percent Change in Insufficent Sleep Between 2023 and 2018",
                  subtitle = "n = 611 Counties",
                  caption = "Source: 2023 and 2018 County Health Rankings")


sleep_cor <- str_glue("Pearson correlation = ", round_half_up(cor(slp$insufficent_sleep_2018, slp$insufficent_sleep_2023), 3))

ggplot(slp, aes(x = insufficent_sleep_2018, y = insufficent_sleep_2023)) + geom_point(size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, col = "red", se = TRUE) +
  geom_smooth(method = "loess", formula = y~x, col = "steelblue", se = FALSE) + theme_bw() +
  geom_text(label = sleep_cor, x = 27, y = 45 ) +
  labs(title = "Association between Insufficent Sleep in 2018 and 2023",
       subtitle = "Fitted with a Linear Model and Loess Regression Curves",
       x = "Percentage of Insufficent Sleep in 2018",
       caption = "Source: 2023 County Health Rankings",
       y = "Percentage of Insufficent Sleep in 2023")



t.test(slp$sleep_diff, data = smoke, conf.int = TRUE, conf.level = 0.90) %>% tidy() %>% gt() %>% fmt_number(decimals = 2)


# quarto::quarto_render("Analysis.qmd", output_format = "pdf")