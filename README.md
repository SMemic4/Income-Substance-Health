# The Impact of Income and Substance Use on Health Outcomes in US Counties

# Introduction

Understanding the relationship between socioeconomic factors and health outcomes is critical for informing public health policies and interventions. This project explores three key research questions using publicly available data on U.S. counties:

1. What is the relationship between median household income and poor health outcomes? Lower household income may correlate with worse health outcomes due to reduced access to healthcare and economic opportunities.
2. Is there an association between adult smoking rates and obesity prevalence? Since nicotine suppresses appetite, counties with higher smoking rates might have lower obesity rates.
3. How has the average sleep quality in U.S. counties changed from 2018 to 2023? Sleep quality trends may reveal shifts in lifestyle, stress levels, and overall well-being over time.

The dataset used in this analysis includes variables related to income, health behaviors, and demographic factors across all U.S. counties. Various statistical models and data visualizations were applied to uncover patterns and relationships.

# Data Source

The data came from [County Health Rankings](https://www.countyhealthrankings.org/) which contains publicly available U.S. county health and socioeconomic data from US counties over the years. This analysis used data from 2023 and 2018.

# Data preprocessing 

* Variable selection and cleaning to ensure consistency.
* Creating new predictors for Analysis 2 and incorporating 2018 data for Analysis 3.
* Arranging and saving an analytic tibble for structured analysis.

# Results Summary
## Analysis 1: Median Household Income and Poor Health Outcomes

This analysis aimed to determine whether counties with higher median household incomes are associated with a lower percentage of adults reporting poor or fair health outcomes. By fitting a log-log linear model, we observed a strong negative relationship between these two variables, where a 1% increase in median household income is associated with an estimated 0.87% decrease in the percentage of adults reporting poor or fair health. The 90% confidence interval for the income coefficient is [-0.891, -0.850], confirming that the effect is statistically significant and suggesting a consistent inverse association between income and health outcomes.The model achieved an R-squared value of 0.613, meaning it explains 61.3% of the variability in poor or fair health outcomes across U.S. counties. While this indicates a moderately strong fit, there remains some unexplained variation, which may be influenced by additional socioeconomic or healthcare access factors not accounted for in this model.


## Analysis 2: Smoking Rates and Obesity

This analysis aimed to determine whether counties in the U.S. with higher smoking prevalence are associated with lower or higher obesity rates among adults. Contrary to the initial hypothesis that higher smoking rates would correlate with lower obesity rates, the results indicate the opposite trend—counties with a higher percentage of adult smokers tend to have higher obesity rates. Using Welch’s ANOVA (One-way ANOVA), a statistically significant difference (p < 2.2e-16) in adult obesity rates across smoking groups was observed. Games-Howell post hoc comparisons further confirmed that all group differences were significant:

* Medium Smoking counties had an obesity rate 3.46 percentage points higher [95% CI: 3.04, 3.88] than Low Smoking counties.
* High Smoking counties had an obesity rate 6.29 percentage points higher [95% CI: 5.93, 6.65] than Low Smoking counties.
* High Smoking counties also had an obesity rate 2.82 percentage points higher [95% CI: 2.46, 3.19] than Medium Smoking counties.

These findings suggest a positive association between smoking prevalence and obesity rates, challenging the assumption that higher smoking rates might contribute to lower obesity levels due to nicotine’s appetite-suppressing effects.

## Analysis 3: Changes in Sleep Quality between 2018 and 2023

This analysis aimed to determine whether there was a significant change in the percentage of adults reporting insufficient sleep across the same U.S. counties between 2018 and 2023. The results indicate a statistically significant increase in insufficient sleep prevalence over this period. The median percentage of adults reporting insufficient sleep in 2023 was 34.6% (IQR: 3.6), compared to 33.6% in 2018 (IQR: 4.86), suggesting a slight but consistent upward trend. A paired t-test confirmed a mean increase of 1.41 percentage points, with a 90% confidence interval of [1.33, 1.48], providing strong evidence that insufficient sleep rates have risen in U.S. counties over time.


# Future Work
* Incorporate additional health indicators, such as mental health trends.
* Explore causal relationships using more advanced modeling techniques.
* Compare county-level trends with national and global datasets.


# Source

[County Health Rankings](https://www.countyhealthrankings.org/)