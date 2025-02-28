# Methods

## Age Standarised Rates {#asr}

Age-standardised rates attempt to adjust for variation in age structures in different populations (either different geographical areas or the same population across time). There are two methods of age-standardisation – direct and indirect.

All cancer diagnoses and cancer deaths trends were calculated using directly standardised rates. The method involves applying age-specific rates from the population of interest (i.e. your catchment) to a standard population, which in CaRDO is the World Standard Population by default [1]. Five-year age groups up to 85-years-and-above were used for all age-standardized rate calculations.

$$\sum_{i = 1}^{18} \frac{events_{i}}{population_{i}} \times weights_{i} \times 100,000$$

## Life Time Risk {#ltr}

Cumulative risk is a measure used to estimate the risk of developing or dying of cancer up to a specific age. It takes into account the removal of persons from the population of interest who have already been diagnosed with or died from cancer. Commonly expressed as a ‘1 in $n$’ proportion, the cumulative risk is calculated as:

$$n = \frac{1}{1-e^{(-5\sum a_{j} \times \frac{100}{100,000})}}$$

where $a_{j}$ are the age-specific rates (5-year age groups) per 100,000 for ages 0 – to your specific age group, for example 85. CaRDO provides the cumulative risk up to the age 85 as an approximation of lifetime risk. An $x$ in 100 variation is also supplied, calculated as the inverse of the cumulative risk multiplied by 100. These calculations assume that the person experiences the current age-specific risk rates up to the age specified (e.g. 85), so do not account for any specific risk factors (such as smoking).

## Incidence & Mortality Trends {#trends}

*This is not complete. We need to detail how trends are calculated and what packages are used etc.*