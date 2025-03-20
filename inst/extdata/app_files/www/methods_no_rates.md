# Methods

## Incidence & Mortality Trends {#trends}

Incidence and mortality trends were calculated by fitting piece-wise functions, composed of splines and linear models, to the data. 

Break points were identified using the `strucchange` package, specifically the function `breakpoints()`. The data was then segmented by these breakpoints. A maximum number of 3 breakpoints were set, with a minimum number of 5 observations within each segment. Then, a spline and linear model was fit to each segment using the `mgcv` package. The model with the lowest AIC was chosen for each given segment.

---

*CaRDO (https://github.com/CCQResearch/CaRDO). Cancer Council Queensland: The Viertel Cancer Research Centre. Version 06-2025.*
