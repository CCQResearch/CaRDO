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

Incidence and mortality trends were calculated by fitting piece-wise functions, composed of splines and linear models, to the data. 

Break points were identified using the `strucchange` package, specifically the function `breakpoints()`. The data was then segmented by these breakpoints. A maximum number of 3 breakpoints were set, with a minimum number of 5 observations within each segment. Then, a spline and linear model was fit to each segment using the `mgcv` package. The model with the lowest AIC was chosen for each given segment.

## Cancer ICD-O3 codes used {#Cancer-codes}

| Cancer Type                                                                                                                             | ICD Code                                    |
| ----------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------- |
| [Anus](https://cancerqld.org.au/glossary/anus/) & anal canal cancer                                                                       | C21                                         |
| [Bladder](https://cancerqld.org.au/glossary/bladder/) cancer                                                                              | C67                                         |
| Bone cancer                                                                                                                                 | C40 to C41                                  |
| Brain cancer                                                                                                                                | C70 to C72                                  |
| [Breast cancer](https://cancerqld.org.au/glossary/breast-cancer/)                                                                         | C50                                         |
| Cervical cancer                                                                                                                             | C53                                         |
| Chronic myeloproliferative diseases                                                                                                         | M995 to M996                                |
| [Colon](https://cancerqld.org.au/glossary/colon/) cancer                                                                                    | C18                                         |
| [Colorectal cancer](https://cancerqld.org.au/glossary/colorectal-cancer/)                                                                   | C18 to C20, C218                            |
| Connective tissue & peripheral nerves cancer                                                                                                | C47, C49                                    |
| Endocrine glands cancer                                                                                                                     | C74 to C75                                  |
| Eye cancer                                                                                                                                  | C69                                         |
| Floor of mouth cancer                                                                                                                       | C04                                         |
| Gallbladder cancer                                                                                                                          | C23 to C24                                  |
| Gum cancer                                                                                                                                  | C03                                         |
| Gynaecological cancers                                                                                                                      | C51 to C58                                  |
| Head and neck cancers                                                                                                                       | C01 to C14, C30 to C32                      |
| Hodgkin lymphoma                                                                                                                            | M965 to M966                                |
| Kaposi sarcoma                                                                                                                              | M914                                        |
| Kidney cancer                                                                                                                               | C64 to C66, C68                             |
| [Larynx](https://cancerqld.org.au/glossary/larynx/) cancer                                                                                  | C32                                         |
| Leukaemia                                                                                                                                   | M980 to M994 (excluding M9733/3)            |
| Lip cancer                                                                                                                                  | C00                                         |
| Liver cancer                                                                                                                                | C22                                         |
| [Lung cancer](https://cancerqld.org.au/glossary/lung-cancer/)                                                                               | C33 to C34                                  |
| Lymphoid leukaemia                                                                                                                          | M982 to M983                                |
| Lymphoma                                                                                                                                    | M959 to M972                                |
| [Melanoma](https://cancerqld.org.au/glossary/melanoma/)                                                                                     | C44 and C80 (only M872 to M879)             |
| [Mesothelioma](https://cancerqld.org.au/glossary/mesothelioma/)                                                                             | M905                                        |
| Myelodysplastic diseases                                                                                                                    | M998                                        |
| Myeloid leukaemia                                                                                                                           | M984 to M993                                |
| Myeloma                                                                                                                                     | M973                                        |
| [Nasal cavity](https://cancerqld.org.au/glossary/nasal-cavity/) cancer                                                                      | C30 to C31                                  |
| [Nasopharynx](https://cancerqld.org.au/glossary/nasopharynx/) cancer                                                                        | C11                                         |
| Non-Hodgkin lymphoma                                                                                                                        | M967 to M972                                |
| Oesophageal cancer                                                                                                                          | C15                                         |
| Other female genital organs cancer                                                                                                          | C52, C55, C57 to C58                        |
| Other lip, [oral](https://cancerqld.org.au/glossary/oral/) cavity & [pharynx](https://cancerqld.org.au/glossary/pharynx/) cancer            | C14                                         |
| Other lymphatic cancers                                                                                                                     | M974 to M976                                |
| Other major [salivary glands](https://cancerqld.org.au/glossary/salivary-glands/) cancer                                                    | C07 to C08                                  |
| Other parts of mouth cancer                                                                                                                 | C05 to C06                                  |
| Other [skin cancer](https://cancerqld.org.au/glossary/skin-cancer/)                                                                         | C44 (excluding M805 to M811, M872 to M879)   |
| Other specified leukaemia                                                                                                                   | M994                                        |
| [Ovarian cancer](https://cancerqld.org.au/glossary/ovarian-cancer/)                                                                         | C56                                         |
| Pancreatic cancer                                                                                                                           | C25                                         |
| Penile cancer                                                                                                                               | C60, C63                                    |
| [Prostate](https://cancerqld.org.au/glossary/prostate/) cancer                                                                              | C61                                         |
| Pyriform sinus & [hypopharynx](https://cancerqld.org.au/glossary/hypopharynx/) cancer                                                       | C12 to C13                                  |
| Rectosigmoid junction & rectal cancer                                                                                                       | C19 to C20                                  |
| Retroperitoneum & peritoneum cancer                                                                                                         | C48                                         |
| Small intestine cancer                                                                                                                      | C17                                         |
| Stomach cancer                                                                                                                              | C16                                         |
| Testicular cancer                                                                                                                           | C62                                         |
| Thymus, heart, [mediastinum](https://cancerqld.org.au/glossary/mediastinum/) & [pleura](https://cancerqld.org.au/glossary/pleura/) cancer    | C37 to C38                                  |
| [Thyroid](https://cancerqld.org.au/glossary/thyroid/) cancer                                                                                | C73                                         |
| Tongue cancer                                                                                                                               | C01 to C02                                  |
| Tonsil & [oropharynx](https://cancerqld.org.au/glossary/oropharynx/) cancer                                                                | C09 to C10                                  |
| Unknown [primary site](https://cancerqld.org.au/glossary/primary-site/) cancers                                                             | C26, C39, C76 to C77, C80                   |
| Unspecified leukaemia                                                                                                                       | M980                                        |
| Uterine cancer                                                                                                                              | C54                                         |
| Vulva cancer                                                                                                                                | C51                                         |

---

*CaRDO (https://github.com/CCQResearch/CaRDO). Cancer Council Queensland: The Viertel Cancer Research Centre. Version 06-2025.*
