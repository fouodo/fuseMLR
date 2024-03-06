[![Travis Build Status](https://travis-ci.org/imbs-hl/fuseMLR.svg?branch=master)](https://travis-ci.org/imbs-hl/survivalsvm)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/imbs-hl/fuseMLR?branch=master&svg=true)](https://ci.appveyor.com/project/fouodo/fuseMLR)
[![Coverage Status](https://coveralls.io/repos/github/imbs-hl/fuseMLR/badge.svg?branch=master)](https://coveralls.io/github/imbs-hl/fuseMLR?branch=master)
### fuseMLR
Cesaire J. K. Fouodo

### Introduction
This R package offers a unique approach to integrate multiple machine learning models. Users can train various machine learning models on different data entities, each representing distinct data collection layers. These models are then combined into a single meta-learner using a super learner framework. When making predictions for new data, the package leverages predictions from each layer, aggregating them to produce a final prediction.

### Installation
Installation from Github:
```R
devtools::install_github("imbs-hl/fuseMLR")
```

CRAN release coming soon.

### Usage
For usage in R, see ?fuseMLR in R. Most importantly, see the Examples section. 

The provided example, utilizing simulated data, mirrors a common scenario in multi-omics analysis. It involves data collected from three distinct layers (methylation, gene expression, and protein expression), with disease status serving as the response variable. Initially, the data entities are consolidated into a single object. Subsequently, the learner arguments (such as ```ranger```) and feature selection parameters for each entity are specified. Following model training for both the entity-level models and the meta-learner, predictions can be generated for new datasets.

Prepare entities.
```R  
data(entities)
data(disease)
entity_obj <- entity(object = entities$methylation,
                         layer = "methylation")
entity_obj <- add(object = entity_obj,
                      layer = "genexpr",
                       data = entities$genexpr)
 entity_obj <- add(object = entity_obj,
                      layer = "proteinexpr",
                       data = entities$proteinexpr)
```
Prepare learner arguments
```R
 lrnarg_obj <- lrnarg(object = list(probability = TRUE),
                       layer = "methylation")
 lrnarg_obj <- add(object = lrnarg_obj,
                    layer = "genexpr",
                    param = list(probability = TRUE))
 lrnarg_obj <- add(object = lrnarg_obj,
                    layer = "proteinexpr",
                    param = list(probability = TRUE))
```
Prepare variable selection arguments
```R
 varselectarg_obj <- varselectarg(object = list(type = "probability",
                                                 mtry.prop = 0.4),
                                   layer = "methylation")
 varselectarg_obj <- add(object = varselectarg_obj,
                          layer = "genexpr",
                          param = list(type = "probability", mtry.prop = 0.5))
 varselectarg_obj <- add(object = varselectarg_obj,
                          layer = "proteinexpr",
                          param = list(type = "probability", mtry.prop = 0.3))
```
Train layer and meta learner
```R
my_meta_lnr <- meta_lnr(data = entity_obj,
                        target = disease,
                        learner = "ranger",
                        learner_args = lrnarg_obj,
                        var_selec = "Boruta_ext",
                        var_selec_arg = varselectarg_obj,
                        aggregation = ranger.cved,
                        aggregation_args = list(validation = "createFolds",
                                               validation_args = list(),
                                               final_learner = "ranger",
                                               final_learner_args = list(mtry = 1,
                                                                            probability = TRUE)))
```
Predict using the meta learner
```R
 data(test_entities)
 data(test_disease)
 my_meta_predictions <- predict(object = my_meta_lnr, data = test_entities)
 print(my_meta_predictions)
 ## Brier score
 print(mean((my_meta_predictions$predictions[ , 1] - (test_disease == 2))^2))
```