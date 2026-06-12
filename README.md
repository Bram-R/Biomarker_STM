# Biomarker_STM: Cost-Effectiveness Analysis of Biomarker-Guided Treatment Strategies

This repository contains the source code and supporting scripts for the development and evaluation of a probabilistic decision-analytic model assessing the cost-effectiveness of biomarker-guided treatment strategies compared with current clinical practice. The model combines a diagnostic decision tree with a state-transition (Markov) cohort model to estimate costs, quality-adjusted life years (QALYs), life-years (LYs), and incremental cost-effectiveness ratios (ICERs).

The model evaluates the impact of using a biomarker test to identify patients who may benefit from treatment. Diagnostic uncertainty is explicitly incorporated through true positive, false positive, false negative, and true negative test outcomes, which subsequently determine treatment allocation and long-term disease progression. The model is implemented in R and can be adapted to reflect healthcare settings in the United Kingdom, the Netherlands, and France.

## Table of Contents

* [Background](#background)
* [Installation](#installation)
* [Usage](#usage)
* [Input Data and Parameters](#input-data-and-parameters)
* [Model Description](#model-description)
* [Validation and Analyses](#validation-and-analyses)
* [Current Development Status](#current-development-status)
* [Future Work](#future-work)
* [Contact](#contact)

## Background

Biomarker-guided treatment strategies have the potential to improve patient outcomes by targeting therapies to individuals most likely to benefit. However, biomarker tests are inherently imperfect and may result in misclassification of patients. Economic evaluation can help determine whether the additional costs associated with biomarker testing are justified by improvements in health outcomes and more efficient allocation of healthcare resources.

This repository contains a decision-analytic model developed to quantify the long-term clinical and economic consequences of implementing biomarker-guided treatment compared with current practice. The model integrates diagnostic performance, disease progression, treatment effectiveness, costs, and health-related quality of life within a unified modelling framework.

## Installation

### Prerequisites

* **R** (version 4.0.0 or later)
* R packages:

  * docstring
  * DiagrammeR
  * DiagrammeRsvg
  * rsvg
  * magick
  * BCEA
  * dampack
  * matrixStats
  * summarytools

### Steps

1. **Clone the Repository (Terminal tab next to the Console tab in RStudio):**

```bash
git clone https://github.com/Bram-R/Biomarker_STM.git
cd Biomarker_STM
```

2. **Install Required R Packages**

In R, run:

```r
required_packages <- c(
  "docstring",
  "DiagrammeR",
  "DiagrammeRsvg",
  "rsvg",
  "magick",
  "BCEA",
  "dampack",
  "matrixStats",
  "summarytools"
)

install.packages(required_packages)
```

3. **Set the Working Directory**

Ensure your R working directory is set to the repository folder.

## Usage

To replicate the model-based analyses:

1. **Main results**

Run the script `Main analyses.R` to generate the base-case deterministic and probabilistic results.

2. **Sensitivity analyses**

Run the script `Sensitivity analyses.R` to perform deterministic and probabilistic sensitivity analyses, including cost-effectiveness acceptability curves, cost-effectiveness planes, and value-of-information analyses.

3. **Intermediate results**

Run the script `Intermediate results.R` to generate cycle-level outputs, including health-state occupancy, costs, QALYs, life-years, and event-related outcomes.

## Input Data and Parameters

The model accepts both deterministic and probabilistic input parameters. These include:

* Disease prevalence
* Diagnostic sensitivity and specificity
* Transition probabilities
* Treatment effects
* Health state utilities
* Healthcare costs
* Event-related costs
* Discount rates

Probabilistic sensitivity analysis is implemented by sampling model parameters from appropriate probability distributions, including beta, gamma, and log-normal distributions.

The parameter generation process is implemented in the `f_input()` function.

## Model Description

The model consists of two components:

### Decision Tree

Patients are initially classified according to diagnostic test results into one of four groups:

* True Positive (TP)
* False Positive (FP)
* False Negative (FN)
* True Negative (TN)

The distribution across these groups is determined by disease prevalence, test sensitivity, and test specificity.

### State-Transition Model

Following diagnostic classification, patients enter a state-transition model representing disease progression.

Each diagnostic group contains three health states:

* Disease State 1
* Disease State 2
* Death

Treatment effects are applied through hazard ratios that modify disease progression and mortality risks. The model operates using monthly cycles over a lifetime horizon and tracks:

* Health state occupancy
* State transitions
* Costs
* QALYs
* Life-years

for each treatment strategy.

## Validation and Analyses

The model is currently undergoing technical and face-validity assessment.

Validation activities include:

* Verification of transition matrices
* Verification of decision tree probabilities
* Cohort trace validation
* Internal consistency checks
* Extreme value testing
* Code review and debugging

Uncertainty analyses are planned using Monte Carlo simulation and standard health-economic evaluation methods.

## Current Development Status

The core model structure has been implemented and is operational.

Currently available:

* Decision tree model
* State-transition model
* Deterministic analyses
* Probabilistic sensitivity analysis framework
* Cost, QALY, and life-year calculations

Currently under development:

* Intermediate result outputs
* Deterministic sensitivity analyses
* Probabilistic sensitivity analyses
* Value-of-information analyses
* Additional validation procedures
* Model documentation

The repository should therefore be considered a working development version pending full validation.

## Future Work

Planned extensions include:

* Completion of intermediate output reporting
* One-way and multi-way sensitivity analyses
* Cost-effectiveness acceptability curves
* Incremental cost-effectiveness planes
* Expected value of perfect information (EVPI)
* Expected value of partial perfect information (EVPPI)
* Scenario analyses
* Additional country-specific adaptations
* External validation exercises

## Contact

For further information regarding this repository, please contact:

**Bram Ramaekers**

* GitHub: @Bram-R

## Disclaimer

This repository contains a research model under active development. Results generated by the model should be considered preliminary until the model has undergone full validation and review.
