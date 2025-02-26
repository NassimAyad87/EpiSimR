# EpiSimR
[![CRAN status](https://www.r-pkg.org/badges/version/EpiSimR)](https://CRAN.R-project.org/package=EpiSimR)

## Overview

**EpiSimR** is an R Shiny application designed for simulating the dynamics of epidemic and endemic disease spread. It utilizes deterministic compartmental mathematical models to provide interactive visualizations and insights into disease transmission and control strategies.

## Description

EpiSimR offers an interactive platform for modeling the spread of infectious diseases. Users can select different epidemiological models, including:

-   **SIR (Susceptible-Infectious-Recovered)**
-   **SEIR (Susceptible-Exposed-Infectious-Recovered)**

The app allows users to configure various parameters, such as:

-   Inclusion or exclusion of permanent immunity
-   Consideration of demographic changes
-   Implementation of vaccination and isolation strategies

Users can adjust key epidemiological parameters, including:

-   **Basic reproduction number (R₀)**
-   **Birth and mortality rates**
-   **Infectious and latent periods**
-   **Immunity duration**
-   **Vaccination coverage**
-   **Isolation rate**

By modifying these parameters, users can simulate and visualize disease progression under different conditions and intervention strategies.

## Installation

To install the package, use:

``` r
# Install from GitHub
devtools::install_github("NassimAyad87/EpiSimR", dependencies = TRUE)

# Install from CRAN
install.packages("EpiSimR", dependencies = TRUE)
```

## Usage

Launch the Shiny application using:

``` r
library(EpiSimR)
run_app()
```

## Features

### Model Selection & Customization

-   Choose between **SIR** and **SEIR** models.
-   Include or exclude permanent immunity.
-   Consider demographic changes.
-   Customize vaccination and isolation strategies.

### Parameter Adjustments

-   **Basic reproduction number (R₀)**
-   **Birth and mortality rates**
-   **Infectious period**
-   **Latent period** (for SEIR model)
-   **Immunity duration**
-   **Vaccination coverage**
-   **Isolation rate**

### Simulation & Interactive Visualization

-   Real-time simulation of epidemic and endemic dynamics.
-   Interactive graphical representation of disease spread over time.
-   Visual assessment of public health intervention impacts (e.g., vaccination, isolation).

### User-Friendly Interface

-   Built with **Shiny** for an intuitive and interactive experience.
-   Dynamic updates based on user inputs.

### Application Purpose

-   Serves as an educational and analytical tool for understanding epidemiological modeling.
-   Assists in evaluating the effectiveness of public health interventions.

## License

This package is licensed under the **GPL-3 License**.

## Citation
To cite this package, use:

``` r
citation("EpiSimR")
```

