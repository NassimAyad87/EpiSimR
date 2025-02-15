# EpiSimR
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14876663.svg)](https://doi.org/10.5281/zenodo.14876663)

This is an R package containing a Shiny app for Simulating the dynamics of spread of epidemic and endemic diseases.

## Description

This package provides an interactive Shiny application based on deterministic compartmental mathematical modeling approach for simulating and visualizing the dynamics of spread of epidemic and endemic diseases. This interactive app allows users to select the model, with considering or not the permanent immunity, demographic changes, vaccination strategy, isolation strategy and adjust their related parameters such as basic reproduction number, birth and mortality rates, infectious period, latent period, immunity duration, vaccination coverage and isolation rate parameters. It offers a powerful tool for simulating epidemic and endemic dynamics and the impact of vaccination and isolation strategies to mitigate the spread of the disease. Thanks to shiny package.

## Installation

Install the package using:

``` r
devtools::install_github("NassimAyad87/EpiSimR", dependencies = TRUE) 
```

## Usage

Launch the app using:

``` r
library(EpiSimR)
run_app()
```
## 🌐 The online version  

📌 **[access the shiny application](https://nassimayad.shinyapps.io/EpiSimR/)**

## Features

### Model selection & customization:

-   Allows users to choose between SIR and SEIR models.
-   Option to include or exclude permanent immunity.
-   Option to consider or ignore demographic changes.
-   Customizable vaccination and isolation strategies.

### Parameter adjustments:

users can modify various epidemiological parameters, including: - Basic reproduction number (R₀) - Birth and mortality rates - Infectious period - Latent period (for SEIR model) - Immunity duration - Vaccination coverage - Isolation rate

### Simulation & interactive visualization:

-   Interactive simulation of epidemic and endemic dynamics.
-   Real-time visualization of disease spread over time.
-   Graphical output to assess the impact of control strategies (vaccination & isolation).

### User-friendly interface:

-   Built using the Shiny package, offering an intuitive and interactive experience.
-   Dynamic updates based on user inputs.

### Application purpose:

-   A powerful educational and analytical tool for understanding epidemiological modeling.
-   Helps explore the effectiveness of public health interventions like vaccination and isolation.

## License

This package is licensed under the GPL-3 License.
