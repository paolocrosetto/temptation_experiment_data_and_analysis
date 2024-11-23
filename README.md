# Data and Analysis for "Hard vs. soft commitments: Experimental evidence from a sample of French gamblers"

In this repository you will find the original data and the analysis carried out for the paper "*Hard vs. soft commitments: Experimental evidence from a sample of French gamblers*", by Paul Bettega, Paolo Crosetto, Dimitri Dubois and Rustam Romaniuc. The artcile is forthcoming in *Theory and Decision*.

# The paper

The paper is available as a [GAEL (Grenoble Applied Economics Laboratory) Working Paper.](https://econpapers.repec.org/scripts/redir.pf?u=https%3A%2F%2Fgael.univ-grenoble-alpes.fr%2Fsites%2Fdefault%2Ffiles%2FMediatheque%2Fdoc-recherche%2FWP%2FA2023%2Fgael2023-05.pdf;h=repec:gbl:wpaper:2023-05) Its final version is available on Theory and Decision.

# The data

Data are stored in the `/Data` folder.

-   `temptation_data` contains the *clean* data issued from our oTree app, and is used for all analyses but the MCMC estimation
-   `MCMC` contains the *results* of the MCMC estimation

# The analysis

Analyses are fully run using `R`. They depend on the following packages

-   `tidyverse`: R dialect used here

-   `broom`: to clean test results and coerce them into a data frame

-   `magrittr`: for extra specialized pipes like `%$%`

-   `gtools`: to set up the list of combinations needed to perform pairwise tests over pairs of treatments

-   `esvis`: to compute cohen's d effect sizes

-   `hrbrthemes`: plot theme

-   `kableExtra`: formatting and exporting nice tables

To reproduce the analyses, simply run the `FDJ_analysis.R` script. It calls all other analysis scripts and saves figures as high-resolution .png in the `/Figures` folder and tables both as .tex source and rendered .pdf files in the `/Tables` folder.
