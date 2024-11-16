# cedR

**cedR** — you can pronounce it like *cedar* (the tree) or *cheddar* (the cheese), but what I really meant was *Cogito Ergo Dubito* (*I think therefore I doubt*), a reference to Descartes' *Cogito Ergo Sum* (*I think, therefore I am.*)

The centuries-long consensus of philosophy? We don't know what true truth is — or how to recognize it if we stumble upon it. cedR leans into this spirit of uncertainty, helping you analyze your data with just enough doubt to keep things honest.

Yeah, yeah, yeah, sure!

Deflating this balloon of philosophical thought and moving to practical details interesting for screen-gazers…

This package is for anyone looking to perform a **quick one-way statistical multicomparison** test without agonizing over which test to use. So, just prepare your data in a long format, roll it into the function, and *voilà!* A quick answer will appear, along with an ugly graph and files with summary data and a statistics report.

------------------------------------------------------------------------

## Features

-   **Automated multiple comparison tests**: No need to stress about which test to use.
-   **Quick output**: Get your results into a .csv file with just one line of code.
-   **Convenient reports**: Generate summary statistics and output files in a jiffy.
-   **Bar graphs** with standard error bars and CLD letters for clear visual representation.

------------------------------------------------------------------------

## Installation

You can install **cedR** from GitHub with the following command:

```r
devtools::install_github("milenakulasek/cedR")
```
------------------------------------------------------------------------

## Usage

To use **cedR**, simply call the function with your data and specify the necessary arguments. Here's how you can use it:

```r
# Example usage of cedR
cedR(data, "Variable_of_Interest", "Grouping_Factor", "output_summary.csv", "output_report.txt")
```

Where:

-   **data:** The dataset you want to analyse. It should be in a long format, with one column for the values and one column for the grouping factor.

-   **"Variable_of_Interest":** The name of the column in your dataset that contains the dependent variable (the values you are testing). This should be provided as a string (e.g., "age", "yield").

-   **"Grouping_Factor":** The name of the column in your dataset that contains the grouping factor (e.g., different experimental conditions or treatments). This should also be provided as a string (e.g., "variant", "treatment", "genotype").

-   **"output_summary.csv":** The name of the file where summary statistics and results will be saved in CSV format. This file will include means, standard deviations, and cld letters indicating statistical differences for each group.

-   **"output_report.txt":** The name of the text file where a report summarising the statistical analysis will be written. This report will include information on the chosen test, results of the multiple comparisons, and p-values.
