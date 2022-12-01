# gp-inference

Repository for "Inference in Gaussian Process Models for Political Science"

## Reproducing results / compiling the paper PDF

The paper is written in [R Markdown](https://rmarkdown.rstudio.com/), so the code that generates the results in the paper is actually in the source text of the paper itself.
So you can reproduce all results in the paper by simply opening the "inference-in-gp-models.Rmd" file in [RStudio](https://posit.co/products/open-source/rstudio/) and pressing the "🧶 Knit" button---this will generate the paper PDF, *rerunning the replication code in the process* and therefore reproducing the results.
However, I use a custom R Markdown format from a personally maintained R package, so to do so you'll need to install that first.
You can do so by running the following R command:

```r
## Install devtools if you don't have that
# install.packages("devtools")
devtools::install_github("duckmayr/quack")
```
