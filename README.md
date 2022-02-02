# Web scraper of pappers

This project scrapes the article information published on [Journal of Pidgin and Creole Languages](https://benjamins.com/catalog/jpcl) published by [John Benjamins Publishing Company](https://benjamins.com/) and [Language Variation and Change](https://www.cambridge.org/core/journals/language-variation-and-change) published by [Cambridge University Press](https://www.cambridge.org/).

By having all the article information in one file, you can search faster for the papers that matter to you without having to navigate the online web pages.

## Outputs

If you just want the results. There is on this repo a run of the scrapper done on the 2n of February 2022:

Journal of Pidgin and Creole Languages -> benjamins_articles.csv
Language Variation and Change -> cambridge_articles.csv


# Run

To generate again the csv files again, you will have to install [Cabal](https://cabal.readthedocs.io/en/3.6/getting-started.html). Then on the project directory run the followiing commands:

```
$ cabal init
$ cabal run
```
