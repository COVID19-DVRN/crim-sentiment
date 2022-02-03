# Data and code for Evaluating Criminal Justice Reform During COVID-19: The Need for a Novel Sentiment Analysis Package

This repo contains R and python scripts used in the manuscript Evaluating Criminal Justice Reform During The COVID-19 Pandemic: The Need for a Novel Sentiment Analysis Package" by Divya Ramjee, Louisa H. Smith, Anhvinh Doanvo, Marie-Laure Charpignon, Alyssa McNulty, Elle Lett, Angel N. Desai, Maimuna S. Majumder.

The scripts are in numeric order. Because several of them take a long time to run, and/or may not be reproducible (some of the articles downloaded from various news sites may have changed or no longer be available), various data products are saved along the way in the `data` directory. These include, among others:

- The manual scores from the reviewers (`ratings{1-5}.csv`).
- Data on the articles and their URLs (`incarc_{states,online}_{}.csv`)

The figures are produced by the `06_make_figures.R` file and are in the `figures` directory.
