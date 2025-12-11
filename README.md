# Spruce Budworm System Diagram

## Purpose

We are a group of university students designing an environmental-related project to help our client, Dr. Rob Johns, of Natural Resources Canada, an expert on spruce budworm and related forest management strategies. Decades of research have been poured into this topic, but the whole system remains modular, with less research happening in between each part of the component. 


Our first objective is therefore to illustrate and identify those system linkages, using the system diagram created with this project. We also used this diagram to identify the components missing in the scientific literature, highlighting research gaps that our client could explore. Finally, we sought to understand how climate change risks might interfere with this entire system. We examined this problem by analyzing the impact of temperature and temperature fluctuations as an exogenous variable on the other linkages. 

---

## Description

A literature review was first conducted to identify the linkages within the biotic, abiotic, and socio-economic systems surrounding the Eastern spruce budworm in Quebec and New Brunswick. Through path-analysis calculations, a meta-analysis was conducted to determine the effects of temperature and temperature fluctuations on the biotic and abiotic compartments of the system. 

## Installation

The following packages must be installed before running the Shiny app:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "visNetwork",
  "readxl",
  "dplyr",
  "DT"
))
