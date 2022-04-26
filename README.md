# Associations between COVID-19 Mortality and Public Health Factors in Texas

## Description
This is a collaborative data analysis project investigating the COVID-19 dataset provided and maintained by the [COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University](https://github.com/CSSEGISandData/COVID-19), implemented in R and submitted as a group project for the *STAT 3355: Data Analysis for Statisticians and Actuaries* course at The University of Texas at Dallas. Our final results consist of a presentation and a report, both available in this repository.

Our driving questions focus on identifying public health and socioeconomic factors associated with the spread and lethality of COVID-19, as well as attempting to predict the future course of COVID-19 with respect to caseloads and vaccine administrations in the state of Texas.

Our primary dataset is the CSSE dataset, which provides data on cases. Secondary sources of public health data (e.g. vaccines, pre-existing conditions, socioeconomic status, population density) are listed below.

## Contents
* `exploratory/`  - Data exploration and pattern discovery code
* `presentation/` - Presentation code and slides as delivered in class
* `proposal/` - Initial proposal containing project ambitions as submitted
* `report/` - Final report and code as submitted

## Primary Dataset
* [JHU CSSE COVID-19 Data Repository](https://github.com/CSSEGISandData/COVID-19)

## Secondary Datasets
* [JHU CCI CRC COVID-19 Data](https://github.com/govex/COVID-19)
* [Texas DHHS COVID-19 Case Fatality and Demographics](https://dshs.texas.gov/coronavirus/AdditionalData.aspx)
* [CDC Research Study (“Underlying Medical Conditions and Severe Illness Among 540,667 Adults Hospitalized With COVID-19, March 2020–March 2021”)](https://www.cdc.gov/pcd/issues/2021/21_0123.htm)
* [Land Area from The County Information Program of Texas Association of Counties](https://txcip.org/tac/census/morecountyinfo.php?MORE=1005)

## Dependencies
* `ggplot2`
* `lubridate`
* `dplyr`
* `plyr`
* `gridExtra`
* `cowplot`
* `grid`
* `gapminder`
* `readxl`
* `UsingR`

## Collaborators
Michael Tsang, Kevin Jin, & Mingyu Sun