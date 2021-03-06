# Project 2: Shiny App Development Version 2.0

### [Project Description](doc/project2_desc.md)

![screenshot](doc/Capture.JPG)

In this second project of GR5243 Applied Data Science, we develop a version 2.0 of an *Exploratory Data Analysis and Visualization* shiny app on a topic of your choice using [NYC Open Data](https://opendata.cityofnewyork.us/) or U.S. government open data released on the [data.gov](https://data.gov/) website. See [Project 2 Description](doc/project2_desc.md) for more details.  

The **learning goals** for this project are:

- business intelligence for data science
- study legacy codes and further development
- data cleaning
- data visualization
- systems development/design life cycle
- shiny app/shiny server

*The above general statement about project 2 can be removed once you are finished with your project. It is optional.

## Project Title: NYC Canopy

[Shiny App](https://zoraxl2788.shinyapps.io/treeappv2/)

[Presentation Slides](https://docs.google.com/presentation/d/1bbIEB2i_deGZsB5M0sKe9NvVHt9uWDsFqEAu7xJED1c/edit#slide=id.g630bec13f4_0_445) 

Term: Fall 2019

+ Team #2
+ **Canopy NYC: A Shiny App to Map Trees and Pollution**: + Team members
	+ Lingyi Cai
	+ Ashley Culver
	+ Tong Dai
	+ Xiaotong Li
	+ Zihan Zhou

+ **Project summary**: In this shiny app, we introduce an eco-conscious app designed to help city planners, as well as individuals, to better plan for a tree-lined New York City. City planners can locate trees that should be removed, look at figures on the breakdown of paid NYC Parks employees versus volunteers working with live or dead trees, and consider air pollution in calculations of where new trees should be planted.

The app includes 

1) a mapping of the trees of New York City from the 2015, 2005, and 1995 NYC Street Tree Census.
	- users can view species, health status, and address of street trees
	- users can add a new tree to update tree database
	
2) a mapping of pollutants PM2.5, O2, NO, NO2, and black carbon in New York City from the NYCCAS Air Pollution Rasters.
	- users can view areas of high concentrations of these air pollutants
	
3) analyses of tree information, human population figures, and communal resources (sidewalks) affected by trees

+ **Contribution statement**: ([default](doc/a_note_on_contributions.md))

All team members contributed to the app idea. AC cleaned and analyzed the Population data (New York City Population By Neighborhood Tabulation Areas). AC and DT explored the NYC Street Tree Data and created analytical tools for tree data. LC built the shiny app baseline and "Forest" feature. ZZ built the "Update Forest" feature of the app. ZZ explored and analyzed the air pollution data (NYCCAS Air Pollution Rasters). LC and XL incorporated member contributions, and merged the final Shiny App product. AC and LC built the slide deck for in-class presentation.

All team members contributed to the GitHub repository and prepared the presentation. All team members approve our work presented in our GitHub repository including this contribution statement.

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is organized as follows.

```
proj/
├── app/
├── lib/
├── data/
├── doc/
└── output/
```

Please see each subfolder for a README file.

