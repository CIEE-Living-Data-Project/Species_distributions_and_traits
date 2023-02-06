#  Linking species distributions and traits to understand ecosystem functioning
 The proposed working group aims to answer: Can we use species distributions and traits data to understand ecosystem functioning? The recent increase in openly accessible species distribution data from monitoring programs and aggregated traits databases are enabling our understanding of how ecosystems respond to global changes. However, a key challenge in ecology is establishing the mechanistic links between traits and ecosystem functions, such as food production, carbon sequestration, and ecosystem stability. The proposed working group will bring together ecologists from different fields to synthesize existing theories and develop a framework that classifies traits according to their ecosystem functions. We will use this framework to develop and test hypotheses that would relate species distributions, traits, and ecosystem functions using empirical data. As a case study we will focus on the plankton community of the Strait of Georgia; a data-rich water body of ecological, societal, and economical importance for Canada. Implementing the working group’s framework will facilitate the understanding of how changes in food web production in the Strait of Georgia are driven by changes in the functional composition of zooplankton. The output of the working group will be extendable to other ecosystems, both aquatic and terrestrial.
 
 This repository contains the zooplankton trait dataset organized by P. Pata. The dataset is in beta version and is currently being prepared for publication and official release. This is stored inside the data/ folder, which also contains zooplankton and oceanography data for the Strait of Georgia from various open-access sources. The tables/ folder contains some necessary files used in data curation. The functions/ folder contains some functions specifically coded to aid in data curation and analysis for this project. The functions/R_package_check.R file lists all the required R packages that would be used in the project. If the packages are not yet located in the local R library, these will be downloaded. Please update this list when necessary.
 
 The scripts/ folder currently contains some scripts for exploring the various datasets as R Markdown files. The files are numbered, and we recommend running and testing each. Figures and tables are displayed below chunks in the Markdown documents and if you wish to save them, please export these to the figures/ and tables/ folders.

[![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg
