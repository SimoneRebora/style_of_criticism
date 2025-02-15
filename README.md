# Style of Criticism

This repository contains the scripts for the paper "Measuring the style of Italian literary criticism: a comparison between professional and non-professional book reviews", to be published on *Romanica Cracoviensia*.  

## Dataset

Because of copyright and privacy issues, I cannot share the corpus that I used for the analyses. The "corpus_clean" folder only contains placeholder files.  
The main statistics of the analyzed corpus can be found here:  

```
                                     |  Social Reading  |  Paper Magazines
-------------------------------------|------------------|-------------------
Source                               |  aNobii          |  Sole 24 Ore
Total number of reviews              |  3781            |  1247
Total number of tokens               |  609514          |  665185
Length of shortest review (tokens)   |  30              |  35
Length of longest review (tokens)    |  2230            |  1843
Mean length of reviews (tokens)      |  161.2           |  533.4
Standard deviation of review length  |  182.5           |  349.6

```

## Instructions

Install all packages listed in the `requirements.txt` file.  
All .R scripts should be run by using [RStudio](https://posit.co/download/rstudio-desktop/). Scripts should be run in the following oder:  
1. `stylo_analysis.R`  
2. All the other scripts, as they read the results produced by the first one

