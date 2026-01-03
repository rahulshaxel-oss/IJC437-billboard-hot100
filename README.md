# IJC437-billboard-hot100
R-based analysis of Billboard Hot 100 songs (2000–2023) using PCA, clustering, and regression for IJC437 coursework.
# IJC437 – Billboard Hot 100 Music Analytics (2000–2023)

**Author:** Rahul Sharma Shanglakpam  
**Module:** IJC437  
**Tools:** R, tidyverse, PCA, k-means clustering, linear regression  

## 📌 Project Overview
This project analyses Billboard Hot 100 year-end songs (2000–2023) to understand
how musical features relate to commercial success. Using Spotify audio features,
the study explores long-term trends, stylistic clusters, and predictive relationships
between sound characteristics and chart performance.

## 🔍 Key Analyses
- Descriptive trend analysis of musical features
- Dimensionality reduction using PCA
- Sound-based clustering using k-means
- Regression modelling to predict chart success

## 📊 Key Findings
- Danceability and loudness have increased over time
- Valence (emotional positivity) has declined
- Four stable sonic archetypes emerged
- Audio features explain a small but meaningful portion of chart success

## 📁 Repository Structure
## ▶️ How to Run the Code

### Requirements
- R (version 4.2 or later)
- RStudio (recommended)

### Required R packages
tidyverse, lubridate, broom, scales

### Steps
1. Clone or download this repository.
2. Place the dataset `billboard_24years_lyrics_spotify.csv` in the project root directory.
3. Open RStudio and set the working directory to the project folder.
4. Open the file `R/analysis.R`.
5. Run the script from top to bottom.

### Outputs
- Cleaned dataset: `billboard_clean_2000_2023.csv`
- Tables: CSV files prefixed with `table_`
- Figures: PNG files saved in the `figs/` folder

All results are fully reproducible.
