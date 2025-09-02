# pandemic-music-shift-analysis

Analysis of global music trends before and after COVID-19 using **time series forecasting**, **clustering**, and **NLP-based sentiment analysis**. Explores shifts in Spotify audio features (valence, danceability, energy) and lyrical emotions, utilizing machine learning, text mining, and data visualization techniques.


---

## Project Overview

This project investigates how the COVID-19 pandemic influenced music preferences, including changes in genres, song characteristics, and lyrical themes. Using Spotify and Genius datasets, we analyzed trends before, during, and after the pandemic to uncover shifts in listener behavior and emotional expression in popular music.

Key questions addressed:  
- How did COVID-19 affect the popularity of music genres?  
- Did musical features such as energy, danceability, tempo, and valence change during and after the pandemic?  
- How did lyrical content and sentiment evolve in response to global events?  

---

## Repository Structure

```text
pandemic-music-shift-analysis/
├── README.md
├── docs/
│   ├── final_Report.pdf
│   └── presentation.pptx
├── data/
│   ├── raw_data_subset.csv
│   └── cleaned_data_subset.csv
└── code/
    ├── data_preprocessing.R
    └── Analysis_TimeSeries_Clustering_NLPSentiment.R

```

---

## Data
- **raw_data_subset.csv** – Subset of the original Spotify and Genius datasets (used for reproducibility).
- **cleaned_data_subset.csv** – Preprocessed and merged data ready for analysis.
- Note: Full datasets are not included to keep the repository lightweight.

---

## Code
- data_preprocessing.R – Scripts for cleaning and merging datasets.
- Analysis_TimeSeries_Clustering_NLPSentiment.R – Performs time series analysis, clustering, and sentiment analysis on music data.

---

## Key Skills Utilized
- Time Series Analysis: Detecting disruptions and trends in musical features over time.
- Clustering Techniques: K-Means, Gaussian Mixture Models, and Hierarchical Clustering to segment songs and listeners.
- Text Mining & Sentiment Analysis: Analyzing lyrical content using Bag-of-Words, TF-IDF, and lexicons (Bing, NRC, AFINN).
- Data Preprocessing & Cleaning: Handling large datasets and preparing data for advanced analytics.
- Visualization: Generating insightful plots to showcase trends and patterns in music.

---

## Key Insights
- Valence (Mood): Positive sentiment in songs increased post-COVID.
- Danceability & Energy: Declined during the pandemic and partially rebounded after.
- Clustering: Two main listener-preference tracks emerged – calm/acoustic vs. high-energy/upbeat.
- Lyrics: Shift toward introspective, emotionally authentic themes with higher listener engagement.

---

## Acknowledgements

Special thanks to **Ria, Nicholas, Liao, and Celia** for their support, feedback, and collaboration throughout this project. Your insights and guidance were invaluable in shaping the analysis and presentation of this work.





