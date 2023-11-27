# Get data here: https://www.crossref.org/blog/news-crossref-and-retraction-watch/

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Load your data
data = pd.read_csv('data.csv')

# Check if 'RetractionDate', 'Country', and 'Reason' columns exist
required_columns = ['RetractionDate', 'Country', 'Reason']
for col in required_columns:
    if col not in data.columns:
        raise ValueError(f"Required column {col} not found in the dataset")

# Convert 'RetractionDate' to datetime and extract the year
data['RetractionYear'] = pd.to_datetime(data['RetractionDate'], errors='coerce').dt.year

# Define categories and their corresponding keywords
categories = {
    "Plagiarism": ["+Duplication of Article", "+Plagiarism of Data", "+Plagiarism of Text",
                   "+Euphemisms for Plagiarism", "+Euphemisms for Duplication",
                   "+Concerns/Issues about Referencing/Attributions", "+Taken from Dissertation/Thesis",
                   "+Duplication of Text"],
    "Data Issues": ["+Falsification/Fabrication of Data", "+Unreliable Data", "+Concerns/Issues About Data",
                    "+Error in Data", "+Original Data not Provided", "+Error in Data",
                    "+Duplication of Image", "+Manipulation of Images", "+Duplication of Data"],
    "Authors Issues": ["+Concerns/Issues About Authorship", "+Objections by Author(s)", "+Withdrawal",
                       "+Withdrawn (out of date)", "+False/Forged Authorship", "+False Affiliation", 
                       "+Complaints about Author"],
    "Ethical Violations": ["+Conflict of Interest", "+Breach of Policy by Author",
                           "+Misconduct - Official Investigation/Finding", "+Misconduct by Author", "+Ethical Violations by Author",
                           "+Lack of IRB/IACUC Approval", "+Concerns/Issues about Human Subject Welfare", "+Informed/Patient Consent - None/Withdrawn"],
    "External Error": ["+Duplicate Publication through Error by Journal/Publisher",
                       "+Lack of Approval from Third Party", "+Publishing Ban"
                       "+Transfer of Copyright/Ownership", "+Copyright Claims", "+Rogue Editor",
                       "+Complaints about Company/Institution", "+Complaints about Third Party"],
    "Review Issue": ["+Fake Peer Review", "+Paper Mill", "+Concerns/Issues with Peer Review", "+Concerns/Issues with Peer Review"],
    "Quality Issues": ["+Error in Text", "+Unreliable Results", "+Concerns/Issues About Results", "+Contamination of Materials",
                       "+Error in Analyses", "+Results Not Reproducible", "+Error in Results and/or Conclusions", "+Hoax Paper",
                       "+Bias Issues or Lack of Balance", "+Error in Materials", "+Randomly Generated Content", "+Doing the Right Thing"],
    "Investigations": ["+Investigation by Journal/Publisher", "+Investigation by Third Party",
                       "+Investigation by Company/Institution", "+Criminal Proceedings", "+Civil Proceedings",
                       "+Investigation by ORI", "+Legal Reasons/Legal Threats"]
}

# Step 1: Create new categories with counts for each article
for category, keywords in categories.items():
    data[category] = data['Reason'].apply(lambda x: any(keyword in x for keyword in keywords)).astype(int)

# Filtering data for the last 10 years
recent_data = data[data['RetractionYear'] >= data['RetractionYear'].max() - 10]

# Selected countries
selected_countries = ["China", "United States", "India", "Russia", "United Kingdom", "Japan"]
selected_countries.sort()

# Step 2: Group by country and calculate the top 8 reasons for each country
grouped_data = recent_data.groupby(['Country', 'RetractionYear'])[list(categories.keys())].sum()

# Function to get heatmap data for a country
def get_heatmap_data(country, df):
    country_data = df.loc[country]
    # Normalize to percentages
    total_counts = country_data.sum(axis=1)
    return (country_data.div(total_counts, axis=0) * 100).fillna(0)

# Step 3: Create heatmaps for each country
plt.figure(figsize=(30, 20))
sns.set(style="whitegrid", palette="muted", color_codes=True)

for i, country in enumerate(selected_countries, 1):
    plt.subplot(3, 3, i)  # Adjusted for the number of countries
    heatmap_data = get_heatmap_data(country, grouped_data)
    ax = sns.heatmap(heatmap_data.T, cmap='coolwarm', annot=True, fmt=".1f", linewidths=.5)
    ax.set_title(country, fontsize=16)
    ax.set_xlabel('Year', fontsize=14)
    ax.set_ylabel('Reasons', fontsize=14)
    ax.set_yticklabels(sorted(heatmap_data.columns), rotation=0, fontsize=10, va='center')

plt.tight_layout()
plt.show()