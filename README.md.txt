MarketSphere Customer Analytics Project

Overview
This project explores data-driven approaches to enhance marketing strategies for MarketSphere Inc., a leading retail company in the U.S. Through detailed statistical analysis and machine learning, our goal is to provide actionable insights into customer behavior, optimizing marketing efforts and improving overall business outcomes.

Dataset
The dataset used in this project comes from a Customer Personality Analysis and includes information on customer demographics, purchasing behaviors, and responsiveness to marketing campaigns. It features over 2,000 observations across multiple variables, such as 'Income', 'Kidhome', 'Teenhome', 'Recency', 'MntWines', and more.

Methodology
The project utilizes a variety of statistical and machine learning methods:
Regression Analysis: Used to predict customer spending on wines based on different demographic and behavioral factors.
Classification Models: Developed to predict customer responsiveness to marketing campaigns.
Ensemble Methods: Applied to improve predictions by combining several models to reduce variance and bias.

Key Findings
Influence of Purchase Channels: Analysis indicates that catalog and in-store purchases significantly boost wine sales.
Impact of Household Composition: Presence of children at home negatively impacts wine purchases, suggesting lifestyle and budgetary adjustments in these households.
Customer Segmentation: Different clustering techniques helped identify distinct customer segments, providing tailored marketing strategies.

Tools Used
R for statistical analysis and model building.
Python for additional data manipulation and analysis.
Various libraries such as ggplot2, caret, randomForest, and gbm.

Installation
Instructions for setting up the project locally:
git clone https://github.com/Ellacham/My_MSBA_Projects.git
cd My_MSBA_Projects


# Setup your environment
Rscript -e "install.packages(c('ggplot2', 'caret', 'randomForest', 'gbm', 'dplyr'))"

Usage
Scripts are divided by analysis type. For example, run the regression analysis:
Rscript regression_analysis.R

Contribution
Contributions are welcome! Please fork the project and submit a pull request with your improvements.

License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.


Contact
Your Name - Emmanuella Acheampong
Project Link: https://github.com/Ellacham/My_MSBA_Projects.git
