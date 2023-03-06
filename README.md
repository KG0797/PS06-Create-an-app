# PS06-Create-an-shiny-app

Overview: 

This assignment creates a Shiny app that allows users to explore a dataset of nutritional facts for common foods. The app has three tabs:

1.  **"About"**: This tab provides an overview of the dataset and displays a small sample of the data.

2.  **"Plots"**: This tab allows users to select a category of food (e.g. "Dairy") and a range of calories, and then displays a barplot of the foods in that category that fall within the selected calorie range. Users also able to change the visual set of the barplot, changing the color.

3.  **"Tables"**: This tab allows users to select a nutrient (e.g. "Protein") and displays a table of the top 10 foods in the dataset for that nutrient.


This app reads in a CSV file of the dataset and uses the following packages: shinyWidgets, shiny, rsconnect, dplyr, ggplot2, and readr.

