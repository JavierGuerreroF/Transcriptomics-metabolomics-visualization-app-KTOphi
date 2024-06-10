# Transcriptomics-metabolomics-visualization-app-KTOphi
Visualization App

This Shiny R app provides a comprehensive interface for visualizing and analyzing genomic, transcriptomic, and metabolomic data. The app has three main tabs, each dedicated to different types of analyses: Volcano Plots, Gene Search, and Metabolomics.
Installation

To run this app locally, you need to have R and Shiny installed. You can install Shiny by running the following command in R:

R

install.packages("shiny")

Clone this repository to your local machine:

bash

git clone https://github.com/yourusername/your-repo-name.git

Navigate to the app directory and run the app:

R

shiny::runApp("path/to/your/app")

Usage
1. Volcano Plots Tab
Screenshot:

Description:

This tab allows you to visualize differential expression analysis results through volcano plots.

    Reference genome (Organism): Choose between different organisms (e.g., Ophiostoma or KT).
    Analysis Options: Compare organism vs consortium, treatment effects (e.g., FaOH vs CTL), and time effects (early vs late).
    Conditions Selection: Filter the data based on specific treatments and time points.
    Volcano Plot Visualization: The plot displays log2 fold change vs -log10(p-value) with points colored by their differential expression status (upregulated, downregulated, or not significant).
    Values Selection: Adjust log fold-change and p-value thresholds to filter significant genes.
    Results Tables: Lists of over- and under-expressed genes with options to export the data.

2. Gene Search Tab
Screenshot:

Description:

This tab provides a detailed view of specific gene expression data.

    Gene Selection: Input a gene ID to display its expression data.
    Reference Genome (Organism): Choose the organism of interest.
    Selected Gene Barplot: Visualize the log2 fold change of the selected gene under different conditions.
    Values Selection: Adjust thresholds for log fold-change and p-value.
    Gene Information: Detailed information about the selected gene, including transcript ID, strand, start and end positions, GO terms, KEGG pathways, and domain information.

3. Metabolomics Tab
Screenshot:

Description:

This tab presents metabolomic data analysis results.

    Pie Chart Visualization: Displays the distribution of different classes of metabolites.
    Parameters: Adjust filters for peak height, sample selection, class selection, percent filter, and color palette.
    Frequency of Features: A table summarizing the frequency, proportion, and log10 peak height of metabolites within each class.

Contributing

Contributions are welcome! Please fork the repository and submit a pull request for any enhancements or bug fixes.
License

This project is licensed under the MIT License - see the LICENSE file for details.
