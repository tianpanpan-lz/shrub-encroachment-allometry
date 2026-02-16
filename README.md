# Grassland Shrub Encroachment, Light Competition, and Plant Diversity Analysis

This repository contains the code and data for the manuscript:  
**“Shrub encroachment reshapes grassland diversity via allometry-mediated light competition.”**

---

## 📁 Project Structure

- **`Shrub analysis.Rproj`**  
  RStudio Project file. Open this to launch the working environment for the project.

---

## 📊 Data Files

- **`Treatment.xlsx`**  
  Contains experimental treatment information for each plot (Block).  
  Block and Treatment were combined to create a unique identifier Block_Treatment for each plot.  
  This dataset is used as metadata to link each plot to its corresponding experimental treatment, enabling analysis of the effects of different treatments on light competition and shrub performance.

- **`MaquShrubPAR_Editor_2.xlsx`**  
  Contains photosynthetically active radiation (PAR) measurements shrubs at heights from 0 to 40 cm.  
  PAR was measured for different rounds (Round), plots (Block), and directions (Direction). The data were processed by averaging PAR at each height and log-transforming the values to calculate light asymmetry (Light Asymmetry). Light penetration (Light Penetration) was also calculated as the ratio Height0 / Height40.
  This dataset is used to analyze how different shrub treatments affect vertical light distribution, light competition intensity, and light asymmetry.

- **`Diversity_20250326.xlsx`**  
  Contains community diversity data across different plots (Block) and treatments.  
  For each plot, species richness (SR) and Pielou’s evenness (Pielou) were calculated. Treatments were organized as factors (Control, Shrub, RemovedShrub, ArtificialShrub) for statistical analysis.
  This dataset is used to assess how different shrub treatments affect plant community structure, including species richness and evenness.

  **`MaquShrubBiomass2024.xlsx`**  
  Contains aboveground biomass measurements per plot.  
  For each plot, the biomass of residual shrub material (RestlBiomass) was summed to calculate total plot-level biomass (PlotBiomass). Plots were uniquely identified by combining Block and Treatment. Treatments were organized as factors (Control, GenuineShrub, RemovedShrub, FakeShrub) for analysis.
  This dataset is used to evaluate the effects of different shrub treatments on community-level aboveground biomass.

  **`MaquTrait.xlsx`**  
  Contains cleaned and filtered plant trait data for all individuals in the experiment.  
  Data include height (Hm), individual biomass (IndividualBiomass), species identifier, treatment, and plot information. Only individuals with sufficient replication (≥5 per species per treatment) were used for standardized major axis (SMA) regression to calculate species-level scaling relationships.
  This dataset is used to compute species-specific height-biomass allometry, providing input for analyzing functional trait responses to different treatments.

  **`smaResult.xlsx`**  
  Contains results from standardized major axis (SMA) analyses of the height-biomass relationship.  
  For each species and treatment combination, the scaling exponent (slope) and intercept of log-transformed height vs. log-transformed individual biomass are recorded. Treatments are organized as factors (Control, Shrub, RemovedShrub, ArtificialShrub).
  This dataset is used to evaluate how different treatments influence species-level allometric patterns, including comparisons of scaling exponents and intercepts.

  **`CWM_FDis_data_20250326.xlsx`**  
  Contains community-weighted mean (CWM) and functional dispersion (FDis) metrics for all plots and treatments.  
  For each plot, CWM and FDis were calculated for both the scaling exponent and intercept derived from individual-level allometry. Plot identifiers include Block and Treatment. Treatments are organized as factors (Control, Shrub, RemovedShrub, ArtificialShrub).
  This dataset is used to analyze community-level allometry and functional trait distribution, examine correlations between CWM and FDis, and explore how treatments shape community trait structure and variation.

  **`MaquSoil.xlsx`**  
  Contains soil measurements for each plot, including SWC, pH, STC, STN, SAP, Block, and Treatment.  
  PCA was applied to reduce dimensionality; Soil_PC1 and Soil_PC2 capture major variation.
  Used for visualizing soil variation, analyzing correlations, and testing treatment effects on soil properties.

  **`MQcommunity.xlsx`**  
  Contains integrated plot-level data for structural equation modeling.  
  Includes community metrics (species richness SR, Pielou evenness), community-weighted mean and functional dispersion (CWM.Ex, CWM.In, FDis.Ex, FDis.In), soil properties (Soil_PC1), light asymmetry, and treatment/plot identifiers (Block, Plot).
  Used to analyze direct and indirect effects of soil, light, and community traits on diversity and functional structure across experimental treatments.
---

## 🧪 Code Files

- **`Shrub analysis.R`**  
  Main R script used in the manuscript.  
  Performs data import, cleaning, statistical analysis and visualization.  
  This script generates the main results and figures presented in the paper.



## ▶️ How to Use

1. Open `Shrub analysis.Rproj` in RStudio.
2. Ensure the data file is in the same directory as the R scripts.
3. Run `Shrub analysis.R` to reproduce the analyses and generate figures used in the manuscript.
