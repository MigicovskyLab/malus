# Malus CWR Species Distribution Modeling & Conservation Analysis

This repository contains a full analytical workflow for modeling the distribution of North American *Malus* crop wild relatives (CWRs), projecting habitat suitability under climate change, and evaluating conservation outcomes using gap analysis and niche comparisons.

---

## Overview

This project integrates:

- Occurrence data acquisition, cleaning, and thinning
- Background extent definition (ecoregion-based)
- Species Distribution Modeling (MaxEnt via ENMeval)
- Climate projections (SSP245 and SSP585)
- MESS (extrapolation) analysis
- PCA-based niche comparisons
- Niche equivalency testing (custom implementation)
- In situ conservation gap analysis
- Publication-quality visualization


### Study taxa

- *Malus coronaria*
- *Malus fusca*
- *Malus ioensis*
- *Malus angustifolia*
- *Malus* sect. *Chloromeles*

---

## About This Folder

This folder contains all scripts used in the SDM, niche, and gap analysis workflow.

Scripts are modular but generally follow this order:

1. occ_clean.R → clean raw data  
2. occ_thin.R → thin occurrences  
3. malus_bg.R → define background  
4. malus_sdm.R → run SDMs  
5. malus_MESS.R → extrapolation  
6. malus_pca.R → niche analysis  
7. malus_gap_v2.R → conservation analysis

---

## Workflow

1. **Download & clean occurrence data**
2. **Thin occurrences to reduce spatial bias**
3. **Define accessible area (background)**
4. **Train SDMs (MaxEnt)**
5. **Project to future climates (SSP245, SSP585)**
6. **Assess extrapolation (MESS)**
7. **Compare niches (PCA + equivalency tests)**
8. **Perform gap analysis**
9. **Summarize and visualize outputs**

---

## Script Descriptions

### Data Acquisition & Cleaning

- **`gbif_occ_local.R`**  
  Downloads occurrence data directly from GBIF using `rgbif`. :contentReference[oaicite:0]{index=0}  

- **`gbif_occ.R`**  
  Loads previously downloaded GBIF occurrence CSVs. :contentReference[oaicite:1]{index=1}  

- **`occ_clean.R`**  
  Cleans occurrence data using spatial filters and coordinate validation. :contentReference[oaicite:2]{index=2}  

---

### Occurrence Processing

- **`occ_thin.R`**  
  Spatially thins occurrence points to reduce sampling bias. :contentReference[oaicite:3]{index=3}  

- **`occ_plot.R`**  
  Generates maps of thinned occurrences for visualization and publication. :contentReference[oaicite:4]{index=4}  

---

### Environment & Setup

- **`libraries.R`**  
  Loads required packages for the main workflow. :contentReference[oaicite:5]{index=5}  

- **`librariesTyler.R`**  
  Alternative library setup with additional tools. :contentReference[oaicite:6]{index=6}  

- **`load_maps.R`**  
  Downloads and prepares base spatial layers (GADM, Great Lakes, etc.). :contentReference[oaicite:7]{index=7}  

---

### Background Selection

- **`malus_bg.R`**  
  Defines species-specific background extents using CEC Level II ecoregions. :contentReference[oaicite:8]{index=8}  

---

### Species Distribution Modeling

- **`malus_sdm.R`**  
  Core SDM workflow:
  - MaxEnt modeling via `ENMeval`
  - Model tuning (regularization + feature classes)
  - Spatial partitioning (checkerboard)
  - Climate projections (historical + future)
  - Model selection (AICc + Boyce index)  
  :contentReference[oaicite:9]{index=9}  

- **`functions.R`**  
  Custom helper functions including a modified Boyce Index (`twsBoyce`). :contentReference[oaicite:10]{index=10}  

---

### Climate Extrapolation

- **`malus_MESS.R`**  
  Computes MESS surfaces to identify novel climate conditions in projections. :contentReference[oaicite:11]{index=11}  

---

### Niche Analysis

- **`malus_pca.R`**  
  Performs PCA-based niche comparisons across species using `ecospat`. :contentReference[oaicite:12]{index=12}  

- **`pca_and_gap_functions.R`**  
  Custom functions for:
  - Niche equivalency testing (permutation-based)
  - Modified niche overlap plotting (`ecospat.plot.niche.pair`)  
  :contentReference[oaicite:13]{index=13}  

- **`run_malus_pca_equiv.R`**  
  HPC-compatible script for running pairwise niche equivalency tests. :contentReference[oaicite:14]{index=14}  

- **`RUN_PCA.sh`**  
  SLURM job script for parallel HPC execution of PCA equivalency tests. :contentReference[oaicite:15]{index=15}  

---

### Gap Analysis

- **`malus_gap_v2.R`**  
  Main gap analysis workflow calculating:
  - SRSin (Species Representation)
  - GRSin (Geographic Representation)
  - ERSin (Ecological Representation)
  - FCSin (Future Conservation Score)  
  :contentReference[oaicite:16]{index=16}  

- **`malus_gap_occ_count.R`**  
  Calculates occurrence-based conservation metrics. :contentReference[oaicite:17]{index=17}  

- **`extract_refugia_pa_names.R`**  
  Identifies protected areas that remain suitable under future climate scenarios. :contentReference[oaicite:18]{index=18}  

---

### Summarization & Visualization

- **`malus_area_latshift.R`**  
  Summarizes habitat suitability changes across time and scenarios. :contentReference[oaicite:19]{index=19}  

- **`gap_plot.R`**  
  Produces multi-panel gap analysis figures. :contentReference[oaicite:20]{index=20}  

- **`sdm_plot.R`**  
  Generates publication-quality SDM maps (Mollweide projection, graticules, scale bars). :contentReference[oaicite:21]{index=21}  

- **`sdm_response.R`**  
  Plots variable importance and response curves for SDMs. :contentReference[oaicite:22]{index=22}  

---

### Model Diagnostics

- **`var_collinear_test.R`**  
  Assesses predictor collinearity using Pearson correlation (for reporting, not model constraint). :contentReference[oaicite:23]{index=23}  

---

## Key Dependencies

- Spatial: `terra`, `geodata`, `wdpar`
- SDMs: `ENMeval`, `predicts`, `rJava`
- Niche analysis: `ecospat`, `ade4`
- Data wrangling: `tidyverse`
- Parallelization: `parallel`, `doParallel`

---

## Outputs

- Habitat suitability rasters (historical + SSP scenarios)
- Thresholded suitability layers (low / moderate / high)
- MESS maps
- PCA niche plots
- Niche overlap and equivalency metrics
- Gap analysis metrics (CSV + plots)
- Publication-quality SDM figures

---

## Notes

- Background extents are defined using **ecoregions containing occurrences**
- Suitability thresholds are based on **occurrence-derived percentiles**
- Gap analysis is restricted to **historically suitable habitat**
- PCA niche comparisons use **shared environmental space**
- Niche equivalency tests follow a **permutation-based framework**

---
