# Open Code and Data 

This repository contains experimental data and statistical analysis scripts for replicating the results in the paper: 

Priniski, et al. (2025). Network structure shapes consensus dynamics through individual decisions. _PNAS_.

Please refer to the following data files, Jupyter Notebooks, and R scripts listed. Subdirectories are formatted as headers, with important files in that directory listed as bullet points. The main replication files are marked with a ⭐️, and are `InteractionDynamics.R`, `Focal Narrative Alignment.ipynb`, `NarrativeShifts.R` (for modeling experimental data), and `SimulationCode.ipynb` (running the agent-based models). For more detailed instructions on running these scripts, consult comments in the notebooks and R scripts. 

### Data
- `all_interaction_data.csv`: long-formatted dataframe of all interactions. (Analysis scripts based on this dataframe)
- `all_tweets.csv`: long-formatted dataframe of all pre- and post-interaction personal narratives. (Analysis scripts based on this dataframe.)
- Network Interactions: long-formatted .csv files for individual network runs. File names: f20h1.csv means Face N = 20 Homogeneous Run 1. 
- Pre- and Post Data: wide-formatted .csv files of personal narratives and pre-/post- interaction hashtags. Pre- and Post-interaction hashtags were not analyzed in this experiment. File names: f20h1.csv means Face N = 20 Homogeneous Run 1. 

### Data Analysis
- Network Interaction
  - ⭐️ `InteractionDynamics.R`: R script for analyzing interaction data (Figure 2)
  - `models.zip`: Statistical models fit in R for interaction analysis. Load these models in when replicating InteractionDynamics.R to reduce run-time for fitting Bayesian models.
  - ⭐️ `Focal Narrative Alignment.ipynb`: Jupyter Notebook for running the narrative alignment analysis in the paper (Figure 3A)
- Personal Narratives
  - `all_tweets.csv`: same file as above, just saved locally for running script. 
  - `claims.csv`: causal claims extracted by the Causal Claims Transformer. The Causal Claims Transformer can be accessed at the following Hugging Face [repo](https://huggingface.co/jpriniski/Causal-Claims-Transformer/blob/main/README.md)
  - ⭐️ `NarrativeShifts.R`: R Script for analyzing the personal narrative changes.


### Experimental Software
- Pre- and Post Qualtrics Wrappers. This directory contains the qualtrics wrappers around the OTree network interaction experiments that are used for Phase 1 and Phase 3 of the experiment.
- Network Interaction OTree Software. 
  - face_experiment: OTree Code for the Name Game condition
  - hashtag_experiment: OTree code for the Hashtag Game condition
  - `generate_trials.py`: Python code for generating interaction pairs. The trials used in the experiment are in custom_networks directories of the face and hashtag experiment.

### Simulations
- ⭐️ `SimulationCode.ipynb`: Run this Jupyter Notebook to simulate the Context Aware Agents. All other code and files are helper functions. 

