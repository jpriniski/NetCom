# Network Communication Experiments
Repository for experimental software, statistical analysis, computational modeling, and behavioral data used in ongoing network experiments on group communication. This README.md provides a roadmap for the software and data stored in this repository. Directories will have additional README.md files to help with using the software. Please consult the following publications for additional information on these experiments and findings. 

- Priniski, J. H., Linford, B., Krishna, S., Hirschman, A., Rodriguez, N., Morstatter, F., Brantingham, J., & Lu, H. (2024). Cognitive complexity and neighborhood structure shape narrative interaction in online social networks. (Under Review). Manuscript available upon request. 
- Priniski, J. H., Linford, B., Krishna, S., Morstatter, F., Brantingham, J., & Lu, H. (2024). Online network topology shapes personal narratives and hashtag generation. Proceedings of the Cognitive Science Society. [Article PDF](https://escholarship.org/uc/item/6pv4z0j5)

### Statistical Analyses
This directory contains Bayesian GLMs and ggplot figures that visualize and model group-level behaviors and narrative shifts resulting from interactions under different network conditions. These GLMs include predictors that test hypotheses on how the cognitive complexity of interacting over various digital media mediates the impact of a group's network structure on its dynamics.

<p align="center">
  <img width="532" alt="group_dynamics" src="https://github.com/user-attachments/assets/dc48b00f-14b8-4263-9569-632eab690d52">
</p>

### Experimental Data 
Raw and processed data from all experimental runs is in this directory. Data carpentry scripts are available as Jupyter Notebooks, which transform the OTree dataframes (one per experimental run) into long format and concatenate them across all runs for statistical analysis and hypothesis testing.

<img width="1100" alt="colormaps" src="https://github.com/user-attachments/assets/f8bbadd3-a435-494d-a1c7-af3a8b4ffa1e">

### Network Experiment Software 
This directory contains the Python (OTree) and JavaScript software for running a network experiment on group communication. There are two folders, face_experiment and hashtag_experiment, which ask participants to communicate and align responses using different digital media. The README.md in this directory provides instructions for setting up a server to host the experiment, which you can customize and extend with minimal Python and JavaScript expertise. Below is a high-level overview of the experimental procedure. The pre and postinteraction phases were administered using a Qualtrics survey, which linked participants to the network server (OTree python code). The code for each of these experimental phases can be found in this directory. 
![Networks Overview Figure](https://github.com/user-attachments/assets/b2541a97-ad18-48e0-be8a-57eed74b318c)


