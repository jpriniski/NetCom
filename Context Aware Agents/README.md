This directory contains:

Directories:
* `Alpha Optimization`: Contains the four .csv files of information corresponding to each of the four experimental conditions that were used to create the figure included in the manuscript. Also contains the .ipynb file with the code used to create the figure, as well as a .png of the figure.

* `datafiles`: Contains all of the original .csv files used to do the calculations needed, these are a bit processed from the original to add some columns for decision type info.
    * Naming convention: 'prior initial/network size/network type initial.csv'

* `Entropy Optimization`: Contains the .csv files of data used to create the entropy comparison figure, a .png of the two figures (Entropy direct comparison and distance) and the .ipynb file containing the Python code to create the figure.

* `pairfiles`: Contains the files used to get pairings for the simulated spatially embedded networks.
    * Naming convention: 'connection_network size.number of rounds.network size_unique identifier.csv'
    
Code Files:
* `ComputedVals.ipynb`: Contains code that computes necessary statistics based on experimental dataframes; is used in other notebook files.
* `SimPreliminaries.ipynb`: Contains all of the preliminary functions for running the simulations and functions that are running behind-the-scenes, as well as plotting functions.
* `SimulationCode.ipynb`: Contains the current simulation code for both the Centola simulation and the CAA simulation.