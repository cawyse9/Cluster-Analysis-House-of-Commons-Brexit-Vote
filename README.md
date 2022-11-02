# Cluster Analysis: House-of-Commons Brexit Voting Patterns

<img align="right" width=300 src="https://user-images.githubusercontent.com/29300100/195382158-d33a0353-816e-4e42-becf-2d5412efd892.png">

## Background
The House of Commons in the UK held a series of indicative votes on Britain's withdrawal from the EU (Brexit) on March 27th and April 1st, 2019. A dataset giving the votes is available as open source data. Within the dataset, a matrix called "divisions" gives the outcome of the votes and a vector called "labels" gives the name of each vote.  The vote outcomes are recorded as "aye vote", "nay vote" and "absent" 

## Objectives
Finite mixture models were used to investigate the following whether the House of Commons politicians fall into clusters when voting on Brexit, and whether the politicians in each cluster tend to have similar voting behaviour

Further details of the analysis are [here](https://github.com/cawyse9/Cluster-Analysis-House-of-Commons-Brexit-Vote/blob/main/analysis/Brexit%20votes.pdf)
The R-code is [here](https://github.com/cawyse9/Cluster-Analysis-House-of-Commons-Brexit-Vote/blob/main/analysis/Brexit_cluster_v1.R)

## Conclusions
These analyses show that the House of Commons politicians do fall into at least 6 important clusters when voting on Brexit. In general, the latent class model seems to have identified realistic cluster of MPs from their patterns in voting. The model segregated the MPs into classes with different probability of wanting to leave the UK and then highlighted different patterns of voting between the classes.

## Acknowledgements
This project was submitted as part of the coursework required for the module, "Stochastic Models" for an a MSc (Data Analytics) course at UCD in 2022. 
