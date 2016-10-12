# SAOM-removing-blindfold
Removing the blindfold from Stochastic Actor-Oriented models. 

## Outline 

1. Simulate from SAOM and view the CTMC underneath.
    - Use the `ndtv` package.
    - Write my own simulation function? or
    - Figure out how to extract intermediate steps from `RSiena`
2. Simulate from many different models and create "average" networks
    - Current definition of average: based on number of times an edge appears in N simulations
    - Other definitions of average? Distance metrics? Come up with something
3. How to visualize the distribution of networks from a model? 
    - Define the "distribution" 
    - Is there a measure of variance? 
    - How to view several networks at once? 
4. Clustering
    - Principal Components Analysis: need distance metrics from 2, see paper on hypothesis testing for networks
    - Other distance metrics? (again) Weighted metric by size of network? 
