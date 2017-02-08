Visualizing the CTMC and SAOM fitting process
================

This outline exists to help me get my thoughts straight with respect to my process of visualizing the SAOM fitting process and the underlying CTMC for network change. Why am I doing this? What am I going to do? What have I already done? What do I want to accomplish?

Outline
=======

1.  Visualize the CTMC
    1.  Why? - to better understand the model. to see what's going on underneath. to see how the objective function affects tie changes.
    2.  How? - write a function to simulate from time 1 to time 2 given parameter values that stores the network at each step, the time between each step, and any covariate values. visualize the one-tie-at-a-time changes in a movie, facets, any other way. use color or shape changes to see how the covariate levels affect the changes.
    3.  What? - this will become part of the "removing the blindfold" chapter. introduction and first sections

2.  Visualize the fitting process
    1.  Why? - to better understand the updating process. to see what's influencing the parameter estimates most.
    2.  How? - write a function that mimics the `RSiena` fitting process for Method of Moments OR figure out how to extract the parameter updates from the `sienafit` object. visualize the chains of parameter changes. look at how different parameter values for starting values affect the estimations.
    3.  What? - this will become part of the "removing the blindfold" chapter. Maybe model-in-data-space section.

Ideas from "Visualizing Statistical Models: Removing the Blindfold"
===================================================================

1.  Introduction
    1.  Talk about the toolbox of visualizations. We must answer the question, "What is in the network visualization toolbox?" and use those tools and more moving forward.

2.  Background
    1.  Model family - the SAOM is the model family
    2.  Model form - which *β* effects get included
    3.  Fitted model - estimates of rate parameters and *β*s resulting from `RSiena` fit
    4.  Question: Can we find a direct interpretation of the SAOM *β* parameters? The *β*s are in the model space. How can we take them from the model space to the data space?
    5.  Question: How can we visualize these networks interactively??

3.  Display the Model in Data-Space
    1.  What does visualizing the model in the data space mean for SAOMs?
        1.  Idea: take the `sims` output object from the SAOM model fit. Create adjaceny matrices from them, tile them in `geom_tile`, shading by frequency of specific ties in simulations, then overlay the squares that are in the data that was being simulated.
        2.  Idea: focus on the probabilities of individual tie changes. How do close probabilities affect overall model behavior??

4.  Collections are more informative than singletons
    1.  We've already done some of this - densities of fitted estimates from `RSiena`.
    2.  Look at distributions of fitted network statistics (corresponding to stats in model form) by the fitted parameter values. I also think we've already done some of this.
    3.  Other ideas????

5.  Do not just look at the final result; explore how the algorithm works
    1.  Idea: We've already discussed visualizing the microsteps from current time to the next time point.
    2.  Idea: Look at the chains for updating the parameter estimates -- use multiple starting values.
