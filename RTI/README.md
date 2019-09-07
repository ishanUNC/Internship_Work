# RTI International Internship Work

These are code snippets and other files from the two projects I worked on throughout my time at RTI International.

### First Project: Synthetic Population Generation

In this project, I developed a creative and efficient pipeline in Python and SQL to transform raw [American Community Survey Data](https://www.census.gov/programs-surveys/acs/data.html) into a newer version of RTI's [Synthetic Population](https://www.rti.org/impact/rti-us-synthetic-household-population™
). Essentially, there are scripts the load the tables into the appropriate format into a Postgres database, and then scripts that dynamically generate populations based on a set of variables that is requested.

Previously, the 2010 and 2013 populations were generated through much more tedious methods, and often used too few households to represent the whole population. Through my pipeline, RTI should be able to generate new Synthetic Populations significantly quicker (starting with 2017) while also improving the diversity and accuracy of the populations. This allows for interesting applications for government researchers such as modeling the spread of disease over time, simulating geographical student debt over time, and targeting at-risk locations for the opioid crisis.

**More details are found in my intern presentation in this repository.**

`RTI-International-Python-Code-Snippets.ipynb`: This contains code snippets of the pipeline, from loading tables into the database to the final generation of the synthetic population. 

`Syn_Pop_Preview.png` is a preview of what the final synthetic population looks like. There is location data down to the block group level, data for four variables (`age, race, size, income`) and a serial number that joins each household to a PUMS record, which is an individual-level data point that is a 5% sample of the US population.  

### Second Project: Evaluating Complexity of Neural Networks with Binary Response

In this project, I assisted RTI Fellow Dr. Georgiy Bobashev on research to find a reliable way to quantify the complexity of Neural Networks. The method proposed uses Generalized Degrees of Freedom, a concept outlined by Ye in [this paper](https://doi.org/10.1080/01621459.1998.10474094). I wrote R scripts that implemented the perturbation algorithms specified in the paper, but for a binary response.

In `single_pt_perturbation_nnet.R`, I use simulated data withg a small `n`. I then write a function that perturbs the binary response in each point and runs a neural net for each perturbation. Summing up the omega values for each run calculates the GDF of the model.

In `multiple_pt_perturbation_nnet.R`, I the same simulated data with a larger `n`. I then write a function that perturbs the binary response of a randomly selected set of multiple points (partitions) and runs a neural net for each perturbation. Summing up the omega values for each perturbation calculates the GDF of the model.

In `GDF_health.R`, I implement the multiple point perturbation method on a dataset where the predictors are binary variables and the response is a binary variable on whether someone is readmitted to a hospital. 

At the end of each of these files, graphs are generated showing the variation in GDF based on the number of hidden nodes and the maximum amount of iterations on each neural net run. Plots are stored in the `plots` folder in the same directory.

For example, in `plot5.png`, we can see that GDF increases steadily as the number of hidden nodes increase but then plateaus at a certain point when N = 200.

Dr. Bobashev hopes to put conclusions from my scripts on an upcoming research paper. 
