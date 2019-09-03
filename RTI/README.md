# RTI International Internship Work

These are code snippets and other files from the two projects I worked on throughout my time at RTI International.

### First Project: Synthetic Population Generation

In this project, I developed a creative and efficient pipeline in Python and SQL to transform raw [American Community Survey Data](https://www.census.gov/programs-surveys/acs/data.html) into a newer version of RTI's [Synthetic Population](https://www.rti.org/impact/rti-us-synthetic-household-population™
). Essentially, there are scripts the load the tables into the appropriate format into a Postgres database, and then scripts that dynamically generate populations based on a set of variables that is requested.

Previously, the 2010 and 2013 populations were generated through much more tedious methods, and often used too few households to represent the whole population. Through my pipeline, RTI should be able to generate new Synthetic Populations significantly quicker (starting with 2017) while also improving the diversity and accuracy of the populations. This allows for interesting applications for government researchers such as modeling the spread of disease over time, simulating geographical student debt over time, and targeting at-risk locations for the opioid crisis.

**More details are found in my intern presentation in this repository.**

`RTI-International-Python-Code-Snippets.ipynb`: This contains code snippets of the pipeline, from loading tables into the database to the final generation of the synthetic population. 

`Syn_Pop_Preview.png` is a preview of what the final synthetic population looks like. There is location data down to the block group level, data for four variables (`age, race, size, income`) and a serial number that joins each household to a PUMS record, which is an individual-level data point that is a 5% sample of the US population.  


