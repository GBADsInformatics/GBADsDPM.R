# GBADsDPM.R

<img src= "https://github.com/GBADsInformatics/GBADsDPM.R/blob/main/img/GBADs.png" width="150">

Welcome is the GBADsDPM (Global Burden of Animal Diseases Dynamic Population Model) R package repository!

## The Dynamic Population Model (DPM)

GBADs runs a novel compartmentalized equation-based model called the Dynamic Population Model (DPM) to compute the AHLE (Animal Health Loss Envelope) --- the difference between ideal and current levels -- used to estimate production losses in livestock such as cattle, poultry, and small ruminants within developing countries like Ethiopia. Such losses can include milk/egg production, breeding stock, meat, manure, *etc*. The DPM is structured according to age (juvenile, sub-adult, and adult) and sex (male and female) and simulations are run in monthly (for small ruminnats and cattle) or weekly (for poultry) timesteps up to one year.

The model is expressed mathematically according to a generic livestock production system functioning in both the presence and the absence of disease as follows:

```math
Y = F(z, \theta) \times [1 - L(b_0(1 - C(x, \theta)))]
```
where 

- $Y$ is the total output of the production system
- $F(z, \theta)$ is the production function for a set of ordinary inputs $z$ (animals, feed, labour, *etc.* required to generate $Y$ in the absence of disease) and exogenous parameters $\theta$ (such as climate, regulations, *etc.*)
- $L$ is a loss function ($0 \leq L \leq 1$) describing the action of disease hazards $b_0$ on the production of $Y$
- $C(x, \theta)$ is a control function ($0 \leq C \leq 1$), increasing in control inputs $x$ (vaccines, antibiotics, *etc.*) which mitigates the effects of $b_0$.

When $b_0 = 0$, this is the disease free case and there is no loss of output, such that $L(0) = 0$, and ideal production, $Y = F(z, \theta)$ is achieved. This is termed the "ideal health" case and is denoted $Y^*$.

Under these conditions, the burden of disease is therefore found in the combined value in lost output when $Y$ is less than $F(z, \theta)$, plus any expenditure on control where $z > 0$.

The AHLE is derived via a cost minimization as:

```math
AHLE = \sum_{i=1}^{I}\left(\sum_{k=1}^{K}p_k(z_{ik} - z_{ik}^*) + q_{i}x_{i} + s_{i}({n_i}_t - n_{i}^*)_t - \sum_{j=1}^{J}r_j(y_{ij} - y_{ij}^*)\right)
```

## Required R packages

The DPM requires several packages to run successfully:

- ```mc2d``` # for 2D Monte Carlo simulations
- ```rtruncnorm``` # for truncated Normal distribution
- ```yaml```  # for importing YAML files
- ```tools```  # for processing

which can be loaded as follows:

    install.packages("mc2d") 
    install.packages("truncnorm") 
    install.packages("yaml")
    install.packages("tools")

    library(mc2d)
    library(truncnorm)
    library(yaml)
    library(tools)

In the ```load.R``` file, a user can either specify a file path manually, or navigate to a file from a pop-up window. For the latter option, an additional package is needed:

    install.packages("rstudioapi")
    library(rstudioapi)

## Package Functions

The GBADs R package comprises three main functions:

1.    ```read_params(file_path, file_type)``` reads in a DPM/AHLE model parameter file in YAML format into a user's R workspace based on a specied ```file_path``` telling R where files are located on a user's system.
2.    ```rpert(n, min, mode, max, lambda = 4)``` generates ```n``` random variates within a given range from the PERT distribution based on specified minimum, maximum, and modal values, where ```x_min``` $<$ ```x_mode``` $<$ ```x_max```. Setting ```lambda = 4``` controls the shape (*i.e.*, flatness) of the distribution, effectively placing more or less weight on certain values. Note, you should not have to alter the value of `lambda`.
3.   ```run_compartmental_model()``` runs a simulation of the DPM/AHLE for a scenario of interest using parameters found in the YAML file and saves results to the user's working directory. Output s generated using 10000 simulations runs by default. These can then be used to compute the AHLE.
4.  ```setup(file_path, seed_value = NULL, parallel = FALSE)``` is a wrapper that loads DPM/AHLE parameters contained in YAML files and runs the compartmental model all at once. The user need only point to the required folders and files interactively through a pop-up window. In addition, a random seed can be set for reproducibility. By default, no seed is used. Parallelization (parallel = TRUE) can be employed to speed up simulatons.

## How to Run

### Via R/Rstudio

Currently, there are two different ways to run the DPM in R/RStudio, depending on which version is used. The most recent version (Version 2) is recommended.

1. Download the code from GitHub.

#### Version 1 (Old)

2. Run the `load.R` script. This installs and loads required packages and sources necessary functions and then runs the model. The packages may need to be installed first. You will need to change the working directory to correspond to your system. Additionally, you can set the `seed_value` to an integer of any length to ensure code reproducibility. This is useful if you don't plan on doing everything in one sitting. 

#### Version 2 (Old)

2. Run the ```setup(file_path = file_path, seed_value = seed_value)``` function with a random seed, `seed_value` (if desired). A filepath, `file_path`, pointing to a folder contsining the YAML parameter files to be analyzed should also be specified.

#### Versions 3, 4, 7 and 7.2 (Current)

1. Create a folder containing the desired YAML files to be processed.

2. Run the `load.R` script. This will prompt the user to interactively select the created folder in which model output will be saved. Presently, a single CSV file is outputted displaying either (1) simulation results for all runs for each variable of interest (using the argument `output = "cumulative total"`, or (2) summary statistics (minimum, 1st quartile, mean, median, 3rd quartile, maximum, and standard deviation) using the argument `output = "summary"`.

### Via the Command Line (Terminal)

Users familiar with the command line (*e.g.*) Mac Terminal can run the DPM using the ```DPM_CommandLine.R``` script.

The main advantage of this is that certain parameters required to set up a model run do not need to be explicitly hard-coded beforehand.

To run, a user needs to only specify four arguments:

1. The file path
2. The seed value (for reproducibility)
3. The output format (either ``summary`` or ```cumulative total```)
4. Whether parallelization is desired (currently, only serialization is implemented)

In the Terminal:

1. Set the working directory using ```cd``` (change directory)
2. Run ```DPM_CommandLine.R "path/to/file" 123 summary FALSE```, replacing ```path/to/file``` with the correct file path pointing to the folder where YANML files are located, and ```123``` with the desired seed value.

### Notes

The package should be run in RStudio.

Within the `load.R` script, for now, a user will need to set their working directory via `setwd()` to point to the folder which was downloaded from GitHub.

Old DPM versions can be found in the R/Old subfolder, These should be used at your own risk.

## More Informatiom

Further useful information on the DPM and the AHLE can be found here: https://gbadske.org/dashboards/ahle-casestudy/. 

In due course, the GBADsDPM package will be submitted to the Comprehensive R Archive Network (CRAN) for global uptake.

## References

Gilbert, W., Marsh, T.L., Jemberu, W.T., Chaters, G., Bruce, M., Steeneveld, W., Alfonso, J.S., Huntington, B. and Rushton, J. (2024) Quantifying cost of disease in livestock: A new metric for the Global Burden of Animal Diseases. *Lancet Planet Health*, 8: e309-317.
