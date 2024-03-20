# GBADsDPM.R

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
AHLE = \sum_{i=1}^{I}\left(\sum_{k=1}^{K}p_k(z_{ik} - z_{ik}^*) + q_{i}x_{i} + s_{i}({n_i}_t - n_{it}^*) - \sum_{j=1}^{J}r_j(y_{ij} - y_{ij}^*)\right)
```

## Package Functions

The GBADs R package comprises three main functions:

1.    ```read_params(file_path, file_type)``` reads in a DPM/AHLE model parameter file in YAML format into a user's R workspace based on a specied ```file_path``` telling R where files are located on a user's system.
2.    ```rpert(n, min, mode, max, lambda = 4)``` generates ```n``` random variates within a given range from the PERT distribution based on specified minimum, maximum, and modal values, where ```x_min``` $<$ ```x_mode``` $<$ ```x_max```. Setting ```lambda = 4``` controls the shape (*i.e.*, flatness) of the distribution, effectively placing more or less weight on certain values. Note, you should not have to alter the value of `lambda`.
3.   ```run_compartmental_model()``` runs a simulation of the DPM/AHLE for a scenario of interest using parameters found in the YAML file. Outputs a CSV file of summary statistcs fr quantities of interest (minimum, 1st quartile, median, mean, third quartile, maximum, and standard deviation) to the user's working directory. ReTurned values correspond to month 12 over 10000 simulations by default. These can then be used to compute the AHLE.
4.  ```setup(seed_value)``` is a wrapper that loads DPM/AHLE parameters and runs the compartmental model all at once. The user need only point to the required folders and files interactively through a pop-up window.

## How to Run

Currently, there are two different ways to run the DPM, depending on which version is used. The most recent version is recommended.

1. Download the code from GitHub.

### Version 1

2. Run the `load.R` script. This installs and loads required packages and sources necessary functions and then runs the model. The packages may need to be installed first. You will need to change the working directory to correspond to your system. Additionally, you can set the `seed_value` to an integer of any length to ensure code reproducibility. This is useful if you don't plan on doing everything in one sitting. 

### Version 2

2. Run the ```setup(file_path = file_path, seed_value = seed_value)``` function with a random seed, `seed_value` (if desired). A filepath, `file_path`, pointing to a folder contsining the YAML parameter files to be analyzed should also be specified.

### Note

The package should be run in RStudio.

## More Informatiom

Further useful information on the DPM and the AHLE can be found here: https://gbadske.org/dashboards/ahle-casestudy/. 

## References

Gilbert, W., Marsh, T.L., Jemberu, W.T., Chaters, G., Bruce, M., Steeneveld, W., Alfonso, J.S., Huntington, B. and Rushton, J. Measuring disease cost in farmed animals for the Global Burden of Animal Diseases: a model of the Animal Health-Loss Envelope. URL: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4472099.
