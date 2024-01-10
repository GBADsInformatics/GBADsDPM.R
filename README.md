# GBADs.R

Welcome is the GBADs (Global Burden of Animal Diseases) R package repository!

## The Model

GBADs runs a novel compartmentalized equation-based model called the Dynamic Population Model (DPM) to compute the AHLE (Animal Health Loss Envelope) --- the difference between ideal and current levels --- used to estimate production losses in livestock such as cattle, poultry, and small ruminants within developing countries like Ethiopia. Such losses can include milk/egg production, breeding stock, meat, manure, *etc*. The DPM is structured according to age (juvenile, sub-adult, and adult) and sex (male and female) and simulations are run in monthly timesteps up to one year. 

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

1.    ```read_params(file_path, file_type)``` reads in a model parameter file in YAML format into a user's R workspace based on a specied ```file_path``` telling R where files are located on a user's system 
2.    ```rpert(n, min, mode, max, lambda = 4)``` generates ```n``` random variates within a given range from the PERT distribution based on specified minimum, maximum, and modal values, where ```x_min``` $<$ ```x_mode``` $<$ ```x_max```. Setting ```lambda = 4``` controls the shape (*i.e.*, flatness) of the distribution, effectively placing more or less weight on certain values
3.   ```run__compartmental_model()``` runs a simulation of the AHLE for a scenario of interest using parameters found in the YAML file

## References

Gilbert, W., Marsh, T.L., Jemberu, W.T., Chaters, G., Bruce, M., Steeneveld, W., Alfonso, J.S., Huntington, B. and Rushton, J. Measuring disease cost in farmed animals for the Global Burden of Animal Diseases: a model of the Animal Health-Loss Envelope. URL: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4472099.
