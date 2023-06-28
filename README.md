# GBADs

Welcome is the GBADs (Global Burden of Animal Diseases) R package repository!

GBADs runs a novel compartmentalized agent-based model called AHLE (Animal Health Loss Envelope) used to estimate production losses in livestock such as cattle, poultry, and small ruminants within developing countries like Ethiopia. Such lossess can include milk/egg production, breeding stock, meat, manure, *etc*.

The model is expressed mathewmatically as follows:

```math
Y = F(z, \theta) \times [1 - L(b_0(1 - C(x, \theta)))]
```
where 

- $Y$ is the total output of the production system
- $F(z, \theta)$ is the production function for a set of ordinary inputs $z$ (animals, feed, labour, *etc.* required to generate $Y$ in the absence of disease) and exogenous parameters $\theta$ (such as climate, regulations, *etc.*)
- $L$ is a loss function ($0 \leq L \leq 1) describing the action of disease hazards $b_0$ on the production of $Y$
- $C(x, \theta)$ is a control function ($0 \leq C \leq 1), increasing in control inputs $x$ (vaccines, antibiotics, *etc.*) which mitigates the effects of $b_0$
