# GBADs

Welcome is the GBADs (Global Burden of Animal Diseases) R package repository!

GBADs runs a novel compartmentalized agent-based model called AHLE (Animal Health Loss Envelope) used to estimate production losses in livestock such as cattle, poultry, and small ruminants within developing countries like Ethiopia. Such lossess can include milk/egg production, breeding stock, meat, manure, *etc*.

The model is expressed mathewmatically as follows:

```math
Y = F(z, \theta) \times [1 - L(b_0(1 - C(x, \theta)))]
```
