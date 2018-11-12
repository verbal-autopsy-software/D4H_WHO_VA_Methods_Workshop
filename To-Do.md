# List of Useful Features to add to OpenVA

* An option in `codeva` that will tell the function to return a data.frame
that includes both the causes of death AND the symptoms/indicators.

* Functions for for comparing results from 2 different algorightms

    + a check for overlap among the top 3 causes -- e.g., if InSilico assigns 
    causes C1 (60%), C2 (30%), and C3 (10%) and InterVA assigns causes C2 (60%),
    C1 (30%), and C3 (10%), then there is some agreement; but if you only compare
    the top cause (C1 vs. C2) the results suggests that the aglorithms disaggree.
    
    + a function that returns a data.frame with the VA indicators along with the
    causes assigned by different algorithms.
