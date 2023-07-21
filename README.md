# DiD_Tutorial
A tutorial on the implementation of difference-in-difference (DiD) analyses using regression for estimating the effect of a group-level treatment among the treated in biomedical research. 

The tutorial code and data here are supplementary material to:
Rothbard S, Etheridge JC, Murray EJ. A Tutorial on Applying the Difference-in-Difference Method to Health Data. _Current Epidemiology Reports_. 2023. [in press] doi: 10.1007/s40471-023-00327-x

Synthetic datasets are available as SAS files (.sas7bdat), as Stata files (.dta), and as a CSV file. In addition, the code for creating the synthetic data is available in SAS. Analytic code is available in SAS, Stata, and R. Note that the SAS code uses non-parametric bootstrapping to obtain valid, non-conservative confidence intervals. The Stata & R code use robust variance estimation which provides valid, but conservative (i.e., wider than 95%) confidence intervals.
 
The Tutorial Workbook is available as a Student Version (no solutions) and a Teacher Version (solutions). This repository accompanies a tutorial article on DiD available at: [tbd]. 

Feedback or bug reports are welcome and can be provided via the GitHub Issue system or by email to ejmurray@bu.edu
