# Ancient Admixture into Africa

This repository contains all of the source code required to reproduce our paper on ancient directional migration using an extention of `smcsmc`.

- `pipelines/` contains two different pipelines which implement all of the analyses in the paper. The first uses `smcsmc` and `msmc2` to analyse real data from the SGDP and HGDP. The second uses `scrm` to simulate demographic scenarios and does inference with `smcsmc` and `msmc`. For instructions on reproducing these results, see directions in these directories. As a forewarning, these analyses are highly computationally intensive and may take several weeks or months of compute to run completely. 
- `data/` contains all of the relevant results from the `pipelines/` directory used in the paper, and in the plots.
- `r/` contains scripts for reproducing all of the plots from the paper. Note that `r/MASTER.r`, if run by itself, contains all of the code necessary to create all the plots using the data in the `data/` directory.
- `plot/` has the output of the `r/` directory, which are the plots used in the paper.
- `tex/` has the `LaTeX` source for the actual manuscript of the paper.
- `analyses` contains configuration files for the different analyses performed using the pipelines. Similarly, `lib/` has some useful files for the plotting scripts. 

To reproduce the analysis completely, please install the required `conda` environments for the specific pipeline that you want to run. There are instructions for this in each folder.
