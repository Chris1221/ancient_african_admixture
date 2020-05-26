# Ancient Admixture into Africa

This repository contains all of the source code required to reproduce our paper on ancient directional migration using an extention of `smcsmc`.

- `pipelines/` contains two different pipelines which implement all of the analyses in the paper. The first uses `smcsmc` and `msmc2` to analyse real data from the SGDP and HGDP. The second uses `scrm` to simulate demographic scenarios and does inference with `smcsmc` and `msmc`. For instructions on reproducing these results, see directions in these directories. As a forewarning, these analyses are highly computationally intensive and may take several weeks or months of compute to run completely. 
- `data/` contains all of the relevant results from the `pipelines/` directory used in the paper, and in the plots.
- `r/` contains scripts for reproducing all of the plots from the paper. Note that `r/MASTER.r`, if run by itself, contains all of the code necessary to create all the plots using the data in the `data/` directory.
- `plot/` has the output of the `r/` directory, which are the plots used in the paper.
- `tex/` has the `LaTeX` source for the actual manuscript of the paper.
- `analyses` contains configuration files for the different analyses performed using the pipelines. Similarly, `lib/` has some useful files for the plotting scripts. 

### Setting up the Environment

To reproduce the analysis completely, please install the required `conda` environment. This allows us to package a majority of the software dependencies together. There are several methods to install `conda`, see [here](https://docs.conda.io/projects/conda/en/latest/user-guide/install/) for details. Once `conda` is installed, create the environment for this analysis from our list of dependencies: 

```sh
conda env create -f envs/aaa.yml
```

This will create a `conda` environemnt called `aaa`. Activate it:

```
conda activate aaa
```

There are a couple of dependencies that are not included in `conda` and must be installed manually. 

- `msmc2` must be installed from the Github source [here](https://github.com/stschiff/msmc2). 
- `admixr` must be installed from Github as well.

```R
install.packages("devtools")
devtools::install_github("bodkan/admixr")
```
