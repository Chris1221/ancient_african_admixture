# Data analysis pipelines

These pipelines perform all of the analyses in the paper. To run these, install `conda` from the [official website](https://docs.conda.io/projects/conda/en/latest/index.html). Alternatively, we also recommend `mamba`, a [drop-in replacement for `conda`](https://github.com/QuantStack/mamba). Install the conda environment in `../env/aaa.yml` with `conda env create -f ../env/aaa.yml`. This will install a current version of `Snakemake`, which is used to organise all analyses.  The `conda` environment will additionally install `smcsmc` and `scrm`. Navigate to the pipeline directory and run `snakemake ${rule}` to produce a certain output. A full description of the various rules is given in the pipeline folder.

### Warning

These pipelines are configured to use SGE `qsub`. If you would like to run the analyses using different compute resources, you may need to alter the `smcsmc` code. See [here](https://smcsmc.readthedocs.io/en/latest/usage/cluster.html) for details. Contact the authors for more details, depending on your particular architecture.
