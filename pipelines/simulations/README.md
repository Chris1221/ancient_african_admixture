# Simulations

This pipeline recreates the simulation experiments in the paper. These analyses follow a similar structure as the Real Data section, with a configuration `.json` file in the `analyses` folder.  There are many experiments documented in the `Snakefile`, mostly used for diagnostics. To run a particular analyses, specify the configuration file at the top of the `Snakefile` and run the corresponding rule. Note that all relevant results are given in the root `/data/` folder of this repository.

- `snakemake sp` will run all of the main simulations from the paper (those shown in the main figures, along with their corresponding supplemental figures). This includes inference on the two diploid seperately. The configuration file for this is `analyses/sp_varyingmig.json`.
- `snakemake yri` will run the supplemental analyses looking at the effect of different initial migration on inference in a CEU-YRI comparison. The configuration file for this is `analyses/yri_varyingmig.json`.
- `snakemake msmc` will run the main simulations with inference in `msmc` rather than `smcsmc`.

### Warning

These pipelines are configured to use SGE `qsub`. If you would like to run the analyses using different compute resources, you may need to alter the smcsmc code. Contact the authors for more details, depending on your particular architecture. Additionally, the wildcard setup in this `Snakefile` is a little more complicated than the Real Data analysis, and if you are getting errors about a particular wild card not being defined or used, simply comment out that definition at the top of the `Snakefile`. 
