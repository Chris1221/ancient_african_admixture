import msprime
import stdpopsim

# I'm going to be defining the models the same way 
# that is currently done in stdpopsim, but this 
# will eventually change.

# Set up from the homo sapiens catelog.
# Might also change at some point.
_chromosome_data = """\
chr1 	 249250621 	 1.1485597641285933e-08
chr2 	 243199373 	 1.1054289277533446e-08
chr3 	 198022430 	 1.1279585624662551e-08
chr4 	 191154276 	 1.1231162636001008e-08
chr5 	 180915260 	 1.1280936570022824e-08
chr6 	 171115067 	 1.1222852661225285e-08
chr7 	 159138663 	 1.1764614397655721e-08
chr8 	 146364022 	 1.1478465778920576e-08
chr9 	 141213431 	 1.1780701596308656e-08
chr10 	 135534747 	 1.3365134257075317e-08
chr11 	 135006516 	 1.1719334320833283e-08
chr12 	 133851895 	 1.305017186986983e-08
chr13 	 115169878 	 1.0914860554958317e-08
chr14 	 107349540 	 1.119730771394731e-08
chr15 	 102531392 	 1.3835785893339787e-08
chr16 	 90354753 	 1.4834607113882717e-08
chr17 	 81195210 	 1.582489036239487e-08
chr18 	 78077248 	 1.5075956950023575e-08
chr19 	 59128983 	 1.8220141872466202e-08
chr20 	 63025520 	 1.7178269031631664e-08
chr21 	 48129895 	 1.3045214034879191e-08
chr22 	 51304566 	 1.4445022767788226e-08
chrX 	 155270560 	 1.164662223273842e-08
chrY 	 59373566 	 0.0
"""

_chromosomes = []
for line in _chromosome_data.splitlines():
    name, length, mean_rr = line.split()[:3]
    _chromosomes.append(stdpopsim.Chromosome(
        id=name, length=int(length),
        mutation_rate=1e-8,  # WRONG!,
        recombination_rate=float(mean_rr)))

_genome = stdpopsim.Genome(chromosomes=_chromosomes)

_species = stdpopsim.Species(
    id="homsap",
    name="Homo sapiens",
    genome=_genome,
    generation_time=29, # To be consistent 
    population_size=10**4)

stdpopsim.register_species(_species)

_gm = stdpopsim.GeneticMap(
    species=_species,
    name="HapmapII_GRCh37",
    url=(
        "https://ftp-trace.ncbi.nih.gov/1000genomes/ftp/technical/working/"
        "20110106_recombination_hotspots/"
        "HapmapII_GRCh37_RecombinationHotspots.tar.gz"),
    file_pattern="genetic_map_GRCh37_{name}.txt",
    description=(
        "The Phase II HapMap Genetic map (lifted over to GRCh37) used in "
        "1000 Genomes. Please see the README for more details."),
    citations=[
        stdpopsim.Citation(
            doi="https://doi.org/10.1038/nature06258",
            year=2007,
            author="1000 Genomes Project consortium"),
        ]
    )
_species.add_genetic_map(_gm)

## Population definitions

_yri_population = stdpopsim.Population(
    name="YRI",
    description="Yorubans")
_ceu_population = stdpopsim.Population(
    name="CEU",
    description="Western Eurasian Ancestry")
_chb_population = stdpopsim.Population(
    name="CHB",
    description="Eastern Eurasian Ancestry")
_vind_population = stdpopsim.Population(
    name="VIND",
    description="Vindija Neanderthal")
_deni_population = stdpopsim.Population(
    name="DENI",
    description="Denisovan")
_san_population = stdpopsim.Population(
    name = "SAN",
    description="San bushmen")
_ghost_population = stdpopsim.Population(
    name = "GHOST",
    description="Unsampled OoA lineage")
_papuan_population = stdpopsim.Population(
    name = "PAP",
    description="Indigenous Papua New Guineans")

class HomoSapiensModel(stdpopsim.Model):
    species = _species
    """
    TODO: documentation
    """
    def __init__(self):
        super().__init__()
        self.generation_time = _species.generation_time


class known_model(HomoSapiensModel):
    """
    The (accepted) model of human history to this point.
    This acts as essentially the null against our 
    merger scenario."""

    g = 29

    ## Split times

    


