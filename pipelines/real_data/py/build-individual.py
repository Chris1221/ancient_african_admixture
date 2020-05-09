#
# make the msmc input files from the phased vcfs and mask files in other directories
#
# cmd line args: the individual identifiers
#

import sys
import subprocess
import gzip

maskdir = "mask"
vcfdir = "tmp"
onekgvcf = "sgdp/chr_{}.filtered.vcf.gz"
smcsmctool = "/apps/well/python/3.4.3/bin/python3 py/generate_smcsmcinput.py" #changed this

key = sys.argv[1]

try:
    chroms = [ int(sys.argv[2]) ]
    indidx = 3
except:
    chroms = range(1,23) #edit cause i fucked up
    indidx = 2

for chrom in chroms:

    # extract data for individuals, dump each in its own vcf
    fnames = [ "{}/tmp{}.{}.chr{}.vcf.gz".format(vcfdir, key, individual, chrom) for individual in sys.argv[indidx:] ]
    try:
        try_open = [ gzip.GzipFile( fname,'r') for fname in fnames ]
        have_files = True
        print ("Found files ",fnames)
    except:
        have_files = False
        print ("Did not find files ",fnames,", generating")
    if not have_files:
        fout = [ gzip.GzipFile( fname ,'w') for fname in fnames ]
        fin = gzip.GzipFile(onekgvcf.format(chrom), 'r')
        print ("Reading ",onekgvcf.format(chrom))
        cols = []
        for line in fin:
            elts = line.strip().split('\t')
            if line.startswith('#CHROM'):
                for f, individual in zip(fout, sys.argv[indidx:]):
                    col = [i for i,e in enumerate(elts) if e == individual]
                    if len(col) == 0:
                        raise ValueError("Could not find individual {}".format(individual))
                    cols.append(col[0])
            if line.startswith('#') and not line.startswith('#CHROM'):
                for f in fout:
                    f.write(line)
            else:
                for i, f in enumerate(fout):
                    # filter out hom ref calls, and indel calls
                    if (not elts[cols[i]].startswith("0|0")) and len(elts[3]) == 1 and len(elts[4]) == 1:
                        f.write( '\t'.join( elts[:9] + [elts[cols[i]]] + ["\n"] ) )

        print ("done reading 1kg file")
        for f in fout:
            f.close()
        
    for (tool, opts, fn) in [(smcsmctool, "--minsize 20", "| gzip > {}.chr{}.seg.gz")]:
    
        cmdline = [tool] + \
            ["--mask={}/{}.chr{}.txt.gz".format(maskdir,"mask",chrom) for na in sys.argv[indidx:] ] + \
            ["{}/tmp{}.{}.chr{}.vcf.gz".format(vcfdir,key,na,chrom) for na in sys.argv[indidx:] ] + \
            [opts + fn.format(sys.argv[1], chrom)]
        
        sys.stderr.write("Running: " + " ".join(cmdline))
        subprocess.call( " ".join(cmdline), shell=True )
        
