import sys
import argparse
import gzip


parser = argparse.ArgumentParser(description='Convert .seg to MSMC format.')
parser.add_argument('-s','--seg', type=str)
parser.add_argument('-c','--chr', type=int)
parser.add_argument('-o','--out', type=argparse.FileType('w'))
args = parser.parse_args()

chromid = args.chr
chrompos = 0
breaklen = 100000
#breaklen = 27000
#breaklen = 11000

numCalled = 0
for line in open(args.seg):
    elts = line.strip().split('\t')
    if len(elts) == 3:
        posS, numS, alleleS = elts
    else:
        posS, numS, _, _, _, alleleS = elts
    num = int(numS)
    #if num >= breaklen:
    #    chromid += 1
    #    chrompos = int(posS)
    pos = int(posS) + num - 1
    if alleleS.count('.') == len(alleleS):
        continue
    if num == 0:   ## occurs once in ceu4.  Overlapping SNP and indel mutations in VCF?
        continue
    numCalled += num
    allele = alleleS.replace('.','?').replace('0','A').replace('1','T')
    args.out.write("{}\t{}\t{}\t{}\n".format(chromid, pos - chrompos, numCalled, allele))
    numCalled = 0

    
