import sys
import os
import glob
import argparse

parser = argparse.ArgumentParser(description='run on hmmr output directory')
parser.add_argument('--dir', help='directory of fasta files')
parser.add_argument('--verbose', '-v', action='store_true')
arg = parser.parse_args()

if not os.path.exists(arg.dir): sys.exit(f'{arg.dir} does not exist')

for ff in glob.glob(f'{arg.dir}/*'): 
	if arg.verbose: print('processing', ff, file=sys.stderr)
	os.system(f'python3 hmmrparser.py {Fasta_files/GCA_*} > {ff}.csv')