import argparse
import glob
import json
import re
import sys

import sraxml

parser = argparse.ArgumentParser(description='xml reader test')
parser.add_argument('dir', help='directory of xml files')
parser.add_argument('--test', action='store_true', help='limit to 999 files')
arg = parser.parse_args()

printed = set()
for filename in glob.glob(f'{arg.dir}/*/*'):
	f = filename.split('/')
	taxid = f[1]
	with open(filename) as fp: data, status = sraxml.read(fp)
	if data is None: continue
	for run in data['runs']:
		if run['run_id'] in printed: continue
		printed.add(run['run_id'])
		readlen = run['nts'] / run['seqs']
		print(taxid, data['srx_id'], run['run_id'], readlen, sep='\t')
