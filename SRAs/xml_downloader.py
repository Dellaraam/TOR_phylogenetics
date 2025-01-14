import argparse
import os
import re
import requests
import sys
import time

parser = argparse.ArgumentParser(description='get selected SRA RNA-seq stuff')
parser.add_argument('file', help='file of taxids')
parser.add_argument('--delay', type=float, default=0.5,
	help='delay between requests [%(default).2f]')
parser.add_argument('--retry', type=int, default=3,
	help='number of times to retry after download failure [%(default)i]')
parser.add_argument('--limit', type=int, help='only get x new downloads')
arg = parser.parse_args()

base = 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils'
taxids = []
with open(arg.file) as fp:
	for line in fp:
		f = line.split()
		taxids.append(f[0])

for taxid in taxids:
	print(taxid, file=sys.stderr, end='', flush=True)
	os.system(f'mkdir -p build/{taxid}')
	term = f'term=txid{taxid}[Organism]'

	# initial request to get number of records
	url = f'{base}/esearch.fcgi?db=sra&{term}&retmax=1'
	response = requests.get(url)
	response.encoding = 'utf-8'
	n = int(re.search(r'<Count>(\d+)</Count>', response.text).group(1))
	time.sleep(arg.delay)

	# full request of all records
	url = f'{base}/esearch.fcgi?db=sra&{term}&retmax={n}'
	response = requests.get(url)
	response.encoding = 'utf-8'
	time.sleep(arg.delay)

	# download each XML file
	done = 0
	fail = []
	for m in re.finditer(r'<Id>(\d+)</Id>', response.text):
		print('.', file=sys.stderr, end='', flush=True)
		uid = m.group(1)
		path = f'build/{taxid}/{uid}.xml'
		if os.path.exists(path) and os.path.getsize(path) > 0: continue

		# download file
		url = f'{base}/efetch.fcgi?db=sra&id={uid}&rettype=xml&retmode=text'
		success = False
		for _ in range(arg.retry + 1):
			response = requests.get(url)
			if response.status_code != 200:
				print(f'ERROR {response.status_code}, will retry in {arg.delay}')
			else:
				with open(path, 'w') as fp: fp.write(response.text)
				success = True
				break
			time.sleep(arg.delay)
		if success: done += 1
		else:       fail.append(uid)
		if arg.limit and done >= arg.limit: break
	print(file=sys.stderr, flush=True)

