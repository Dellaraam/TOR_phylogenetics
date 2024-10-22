import sys 
import csv
import argparse 
import glob
import os

file = sys.argv[1]

with open(file) as fp:
	for line in fp:
		if line.startswith('# target'): 
			titles = line.split(maxsplit=21)
			target_name = titles[1] + "_" + titles[2]
			query_name = titles[4] + "_" + titles[5]
			description = titles[-1] + "_" + titles[-2] + "_" + titles[-3]
			print(f'{target_name}, {titles[3]}, {query_name}, {titles[6]}, {titles[7]}, {titles[8]}, {titles[9]}, {titles[10]}, {titles[11]}, {titles[12]}, {titles[13]}, {titles[14]}, {titles[15]}, {titles[16]}, {titles[17]}, {titles[18]}, {titles[19]}, {titles[20]}, {titles[-1]}')
		if line.startswith('#'): continue
		colomns = line.split()
		print(f'{colomns[0]}, {colomns[1]}, {colomns[2]}, {colomns[3]}, {colomns[4]}, {colomns[5]}, {colomns[6]}, {colomns[7]}, {colomns[8]}, {colomns[7]}, {colomns[8]}, {colomns[9]}, {colomns[10]}, {colomns[11]}, {colomns[10]}, {colomns[11]}, {colomns[12]}, {colomns[13]}, {colomns[14]}, {colomns[15]}, {colomns[16]}, {colomns[17]}, {colomns[18:]}')
	

