
import sys

field = {
        'tar': (0,19, False),
        'tid': (21,30, False),
        'qry': (32,52, False),
        'qid': (54,65, False),
        'eva': (67,75, True),
        'sca': (77,82, True),
        'bia': (84,88, True),
        'evd': (90,98, True),
        'scd': (100,105, True),
        'bid': (107,111, True),
}

print(','.join(field.keys()))

with open(sys.argv[1]) as fp:
    for line in fp:
        if line.startswith('#'): continue
        fields = []
        for tag, (beg, end, atof) in field.items():
            if atof: val = float(line[beg:end])
            else:    val = line[beg:end].rstrip()
            fields.append(str(val))
        print(','.join(fields))


