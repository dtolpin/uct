import sys, re

def do(logf,template='costs-size=%s-nsamples=%s.txt'):
    resf = None
    size='X'
    for line in logf:
        m = re.match('.*\(exp1 .*:size (\d+)\s*', line)
        if m:
            size = int(m.group(1))
            continue
        m = re.match('\[(\d+) samples\].*', line)
        if m:
            nsamples = int(m.group(1))
            if resf:
                resf.close()
            resf = file(template % (size, nsamples), "w")
            continue
        if resf:
            m = re.match('\s*[A-Z]+(\s+[A-Z]+)* *', line) \
                or re.match('\s*[-\d.]+(\s+[-\d.]+)* *', line)
            if m:
                print >>resf, line,
    if resf:
        resf.close()

if __name__=="__main__":
    if len(sys.argv)>1:
        logf = file(sys.argv[1])
        if len(sys.argv)==3:
            do(logf, sys.argv[2])
        else:
            do(logf)
    else:
        do(sys.stdin)
        
