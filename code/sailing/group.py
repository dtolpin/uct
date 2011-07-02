import re 
import sys

def median(vals):
  vals.sort()

  if len(vals) % 2 == 1:
    return vals[(len(vals)+1)/2-1]
  else:
    lower = vals[len(vals)/2-1]
    upper = vals[len(vals)/2]
    return (float(lower + upper)) / 2  

def do(filenames):
    regrets = {}
    names = None
    size = None
    for filename in filenames:
        m = re.match('costs-size=(\d+)-nsamples=(\d+).txt', filename)
        size = m.group(1)
        nsamples = int(m.group(2))
        f = file(filename)
        names = f.readline().split()
        values = [[] for name in names[1:]]
        for line in f:
            lvs = [float(v) for v in line.split()[1:]]
            for i in range(len(values)):
                values[i].append(lvs[i])
            regrets[nsamples] = [min(vs) for vs in values]
        f.close()
    names = ['SAMPLES']+names[1:]
    f = file("costs-size=%s.txt" % size, "w")
    print >> f, ' '.join(names)
    for nsamples in sorted(regrets.keys()):
        print >> f, nsamples, ' '.join([str(x) for x in regrets[nsamples]])
    f.close()

if __name__=='__main__':
    import sys
    do(sys.argv[1:])
    
            
            
            
    
