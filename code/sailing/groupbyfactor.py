import re 

def do(filenames):
    byfactor = {}
    names = None
    size = None
    for filename in filenames:
        m = re.match('costs-size=(\d+)-nsamples=(\d+).txt', filename)
        size = m.group(1)
        nsamples = int(m.group(2))
        f = file(filename)
        names = f.readline().split()
        for line in f:
            words = line.split()
            factor = words[0]
            if factor not in byfactor:
                byfactor[factor] = {}
            byfactor[factor][nsamples] = words[1:]
        f.close()
    names = ['SAMPLES']+names[1:]
    for factor in sorted(byfactor.keys()):
        f = file("costs-size=%s-factor=%s.txt" % (size, factor), "w")
        print >> f, ' '.join(names)
        for nsamples in sorted(byfactor[factor].keys()):
            print >> f, nsamples, ' '.join(byfactor[factor][nsamples])
        f.close()

if __name__=='__main__':
    import sys
    do(sys.argv[1:])
    
            
            
            
    
