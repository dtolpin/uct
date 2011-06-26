from uvexp import *
import math
import sys

MINSF=8        # initial sampling factor in the random experiment
MAXRWRD=1.0    # reward of a random arm is [1-MAXRWRD, MAXRWRD)

class RandomHandle(Handle):
    def __init__(self, mean):
        flip = random.randrange(3)
        if flip==0: # bi-value
            draw = lambda: random.random() < mean and 1.0 or 0.0
        elif flip==1: # constant
            draw = lambda: mean
        else: # triangular
            mode = mean
            mean = (mode+1)/3
            draw = lambda: random.triangular(0, 1, mode)

        Handle.__init__(self, mean, draw)

def repeat_alg(alg=UCB, mkhandles=lambda: [RandomHandle(random.random()) for i in range(10)], nsamples=10, nruns=1000):
    
    results = [ try_alg(alg=alg, handles=mkhandles(), nsamples=nsamples)
                for i in range(nruns) ]
    drawcounts = [round(sum(len(r[0][i]) for r in results)/float(nruns))
                  for i in range(len(results[0][0]))]
    regret = sum(r[1] for r in results)/nruns
    return (drawcounts, regret)

def identity(x): 
    return x

def sigmoid(x):
    return math.tanh(5*(x-0.5))
    
def highkey(x):
    return 0.5*x**0.25

def lowkey(x):
    return 0.5*(1+x**4)

transform = identity
    
def randomexp(nhandles=10, nruns=1000, minsf=MINSF, sfstep=2, nsf=8):
    assert (MAXRWRD<=1.0 and MAXRWRD>0.5)
    print "nsamples "+" ".join("r_"+alg for alg in ALGORITHMS)
    def mkhandles():
        return [RandomHandle(transform(1-MAXRWRD
                                       +(2*MAXRWRD-1)*random.random()))
                for i in range(nhandles)]
    handles = mkhandles()
    nsamples = minsf*nhandles
    for i in range(nsf):
        print nsamples,
        sys.stdout.flush()
        for alg in ALGORITHMS:
            print " %#8f" % repeat_alg(alg=alg, mkhandles=mkhandles, nsamples=nsamples, nruns=nruns)[1],
            sys.stdout.flush()
        print
        nsamples = int(nsamples*sfstep)

if __name__=="__main__":
    transform = locals()[sys.argv[3]]
    minsf = len(sys.argv)==4 and MINSF or int(sys.argv[4])
    randomexp(nhandles=int(sys.argv[1]), nruns=int(sys.argv[2]), minsf=minsf)
