from uvexp import *
import math
import sys

class RandomHandle(Handle):
    def __init__(self, mean):
        flip = 0 # random.randrange(3)
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

transform = lowkey # identity
    
def randomexp(nhandles=10, nruns=1000, samples=range(1, 16)):
    print "nsamples "+" ".join("r_"+alg for alg in ALGORITHMS)
    def mkhandles():
        return [RandomHandle(transform(random.random())) for i in range(nhandles)]
    handles = mkhandles()
    for nsamples in [nhandles*i for i in samples]:
        print nsamples,
        for alg in ALGORITHMS:
            print repeat_alg(alg=alg, mkhandles=mkhandles, nsamples=nsamples, nruns=nruns)[1],
        print
        sys.stdout.flush()

if __name__=="__main__":
    transform = locals()[sys.argv[3]]
    randomexp(nhandles=int(sys.argv[1]), nruns=int(sys.argv[2]))
