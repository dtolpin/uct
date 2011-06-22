import random
from math import sqrt, log, exp

import sys

class Handle:
    def __init__(self, mean, draw):
        self.mean = mean
        self.draw = draw

def perm(seq):
    seq = seq[:]
    random.shuffle(seq)
    return seq

RND = 'RND'
UCB = 'UCB'
UCBt = 'UCBt'
GRD = 'GRD'
UVB = 'UVB'

ALGORITHMS = [RND, UCB, GRD, UVB]
# ALGORITHMS = [RND, UCB, UCBt, GRD, UVB]

class Exp:

    def __init__(self, alg, handles):
        self.alg = getattr(self, alg)
        self.handles = handles

    def run(self, nsamples):
        self.predraw()
        for i in range(nsamples):
            self.alg()
        return self.regret()

    def predraw(self):
        self.outcomes = [[handle.draw()] for handle in self.handles]
        
    def draw(self, i):
        self.outcomes[i].append(self.handles[i].draw())

    # Random
    def RND(self):
        self.draw(random.randrange(len(self.handles)))
    
    # Upper Confidence Bounds
    def UCB(self):
        n = sum(len(o) for o in self.outcomes)
        ibest, vbest = -1, -1
        indices = range(len(self.outcomes))
        random.shuffle(indices) # break ties randomly
        for i in indices:
            ni = len(self.outcomes[i])
            avg = sum(self.outcomes[i])/ni
            v = avg+sqrt(2*log(n)/ni)
            if v > vbest:
                ibest, vbest = i, v
        self.draw(ibest)

    # UCB Tuned 
    def UCBt(self):
        n = sum(len(o) for o in self.outcomes)
        ibest, vbest = -1, -1
        indices = range(len(self.outcomes))
        random.shuffle(indices) # break ties randomly
        for i in indices:
            ni = len(self.outcomes[i])
            avg = sum(self.outcomes[i])/ni
            var = sum(o*o for o in self.outcomes[i])/ni-avg*avg
            v = avg+sqrt(min(0.25,var+sqrt(2*log(n)/ni))*log(n)/ni)
            if v > vbest:
                ibest, vbest = i, v
        self.draw(ibest)

    # Random greedy
    def GRD(self):
        k = len(self.outcomes)
        ibest, abest = -1, -1
        indices = range(len(self.outcomes))
        random.shuffle(indices) # break ties randomly
        for i in indices:
            avg = sum(self.outcomes[i])/len(self.outcomes[i])
            if avg > abest:
                ibest, abest = i, avg
        if random.random() > 0.5*k/(k-1):
            self.draw(ibest)
        else:
            self.draw(random.choice(range(len(self.outcomes))))

    def voi(self, o, a):
        ni = len(o)
        si = sum(o)
        avg = si/ni
        kappa = 1.0/len(self.outcomes)
        voi = (avg==a and 1-kappa or kappa)/ni
        return voi

    def UVB(self):
        # find best average
        abest = -1.0
        for o in self.outcomes:
            avg = sum(o)/len(o)
            if avg > abest:
                abest = avg
    
        # find best handle
        vois = [self.voi(o, abest) for o in self.outcomes]
        ibest, vbest = -1, -1
        indices = range(len(vois))
        random.shuffle(indices) # break ties randomly
        for i in indices:
            if vois[i] >= vbest:
                ibest, vbest = i, vois[i]
        self.draw(ibest)

    def regret(self):
        avgs = [sum(o)/len(o) for o in self.outcomes]
        means = [h.mean for h in self.handles]
        return max(means)-max(zip(avgs, means), key=lambda am: am[0])[1]

# Testing and Experimenting

handles_symmetric = [ Handle(1.0/3.0, lambda: random.choice([0.0, 0.0, 1.0])),
                      Handle(2.0/3.0, lambda: random.choice([0.0, 1.0, 1.0])) ]

handles_third = [ Handle(1.0/3.0, lambda: 1.0/3.0),
                  Handle(2.0/3.0, lambda: random.choice([0.0, 1.0, 1.0])) ]

handles_twothirds = [ Handle(2.0/3.0, lambda: 2.0/3.0),
                      Handle(1.0/3.0, lambda: random.choice([0.0, 0.0, 1.0])) ]

handles_quarter = [ Handle(0.25, lambda: 0.25),
                    Handle(0.5, lambda: random.choice([0.0, 1.0])) ]

handles_threequarters = [ Handle(0.75, lambda: 0.75),
                          Handle(0.5, lambda: random.choice([0.0, 1.0])) ]

handles_many = [ Handle(0.25, lambda: 0.25),
                 Handle(1.0/3.0, lambda: random.choice([0.0, 0.0, 1.0])),
                 Handle(0.5, lambda: random.choice([0.0, 1.0])),
                 Handle(0.5, lambda: 0.5),
                 Handle(2.0/3.0, lambda: random.choice([0.0, 1.0, 1.0])),
                 Handle(0.75, lambda: 0.75) ]

def try_alg(alg=UCB, handles=handles_symmetric, nsamples=10):
    exp = Exp(alg, handles=perm(handles))
    r = exp.run(nsamples)
    return (exp.outcomes, r)

def repeat_alg(alg=UCB, handles=handles_symmetric, nsamples=10, nruns=1000):
    results = [ try_alg(alg=alg, handles=handles, nsamples=nsamples)
                for i in range(nruns) ]
    drawcounts = [round(sum(len(r[0][i]) for r in results)/float(nruns))
                  for i in range(len(results[0][0]))]
    regret = sum(r[1] for r in results)/nruns
    return (drawcounts, regret)

def experiment(handles, nruns=10000, samples=range(4, 16)):
    print "nsamples "+" ".join("r_"+alg for alg in ALGORITHMS)
    for nsamples in [len(handles)*i for i in samples]:
        print nsamples,
        for alg in ALGORITHMS:
            print repeat_alg(alg=alg, handles=handles, nsamples=nsamples, nruns=nruns)[1],
        print
        sys.stdout.flush()

if __name__=="__main__":
    import sys
    if sys.argv==1:
        handles = 'many'
    else:
        handles = sys.argv[1]
    experiment(locals()['handles_'+handles])
