import random
from math import sqrt, log

class Handle:
    def __init__(self, mean, draw):
        self.mean = mean
        self.draw = draw

def voi(o, a, b, n):
    avg = sum(o)/len(o)
    ni = len(o)
    return ( avg > b
             and sum([ b-y for y in
                       [ (sum(o)+x)/(ni+1)
                         for x in o ]
                       if y<b ]) + b
             or  sum([ y-a for y in
                       [ (sum(o)+x)/(ni+1)
                         for x in o ]
                       if y>a ])+1-a ) / len(o)

RND = 'RND'
UCB = 'UCB'
SVE = 'SVE'

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

    def RND(self):
        self.draw(random.randrange(len(self.handles)))
    
    def UCB(self):
        n = sum([len(o) for o in self.outcomes])
        ibest, vbest = -1, -1
        for  i in range(len(self.handles)):
            ni = len(self.outcomes[i])
            v = sum(self.outcomes[i])/ni+sqrt(2*log(n)/ni)
            if v > vbest:
                ibest, vbest = i, v
        self.draw(ibest)
    
    def SVE(self):
        n = sum([len(o) for o in self.outcomes])
        # find alpha and beta
        ia, ib, a, b = -1, -1, -1, -1
        for i in range(len(self.outcomes)):
            avg = sum(self.outcomes[i])/len(self.outcomes[i])
            if avg > a:
                ia, ib, a, b = i, ia, avg, a
            elif avg > b:
                ib, b = i, avg
    
        # find best handle
        vois = [voi(o, a, b, n) for o in self.outcomes]
        ibest, vbest = -1, -1
        for i in range(len(vois)):
            if vois[i] > vbest:
                ibest, vbest = i, vois[i]

        self.draw(ibest)
    
    def regret(self):
        avgs = [sum(o)/len(o) for o in self.outcomes]
        means = [h.mean for h in self.handles]
        return max(means)-max(zip(avgs, means), key=lambda am: am[0])[1]

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



def try_alg(alg=UCB, handles=handles_symmetric, nsamples=10):
    exp = Exp(alg, handles=handles)  
    r = exp.run(nsamples)
    return (exp.outcomes, r)

def repeat_alg(alg=UCB, handles=handles_symmetric, nsamples=10, nruns=1000):
    results = [ try_alg(alg=alg, handles=handles, nsamples=nsamples)
                for i in range(nruns) ]
    drawcounts = [round(sum([len(r[0][i]) for r in results])/float(nruns))
                  for i in range(len(results[0][0]))]
    regret = sum([r[1] for r in results])/nruns
    return (drawcounts, regret)

def compare_algs(handles=handles_symmetric, nsamples=10, nruns=1000):
    print "r_rnd=%s r_ucb=%s r_sve=%s" \
        % tuple([ repeat_alg(alg=alg, handles=handles, nsamples=nsamples, nruns=nruns)
                  for alg in [RND, UCB, SVE] ])


