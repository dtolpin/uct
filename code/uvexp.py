import random
from math import sqrt, log, exp

class Handle:
    def __init__(self, mean, draw):
        self.mean = mean
        self.draw = draw

def perm(seq):
    seq = seq[:]
    random.shuffle(seq)
    return seq

# Value of Information:
# two components --- upper bound and greedy
# the influence of upper bound decreases with the number of samples

def voi_semigreedy(o, a, b, n, ampl = lambda n, ni: n/ni):
    ni = len(o)
    si = sum(o)
    avg = si/ni
    k = ampl(n, ni)
    def u(x): return (si+k*x)/(ni+k)
    us = (u(x) for x in o)
    voi =  ( avg==a
             and sum(b-y for y in us if y < b)
             or  sum(y-a for y in us if y > a) ) / ni
    return voi

voi_greedy = lambda o, a, b, n: voi_semigreedy(o, a, b, n, ampl = lambda n, ni: 1)

def voi_upper(o, a, b, n):
    ni = len(o)
    si = sum(o)
    avg = si/ni
    voi = (avg==a and b or 1-a)/ni
    return voi

def voi(o, a, b, n):
    ni = len(o)
    return voi_upper(o, a, b, n)/ni + voi_semigreedy(o, a, b, n)
             
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

    # Random
    def RND(self):
        self.draw(random.randrange(len(self.handles)))
    
    # Upper Confidence Bounds
    def UCB_1(self):
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
    
    def UCB_1TUNED(self):
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

    UCB = UCB_1TUNED
    
    # Sample-based VOI Estimate
    def SVE(self):
        n = sum(len(o) for o in self.outcomes)
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

def compare_algs(handles=handles_symmetric, nsamples=10, nruns=1000):
    print "r_rnd=%s r_ucb=%s r_sve=%s" \
        % tuple([ repeat_alg(alg=alg, handles=handles, nsamples=nsamples, nruns=nruns)[1]
                  for alg in [RND, UCB, SVE] ])

def experiment(handles, nruns=5000, samples=range(4, 16)):
    print "nsamples r_rnd r_ucb r_sve"
    for nsamples in [len(handles)*i for i in samples]:
        print nsamples,
        for alg in [RND, UCB, SVE]:
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

