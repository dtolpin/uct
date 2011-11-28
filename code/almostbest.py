import sys

bestweight = len(sys.argv)==1 and 1.0 or float(sys.argv[1])

header = sys.stdin.readline().split()
alpha = 1.0
beta = 1.0
for line in sys.stdin:
	data = dict(zip(header, line.split()))
	value = float(data['WIN_W'])
	if value < alpha:
		beta = alpha
		alpha = value
	elif value < beta:
		beta = value
print (bestweight*alpha+(1-bestweight)*beta)

