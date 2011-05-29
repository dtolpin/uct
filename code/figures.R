ALGORITHMS <- c('rnd', 'ucb', 'sve')

draw.regrets <- function(regrets_file, legend.position='topright', xlab=expresion(N[samples])) {
	regrets <- read.table(regrets_file, header=T)
	xlim <- range(regrets$nsamples)
	ylim <- range(0, regrets[-1])
	plot(x=regrets$nsamples, type='n', log="x", xlim=xlim, ylim=ylim, yaxs='i', xlab=xlab, ylab='Regret', main=unlist(strsplit(regrets_file, "\\."))[[1]])
	i <- 1
	for(alg in names(regrets)[-1]) {
		lines(x=regrets$nsamples, y=regrets[[alg]],
			  type="o", pch=i)
		i <- i+1
	}
	legend(x=legend.position, legend=sapply(names(regrets)[-1], function(n) substring(n, 3)), pch=1:length(names(regrets)[-1]))
}

draw.experiment <- function(prefix="") {
  	old.par <- par(no.readonly=TRUE)
	par(mar=c(4,4,2,2))
	layout(cbind(c(1,2,3),c(4,5,6)))
	for(result in paste(c('symmetric', 'twothirds', 'threequarters', 'quarter', 'third', 'many'), "txt", sep=".")) draw.regrets(paste(prefix,result,sep=""))
        par(old.par)
}

compare.random <- function(upper_file='random-upper.txt', greedy_file='random-greedy.txt', semigreedy_file='random-semigreedy.txt') {
  upper <- read.table(upper_file, header=T)
  greedy <- read.table(greedy_file, header=T)
  semigreedy <- read.table(semigreedy_file, header=T)

  xlim <- range(upper$nsamples)
  ylim <- range(0, upper$r_sve, greedy$r_sve, semigreedy$r_sve)
  plot(x=upper$nsamples, type='n', xlim=xlim, ylim=ylim, xlab=expression(N[samples]), ylab='Regret', main='upper vs. greedy vs. semigreedy')
  lines(x=upper$nsamples, upper$r_sve, type="o", pch=1)
  lines(x=greedy$nsamples, greedy$r_sve, type="o", pch=2)
  lines(x=semigreedy$nsamples, semigreedy$r_sve, type="o", pch=3)
  legend(x='topright', legend=c('upper', 'greedy', 'semigreedy'), pch=1:3)
}

draw.random <- function(prefix='random', legend.position='topright') {
  par(mar=c(4,4,2,2))
  layout(cbind(c(1,2),c(3,4)))
  draw.regrets(paste(prefix,'8.txt',sep='-'), legend.position=legend.position)
  draw.regrets(paste(prefix,'16.txt',sep='-'), legend.position=legend.position)
  draw.regrets(paste(prefix,'32.txt',sep='-'), legend.position=legend.position)
  draw.regrets(paste(prefix,'64.txt',sep='-'), legend.position=legend.position)
}
