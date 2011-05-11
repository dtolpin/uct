ALGORITHMS <- c('rnd', 'ucb', 'sve')

draw.regrets <- function(regrets_file) {
	regrets <- read.table(regrets_file, header=T)
	xlim <- range(regrets$nsamples)
	ylim <- range(0, regrets$r_rnd, regrets$r_ucb, regrets$r_sve)
	plot(x=regrets$nsamples, type='n', xlim=xlim, ylim=ylim, xlab=expression(N[samples]), ylab='Regret', main=unlist(strsplit(regrets_file, "\\."))[[1]])
	i <- 1
	for(alg in ALGORITHMS) {
		lines(x=regrets[["nsamples"]], y=regrets[[paste("r_",alg,sep="")]],
			  type="o", pch=i)
		i <- i+1
	}
	legend(x='topright', legend=ALGORITHMS, pch=1:length(ALGORITHMS))
}

draw.experiment <- function() {
  	old.par <- par(no.readonly=TRUE)
	par(mar=c(4,4,2,2), yaxs='i')
	layout(cbind(c(1,2,3),c(4,5,6)))
	for(result in paste(c('symmetric', 'twothirds', 'threequarters', 'quarter', 'third', 'many'), "txt", sep=".")) draw.regrets(result)
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

draw.random <- function() {
  layout(cbind(c(1,2),c(3,4)))
  draw.regrets('random-4.txt')
  draw.regrets('random-8.txt')
  draw.regrets('random-16.txt')
  draw.regrets('random-64.txt')
}
