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
	par(mar=c(4,4,2,2))
	layout(cbind(c(1,2,3),c(4,5,6)))
	for(result in paste(c('symmetric', 'twothirds', 'threequarters', 'quarter', 'third', 'many'), "txt", sep=".")) draw.regrets(result)
}
