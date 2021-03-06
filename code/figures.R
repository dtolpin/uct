ALGORITHMS <- c('rnd', 'ucb', 'sve')


draw.regrets <- function(regrets_file, log="xy", legend.position='topright', xlab=expression(N[samples]), main=NA) {
	regrets <- read.table(regrets_file, header=F)

        names <- as.matrix(regrets[1,])
        regrets <- as.matrix(regrets[-1,])
        regrets <- matrix(as.numeric(regrets), nrow=nrow(regrets), ncol=ncol(regrets))

        xlim <- range(regrets[,1])
	values <- unlist(regrets[,-1])
	ylim <- range(values[values>0])

	plot(x=regrets[,1], type='n', log=log, xlim=xlim, ylim=ylim, yaxs='i', xlab=xlab, ylab='Regret', main=if(is.na(main)) unlist(strsplit(regrets_file, "\\."))[[1]] else main)
        for(i in (2:ncol(regrets))) {
          lines(x=regrets[,1], y=regrets[,i],
                type="o", pch=i-1)
	}
	legend(x=legend.position, legend=sapply(names[-1], function(n) substring(n, 3)), pch=1:length(names[-1]))
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

draw.curves <- function (imin=5,imax=10,max=100, alpha=1, gamma=0.5,log="xy") {
	x <- 0.01*(imin:imax)*max
	rnd <- sapply(x, function (x) 0.2*exp(-gamma*x/2))
	grd <- sapply(x, function (x) 0.1*exp(-gamma*x))
	ucb <- sapply(x, function (x) 0.2*x^(-alpha))
        x <- 100*x
	ylim <- range(rnd, grd, ucb)
	plot(x=x, y=rnd, log=log, type='o', pch=1, ylim=ylim, xlab="samples", ylab="regret")
	lines(x=x, y=grd, type='o', pch=2)
	lines(x=x, y=ucb, type='o', pch=3)
	legend(x='topright', legend=c('RND','GRD','UCB'),pch=c(1,2,3))
}

make.pdfs <- function() {
  for(prefix in c('flat-low-key', 'flat-trilevel', 'tree-trilevel', 'tree-identity')) {
    for(k in c('16', '32', '64')) {
      basename <- paste(prefix, '-k=', k, '-uqb=8', sep='')
      pdf(file=paste(basename, 'pdf', sep='.'), width=6, height=4.5)
      draw.regrets(paste(basename,'txt', sep='.'), legend.position='right', main='')
      dev.off()
    }
  }
}


go.times <- c('5000', '7000', '10000', '15000')
go.pchs <- list('5000'=1, '7000'=2, '10000'=3, '15000'=4)
go.draw.curves <- function(agent="vct") {
  res <- list()
  lo <- 100.0
  hi <- 0.0
  for(time in go.times) {
    fname <- paste("uct", agent, "-time=", time, ".shomersu.dat", sep="")
    res[[time]] <- read.table(fname, header=T)
    res[[time]]$MAXVOI <- res[[time]]$MAXVOI+1E-8
    res[[time]]$WIN_W <- 100-res[[time]]$WIN_W*100 
    if(lo > min(res[[time]]$WIN_W)) lo <- min(res[[time]]$WIN_W)
    if(hi < max(res[[time]]$WIN_W)) hi <- max(res[[time]]$WIN_W)
  }
  lo <- max(lo, 30)
  hi <- min(hi, 70)
  

  par(xpd=F, mgp=c(2.5, 1, 0))
  plot(res[['5000']]$MAXVOI, res[['5000']]$WIN_W, ylim=c(lo,hi), log="x", type='n',
       main=NA, xlab='c', ylab='VOI wins, %', xaxs='i', xaxp=c(1E-7, 1, 1))
  abline(h=50, lty='longdash')
  abline(v=1.01E-6, lty='dashed')
  par(xpd=NA)
  segments(x0=1E-8, y0=lo, y1=lo-3)
  text(x=1E-8, y=lo-6, "0")
  segments(x0=1.01E-6, y0=lo, y1=lo-3)
  text(x=1.01E-6, y=lo-6, "1e-06")
  segments(x0=1.E-4, y0=lo, y1=lo-3)
  text(x=1.E-4, y=lo-6, "1e-04")

  par(xpd=F)
  for(time in go.times) {
    lines(res[[time]]$MAXVOI, res[[time]]$WIN_W, type='o', pch=go.pchs[[time]])
  }

  legend(x='topright', legend=go.times, pch=sapply(go.times, function(time) go.pchs[[time]]))
}

go.draw.bests <- function(bestsfile, colors=rainbow(3), densities=c(20, 20, 30)) {
  angles <- c(70, -45, 30)
  bests <- read.table(bestsfile, header=T)
  barplot(t(as.matrix(bests[,-1])), names.arg = bests$NSMPLS,
          density=densities, angle=angles, col=colors, beside=T)
  legend(x="topright", names(bests)[-1], angle=angles,
         density=densities, fill=colors, bty="n", ncol=3, bg=rainbow(3))
}
