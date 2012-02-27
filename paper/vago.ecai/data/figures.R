draw.regrets <- function(regrets_file, log="xy", legend.position='topright', xlab=expression(N[samples]), main=NA) {
	regrets <- read.table(regrets_file, header=F)

        names <- as.matrix(regrets[1,])
        regrets <- as.matrix(regrets[-1,])
        regrets <- matrix(as.numeric(regrets), nrow=nrow(regrets), ncol=ncol(regrets))

        xlim <- range(regrets[,1])
	values <- unlist(regrets[,-1])
	ylim <- range(values[values>0])

	plot(x=regrets[,1], type='n', log=log, xlim=xlim, ylim=ylim, yaxs='i', xlab=xlab, ylab='Regrets', main=if(is.na(main)) unlist(strsplit(regrets_file, "\\."))[[1]] else main)
	for(i in (2:ncol(regrets))) {
          lines(x=regrets[,1], y=regrets[,i],
                type="o", pch=i-1)
	}
	legend(x=legend.position, legend=sapply(names[-1], function(n) substring(n, 3)), pch=1:length(names[-1]))
}


draw.winrate <- function(regrets_file, log="xy", legend.position='topright', xlab=expression(N[samples]), main=NA) {
	regrets <- read.table(regrets_file, header=F)

        names <- as.matrix(regrets[1,])
        regrets <- as.matrix(regrets[-1,])
        regrets <- matrix(as.numeric(regrets), nrow=nrow(regrets), ncol=ncol(regrets))

    xlim <- range(regrets[,1])
    ylim <- range(max(0, min(regrets[,-1])*0.95), min(100, max(regrets[,-1])*1.05))

	plot(x=regrets[,1], y=regrets[,2], type='o', log=log, xlim=xlim, ylim=ylim, yaxs='i', xlab=xlab, ylab='VOI wins, %', main=if(is.na(main)) unlist(strsplit(regrets_file, "\\."))[[1]] else main, axes=F)
	box()
	axis(1, at=regrets[,1])
	axis(2)
	abline(h=50.0, lty='dashed');
}


