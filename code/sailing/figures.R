draw.costs.of.factor <- function(costs_file, legend.position='bottomright', log="x",main=NA) {
  costs <- read.table(costs_file, header=F)
  names <- as.matrix(costs[1,])[-1]
  costs <- as.matrix(costs[-1,])
  costs <- matrix(as.numeric(costs), nrow=nrow(costs), ncol=ncol(costs))

  fctr <- costs[,1]
  fctr <- fctr*fctr*4
  xlim <- range(fctr)
  ylim <- range(unlist(costs[,-1]))
  plot(x=fctr, type='n', log=log, xlim=xlim, ylim=ylim, xaxs='i',
       xlab='exploration factor', ylab='cost',
       main=if(is.na(main)) do.call(paste,as.list(unlist(strsplit(costs_file,
                            "-|\\.txt",fixed=FALSE)))[-1])
            else main)

  for(i in (2:ncol(costs))) {
    lines(x=fctr, y=costs[,i],
          type="o", pch=i-1)
  }
  legend(x=legend.position, legend=names, pch=1:length(names), bg='white')
}

draw.costs.of.nsamples <- function(costs_file, legend.position='right', log="x", main=NA) {
  costs <- read.table(costs_file, header=F)
  names <- as.matrix(costs[1,])[-1]
  costs <- as.matrix(costs[-1,])
  costs <- matrix(as.numeric(costs), nrow=nrow(costs), ncol=ncol(costs))

  smpls <- costs[,1]
  xlim <- range(smpls)
  ylim <- range(unlist(costs[,-1]))
  plot(x=smpls, type='n', log=log, xlim=xlim, ylim=ylim, xaxs='i',
       xlab='samples per node', ylab='cost',
       main=if(is.na(main)) do.call(paste,as.list(unlist(strsplit(costs_file,
                            "-|\\.txt",fixed=FALSE)))[-1])
            else main)
  for(i in (2:ncol(costs))) {
    lines(x=smpls, y=costs[,i],
          type="o", pch=i-1)
  }
  legend(x=legend.position, legend=names, pch=1:length(names), bg='white')
}
  
draw.rcq <- function() {
	layout(cbind(c(1,2), c(3,4))); 
	for(x in c(199, 397, 793, 1585)) 
		draw.costs.of.factor(paste('../../exp/sailing/rcq-size=6-nsamples=', x, '.txt', sep=''),
							 legend.position='right')
}
							 

make.pdfs <- function () {
  for(size in c('3', '6', '10')) {
    for(group in c('minimum', 'median')) {
      basename <- paste('costs-size=', size, '-group=', group, sep='')
      pdf(file=paste(basename, 'pdf', sep='.'), width=6, height=4.5)
      draw.costs.of.nsamples(paste(basename, 'txt', sep='.'), main='',
                             legend.position='topright')
      dev.off()
    }
    basename <-paste('costs-size=', size, '-nsamples=397', sep='')
    pdf(file=paste(basename, 'pdf', sep='.'), width=6, height=4.5)
    draw.costs.of.factor(paste(basename, 'txt', sep='.'), main='',
                         legend.position='right')
    dev.off()
  }
  for(prefix in c('costs', 'rcq')) {
    for(nsamples in c('199',  '397', '793', '1585')) {
      basename <-paste(prefix, '-size=6-nsamples=', nsamples, sep='')
      pdf(file=paste(basename, 'pdf', sep='.'), width=6, height=4.5)
      draw.costs.of.factor(paste(basename, 'txt', sep='.'), main='',
                           legend.position='topright')
      dev.off()
    }
  }
}
