draw.costs.of.factor <- function(costs_file, legend.position='bottomright', log="x",main=NA) {
  costs <- read.table(costs_file, header=T)
  algorithms <- names(costs)[-1]
  xlim <- range(costs$FACTOR)
  ylim <- range(costs[algorithms])
  plot(x=costs$nsamples, type='n', log=log, xlim=xlim, ylim=ylim, xaxs='i',
       xlab='exploration factor', ylab='cost',
       main=if(is.na(main)) do.call(paste,as.list(unlist(strsplit(costs_file,
                            "-|\\.txt",fixed=FALSE)))[-1])
            else main)
  i <- 1
  for(alg in algorithms) {
    lines(x=costs$FACTOR, y=costs[[alg]],
          type="o", pch=i)
    i <- i+1
  }
  legend(x=legend.position, legend=algorithms, pch=1:length(algorithms), bg='white')
}

draw.costs.of.nsamples <- function(costs_file, legend.position='right', log="x", main=NA) {
  costs <- read.table(costs_file, header=T)
  algorithms <- names(costs)[-1]
  xlim <- range(costs$SAMPLES)
  ylim <- range(costs[algorithms])
  print(costs)
  plot(x=costs$nsamples, type='n', log=log, xlim=xlim, ylim=ylim, xaxs='i',
       xlab='samples per node', ylab='cost',
       main=if(is.na(main)) do.call(paste,as.list(unlist(strsplit(costs_file,
                            "-|\\.txt",fixed=FALSE)))[-1])
            else main)
  i <- 1
  for(alg in algorithms) {
    lines(x=costs$SAMPLES, y=costs[[alg]],
          type="o", pch=i)
    i <- i+1
  }
  legend(x=legend.position, legend=algorithms, pch=1:length(algorithms), bg='white')
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
