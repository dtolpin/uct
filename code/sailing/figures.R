draw.costs.of.factor <- function(costs_file, legend.position='bottomright', log="x") {
  costs <- read.table(costs_file, header=T)
  algorithms <- names(costs)[-1]
  xlim <- range(costs$FACTOR)
  ylim <- range(costs[algorithms])
  plot(x=costs$nsamples, type='n', log=log, xlim=xlim, ylim=ylim, xaxs='i',
       xlab='exploration factor', ylab='cost',
       main=do.call(paste,as.list(unlist(strsplit(costs_file,
         "-|\\.txt",fixed=FALSE)))[-1]))
  i <- 1
  for(alg in algorithms) {
    lines(x=costs$FACTOR, y=costs[[alg]],
          type="o", pch=i)
    i <- i+1
  }
  legend(x=legend.position, legend=algorithms, pch=1:length(algorithms))
}

draw.costs.of.nsamples <- function(costs_file, legend.position='right', log="x") {
  costs <- read.table(costs_file, header=T)
  algorithms <- names(costs)[-1]
  xlim <- range(costs$SAMPLES)
  ylim <- range(costs[algorithms])
  print(costs)
  plot(x=costs$nsamples, type='n', log=log, xlim=xlim, ylim=ylim, xaxs='i',
       xlab='samples per node', ylab='cost',
       main=do.call(paste,as.list(unlist(strsplit(costs_file,
         "-|\\.txt",fixed=FALSE)))[-1]))
  i <- 1
  for(alg in algorithms) {
    lines(x=costs$SAMPLES, y=costs[[alg]],
          type="o", pch=i)
    i <- i+1
  }
  legend(x=legend.position, legend=algorithms, pch=1:length(algorithms))
}
  
