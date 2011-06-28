draw.costs.of.factor <- function(costs_file, legend.position='bottomright',maxfactor=1.5) {
  algorithms <- c('RANDOM', 'UCT', 'VCT')
  costs <- read.table(costs_file, header=T)
  xlim <- range(costs$FACTOR[costs$FACTOR<=maxfactor])
  costs$RANDOM <- mean(costs$RANDOM)
  ylim <- range(min(costs[algorithms]), costs$RANDOM)
  plot(x=costs$nsamples, type='n', log="x", xlim=xlim, ylim=ylim, xaxs='i',
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

draw.costs.of.nsamples <- function(costs_file, legend.position='right', minsamples=80) {
  algorithms <- c('RANDOM', 'UCT', 'VCT', 'OPT')
  costs <- read.table(costs_file, header=T)
  xlim <- range(costs$SAMPLES[costs$SAMPLES>=minsamples])
  costs$RANDOM <- mean(costs$RANDOM)
  ylim <- range(min(costs[algorithms]), costs$RANDOM)
  plot(x=costs$nsamples, type='n', log="", xlim=xlim, ylim=ylim, xaxs='i',
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
  