(let ((*make-arm* #'make-armb) (*make-tree* #'make-flat))
           (experiments :levels 1 :branching 16 :min-sf 4 :sf-step 1.5 :n-sf 12 :nruns 100))