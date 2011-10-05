(let ((*make-arm* #'make-armb) (*make-tree* #'make-flat))
           (experiments :levels 1 :branching 16 :min-sf 4 :sf-step 1.5 :n-sf 12 :nruns 1000
                        :algorithms '(rnd uct gct tct hct)))

(let ((*uqb-alpha* 8)
      (*make-arm* #'make-armb)
      (*transform* #'identity)
      (*make-tree* #'make-flat))
           (experiments :nruns 100 :levels 2 :branching 32
                        :min-sf 1 :sf-step ( sqrt 2) :n-sf 10))
