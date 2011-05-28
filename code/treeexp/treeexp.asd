(in-package "ASDF")

(defsystem "treeexp"
    :name "Tree Experiments"
    :author "David Tolpin"
    :depends-on ("mcts")
    :components ((:file "treeexp")))