(setf *read-default-float-format* 'double-float)

(in-package "ASDF")

(defsystem "treeexp"
    :name "Tree Experiments"
    :author "David Tolpin"
    :depends-on ("mcts")
    :components ((:file "treeexp")))