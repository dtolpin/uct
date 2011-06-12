(in-package "ASDF")

(defsystem "mcts"
    :name "MCTS"
    :author "David Tolpin"
    :components ((:file "mcts")))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system "mcts"))))
  (funcall (intern (symbol-name '#:test) (find-package "MCTS"))))

