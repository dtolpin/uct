(in-package "ASDF")

(defsystem "sailing"
    :name "Sailing Strategy Simulator"
    :author "David Tolpin"
    :components 
    ((:file "package")
     (:file "model" :depends-on ("package"))
     (:file "test" :depends-on ("package" "model"))))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system "sailing"))))
  (funcall (intern (symbol-name '#:test) (find-package "SAILING"))))

