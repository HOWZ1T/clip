(defsystem "clip"
  :version "0.1.0"
  :author "HOWZ1T@github.com"
  :license "LGPL 2.1"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main")
		 (:module "utils"
			  :components
			  ((:module "collections"
				    :components
				    ((:file "doubly-linked-list")))))
		 (:module "decoders"
			  :components
			  ((:file "png")))
		 (:module "encoders"
			  :components
			  ((:file "png")))
		 (:module "objects"
			  :components
			  ((:file "image"))))))
  :description "WIP"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "clip/tests"))))

(defsystem "clip/tests"
  :author "HOWZ1T@github.com"
  :license "LGPL 2.1"
  :depends-on ("clip"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for clip"

  :perform (test-op (op c) (symbol-call :rove :run c)))
