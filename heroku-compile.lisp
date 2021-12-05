(declaim (optimize (debug 3)))

(ql:quickload :qlot)

(format t "*load-truename*: ~S~%"
        *load-truename*)

(let* ((raw-qlfile (uiop:merge-pathnames* #P"qlfile"
                                          (uiop:pathname-directory-pathname
                                           *load-truename*)))
       (qlfile (probe-file raw-qlfile)))
  
  (format t "qlfile: ~S~%"
          raw-qlfile)

  (unless qlfile
    (format t "qlfile not found!~%"))
  
  (qlot/install:install-qlfile qlfile)

  (qlot:with-local-quicklisp (qlfile)
    (push "./" asdf:*central-registry*)
    (ql:quickload "reblocks/doc/index")
    (ql:quickload "reblocks/doc/example")

    ;; These modules are required because sources will not be available at runtime
    ;; on Heroku and SLYNK will die tryng to do asdf:load-system unless we preload
    ;; these systems into the Lisp image
    (ql:quickload '(:slynk/arglists
                    :slynk/mrepl
                    :slynk/fancy-inspector
                    :slynk/package-fu
                    :slynk/trace-dialog
                    :slynk/stickers
                    :slynk/indentation))))
