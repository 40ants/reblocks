(defsystem "reblocks-docs"
  :class :package-inferred-system
  :author "Alexander Artemenko"
  :licence "LLGPL"
  :description "A documentation for Reblocks, Common Lisp web framework."
  :homepage "https://40ants.com/reblocks"
  :source-control (:git "https://github.com/40ants/reblocks")
  :depends-on ("reblocks"
               "reblocks/doc/index"
               "reblocks/doc/changelog"))
