(uiop:define-package #:reblocks/doc/continuations
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection))
(in-package #:reblocks/doc/continuations)


(defsection @continuations (:title "Continuations-based tools")
  "
> **Warning!** Continuations support support was removed in the Reblocks fork.

This feature will reborn as a separate library with a custom widget class.
")
