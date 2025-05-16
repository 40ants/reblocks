(uiop:define-package #:reblocks/doc/contribute
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection))
(in-package #:reblocks/doc/contribute)


(defsection @contribute (:title "Contributing to Reblocks"
                         :external-links (("semver" . "https://semver.org/")
                                          ("discuss" . "https://github.com/40ants/reblocks/discussions")))
  "
0. Discuss a problem [at the GitHub discussion][discuss].
1. Checkout the branch.
2. Make changes.
3. Add tests to the `t/` subdirectory.
4. Test changes: `(asdf:test-system :reblocks)`.
5. Ensure you've updated documentation in the `docs` folder if you've
   changed or added functionality.
6. Describe your changes for in a readable way ontop of the
   `changelog.lisp`. Increment a version number according to [Semantic Versioning][semver].
7. Create a pull-request and make the world a better place!
")
