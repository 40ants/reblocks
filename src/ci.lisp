(uiop:define-package #:reblocks/ci
  (:use #:cl)
  (:import-from #:40ants-ci/workflow
                #:defworkflow)
  (:import-from #:40ants-ci/jobs/linter)
  (:import-from #:40ants-ci/jobs/run-tests)
  (:import-from #:40ants-ci/jobs/docs)
  (:import-from #:40ants-ci/jobs/autotag
                #:autotag))
(in-package #:reblocks/ci)


(defworkflow release
  :on-push-to "master"
  :jobs ((autotag)))

(defworkflow linter
  :on-pull-request t
  :cache t
  :jobs ((40ants-ci/jobs/linter:linter
          :check-imports t)))


(defworkflow ci
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((40ants-ci/jobs/run-tests:run-tests
          :os ("ubuntu-latest"
               "windows-latest")
          :lisp ("sbcl-bin"
                 "ccl-bin")
          :exclude ((:os "windows-latest"
                         ;; CCL-BIN 1.13 has issues on Windows
                         ;; https://github.com/40ants/reblocks/actions/runs/10862718561/job/30146062047?pr=66
                     :lisp "ccl-bin"))
          :coverage t)))

(defworkflow docs
  :on-push-to "master"
  :on-pull-request t
  :cache t
  :jobs ((40ants-ci/jobs/docs:build-docs
          :asdf-system "reblocks-docs")))
