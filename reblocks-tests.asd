(defsystem "reblocks-tests"
  :class :package-inferred-system
  :pathname "t"
  :depends-on ("reblocks"
               "hamcrest"
               "reblocks-tests/dependencies"
               "reblocks-tests/hooks"
               "reblocks-tests/html"
               "reblocks-tests/reblocks"
               "reblocks-tests/request"
               "reblocks-tests/response"
               "reblocks-tests/request-handler"
               "reblocks-tests/actions"
               "reblocks-tests/commands"
               "reblocks-tests/default-init"
               "reblocks-tests/widgets/mop"
               "reblocks-tests/widgets/macroexpansion"
               "reblocks-tests/widgets/render-methods")
  :perform (test-op (o c)
                    (unless (symbol-call :rove '#:run c)
                      (error "Tests failed"))))


(register-system-packages "lack-test" '(#:lack.test))
(register-system-packages "lack-request" '(#:lack.request))
