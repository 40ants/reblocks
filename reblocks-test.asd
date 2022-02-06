(defsystem "reblocks-test"
  :class :package-inferred-system
  :pathname "t"
  :depends-on ("reblocks"
               "hamcrest"
               "reblocks-test/dependencies"
               "reblocks-test/hooks"
               "reblocks-test/reblocks"
               "reblocks-test/request"
               "reblocks-test/response"
               "reblocks-test/request-handler"
               "reblocks-test/actions"
               "reblocks-test/commands"
               "reblocks-test/default-init"
               "reblocks-test/widgets/mop"
               "reblocks-test/widgets/macroexpansion"
               "reblocks-test/widgets/render-methods")
  :perform (test-op (o c)
                    (unless (symbol-call :rove '#:run c)
                      (error "Tests failed"))))


(register-system-packages "lack-test" '(#:lack.test))
(register-system-packages "lack-request" '(#:lack.request))
