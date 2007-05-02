
(in-package weblocks-test)

;;; test render-debug-toolbar
(deftest-html render-debug-toolbar-1
    (weblocks::render-debug-toolbar)
  (:div :class "debug-toolbar"
	(:a :href "?action=debug-reset-sessions"
	    :title "Reset Sessions"
	    (:img :src "pub/reset.png"))))

;;; test initialize-debug-actions
(deftest initialize-debug-actions-1
    (with-request :get nil
      (weblocks::initialize-debug-actions)
      (apply #'values (mapcar (lambda (str)
				(not (null (session-value str))))
			      '("debug-reset-sessions"))))
  t)
