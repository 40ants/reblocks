(uiop:define-package #:reblocks/doc/changelog
  (:use #:cl)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax))
(in-package #:reblocks/doc/changelog)

(in-readtable pythonic-string-syntax)


(40ants-doc/changelog:defchangelog (:ignore-words ("DSL"
                                                   "URL"
                                                   "URI"
                                                   "API"
                                                   "DOM"
                                                   "AJAX"
                                                   "JSON"
                                                   "JSON-RPC"
                                                   "SBCL"
                                                   "ASDF"
                                                   "MOP"
                                                   "JS"
                                                   "UI"
                                                   "GET"
                                                   "POST"
                                                   "LOADED"
                                                   "HTML"
                                                   "CSS"
                                                   "HTTP"
                                                   "TailwindCSS"
                                                   "WITH-HANDLED-ERRORS"
                                                   "REBLOCKS/ERROR-HANDLER"
                                                   "REBLOCKS/PAGE-DEPENDENCIES"
                                                   "REBLOCKS/SESSION:INIT")
                                    :external-links (("Ultralisp" . "https://ultralisp.org"))
                                    :external-docs ("https://40ants.com/log4cl-extras/"))
  (0.61.0 2024-09-14
          """
Fixes
=====

* Fixed an error inside REBLOCKS/ACTIONS:MAKE-JS-ACTION function when called without :ARGS. (Thanks to olivercsr@github for the PR)

""")
  (0.60.0 2024-07-28
          """
Incompatible Changed
====================

* JS function initiateFormAction now accepts four arguments instead of three.
  Second argument should be an event, third - the form object and fourth an options object.

  This change allows to submit form using different buttons having their values passed to a
  lisp callback.

  An `examples/simple-form.lisp` file was added to demonstrate how this feature works when
  form has Submit and Cancel buttons.

  **Note:** if you've used Reblocks-UI system, then you'll need to update it to the newest version.
""")
  (0.59.0 2024-01-26
          """
Changed
=======

* Generic-function REBLOCKS/WIDGET:GET-HTML-TAG now can return a list like `(list :input :onchange \"somevalue\")` to change not only HTML tag of the widget node, but also to add some arguments to it.
""")
  (0.58.0 2024-01-13
          """
Added
=====

A new function REBLOCKS/RESPONSE:STATUS-CODE was added as a replacement to REBLOCKS/RESPONSE:GET-CODE.
The new function is setfable. Now you can change HTTP status code of the currently rendered page.
This is especially helpful when rendering \"Not found\" or error pages.
""")
  (0.57.0 2024-01-08
          """
Added
=====

* Error about missing action now is continuable. In case if \"continue\" restart was selected,
  page is refreshed to update actions in the session.
* Now it is possible to use symbol as an action and it will be funcalled to handle an action.

""")
  (0.56.0 2024-01-01
          """
Added
=====

* Generic-function REBLOCKS/PAGE:BODY-CLASSES was added and can be used to specify classes
  for HTML body. This can be helpful when making a site using frameworks like `TailwindCSS`.
""")
  (0.55.0 2023-12-05
          """
Added
=====

* Optional argument :defer was added to javascript dependencies.
* Argument BASE-URI was added to REBLOCKS/RESPONSE:MAKE-URI function.
* Now Reblocks responds with Server-Timing and this performance information can be
  viewed in the browser. Read more about this header at
  [Mozilla's docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Server-Timing).
* Generic-function REBLOCKS/WIDGET:UPDATE now has an optional argument REMOVED.
  You can set it to T to remove widget from the DOM tree.

""")
  (0.54.0 2023-10-22
          """
Added
=====

Function REBLOCKS/REQUEST:GET-REMOTE-IP was added.

Fixed
=====

Now function REBLOCKS/ACTIONS:MAKE-ACTION-URL removed old `action` argument if it is present in the
current URL. This fixes the case when you want to make an action url from the callback processing
another action.

JavaScript function `initiateFormAction` now does not overwrites whole options.args object,
but only replaces the keys corresponding to a form data fields.

""")
  (0.53.0 2023-06-20
          """
Changed
=======

Now all commands added using REBLOCKS/RESPONSE:SEND-SCRIPT or REBLOCKS/COMMANDS:ADD-COMMAND
during the call to REBLOCKS/WIDGET:UPDATE generic-function, are added after the command
for updating DOM node on the frontend. This fixes a problem happened when you attach
some JS handlers to the DOM node, but after the update these handlers are deleted.
""")
  (0.52.0 2023-06-08
          """
Added
=====

* Documentation on REBLOCKS/WIDGETS/STRING-WIDGET:STRING-WIDGET class and it's methods.q
""")
  (0.51.0 2023-05-27
          """
Changed
=======

* Function REBLOCKS/ACTIONS:MAKE-JS-ACTION now accepts ARGS argument. If given, it will be serialized as a dictionary and embedded into the action calling code. This way you can pass the arguments back into your lisp callback.

Added
=====

* Added `includeCSS` and `includeJS` command handlers. This makes it possible to send such kind of commands via a websocket.
* Now all widgets rendered on a page are collected into a hash table and you can find them by dom-id using REBLOCKS/PAGE:FIND-WIDGET-BY-ID function.

""")
  (0.50.0 2022-12-03
          """
Fixed
=====

Fixed a bug when session lock was written into the session itself instead of into the
inner hash table.

Fixed error handling when debug-mode is off. Previously, the debugger was invoked before showing 500 error page to the user.

Now every response has \"Cache-Control: no-cache, no-store, must-revalidate\" header to prevent
caching of dynamic content in the browser. This fixes the issue when you did some changes using actions then went to
another page and returned using \"Back\" button in the browser. Previously browser was able to show you an old content of the
page.

Added
=====

### Current page abstraction

Added a notion of the current page. Now all actions are bound to some page and
pages can expire, clearing actions from the memory. This should prevent memory leak
leading to posible DoS attack. Page expiration is controlled by two values:

- A number of seconds while page should be considered alive. This prevents
  evil person from filling memory by creation of many separate sessions.
- A maximum number of pages per session. This may be needed to protect from
  opening too many pages within one session.

Also, you can use REBLOCKS/PAGE:EXTEND-EXPIRATION-TIME function to extend current page's
expiration time.

When all pages in the session are expired, session is removed from the memory too.

Generic-function REBLOCKS/PAGE:ON-PAGE-REFRESH was added.

### Cached widget dependencies

Another interesting feature is mixin class REBLOCKS/CACHED-DEPENDENCIES-MIXIN:CACHED-DEPENDENCIES-MIXIN.

Changed
=======

These functions were moved to separate package `REBLOCKS/PAGE-DEPENDENCIES`:

- with-collected-dependencies
- get-collected-dependencies
- push-dependencies
- already-loaded-p
- page-dependencies

New generic-function REBLOCKS/SESSION:INIT-SESSION was introduced. Define a method if you
need to add something into the user's session when it is initialized. This function will
be called once per user.

Generic function REBLOCKS/SESSION:INIT was replaced with REBLOCKS/PAGE:INIT-PAGE and it will
be called each time a user opens site in a new browser window or tab. When user refreshes
the page it will not be called.

Function REBLOCKS/DEBUG:RESET-LATEST-SESSION now provides a convenient restart to enable debug mode.

Deleted
=======

Function `reblocks/widgets/root:get` was deleted. Use `(page-root-widget (current-page))` instead.
Also `root-widget-key` and corresponding package `reblocks/widgets/root` were removed.

""")
  (0.49.0 2022-11-26
          """
New functional was added which allows to control how does HTTP middlewares list is generated for the server.
You might define your own REBLOCKS/SERVER:SERVER class and a method for REBLOCKS/SERVER:MAKE-MIDDLEWARES generic-function.
Inside this method, you might inject additional middlewares using REBLOCKS/SERVER:INSERT-MIDDLEWARE function.

Also, a variable REBLOCKS/SERVER:*DEFAULT-SAMESITE-POLICY* was introdiced, to control default value of the SameSite HTTP header.

As a bonus, loading of reblocks-docs system was fixed.
""")
  (0.48.0 2022-11-07
          """
Fixed
=====

Now Reblocks remembers which dependencies are already loaded into the page and
will not fetch them again. This fixes issue when it fetched jQuery and broke
it's extensions making further action calls made via GET instead of Ajax.
""")
  (0.47.0 2022-11-04
          """
Added
=====

Added ability to set cookie in response handler. You can do it in any render method.

Here is an example how to expire a cookie right now:

```lisp
(reblocks/response:set-cookie
     (list :name "auth_token"
           :value ""
           :path "/"
           :expires (get-universal-time)
           :secure t
           :samesite :lax))
```
""")
  (0.46.1 2022-09-16
          """
Fixed
=====

* Error page rendering was fixed. Now such variables as page description, tags and title are
  bound to their defaults while calling REBLOCKS/ERROR-HANDLER:ON-ERROR generic-function.
""")
  (0.46.0 2022-07-03
          """
Changed
=======

* Generic-function REBLOCKS/ERROR-HANDLER:ON-ERROR now accepts BACKTRACE argument which may
  contain a string with a backtrace. Previously, default method of this generic-function
  has rendered wrong backtrace when you aborted request in debugger.
* Also, a function LOG4CL-EXTRAS/ERROR:PRINT-BACKTRACE is used now to format a backtrace
  for error page and now it looks the same as backtrace in logs.
* Plus, a backtrace now logged only once when unhandled error happens.

Removed
=======

* Macro WITH-HANDLED-ERRORS is not exported from `REBLOCKS/ERROR-HANDLER` package anymore.
""")
  (0.45.3 2022-06-25
          """
Fixed
=====

* Fixed how REBLOCKS/PAGE:GET-TITLE and other similar functions change the state.
  Previously there was a bug and these functions have changed a global variables.
""")
  (0.45.2 2022-06-06
          """
Fixed
=====

* Fixed loading of CSS styles for new widgets appearing on a page.
* Fixed starting server in debug mode.

""")
  (0.45.1 2022-06-02
          """
Added
=====

* Added function REBLOCKS/PREVIEW:STOP to stop an app for previewing widgets during development.

Fixed
=====

* Fixed the way how dependencies are rendered in AJAX action response. Previously,
  when many widgets of the same class were updated on one user action Reblocks sent
  a duplicate css/js dependencies.
* Function REBLOCKS/REQUEST:REFRESH-REQUEST-P was fixed and now works as expected.
""")
  (0.45.0 2022-02-14
          """
Changed
=======

* Interactive demos now are rendered just as code block into markdown documents.
  Such code block is followed by a link to HTML documentation if ASDF system
  defines a `homepage`.
""")
  (0.44.0 2022-02-09
          """
Fixed
=====

* Fixed the way how welcome page app is started. Now it always started unless some
  other application does not use "/" prefix.
* Also, fixed the way how application is choosen depending on request url path.
* Function REBLOCKS/SERVER:STOP now removes all application and routes from the
  server instance. This way when you'll start it again, server might serve another
  set of the applications.
* Now if you redefine your app using REBLOCKS/APP:DEFAPP macro and change
  AUTOSTART argument from `T` to NIL, this app will be removed from the list
  of applications to autostart.

Added
=====

* Now macro REBLOCKS/APP:DEFAPP supports DOCUMENTATION argument. Use it to add documentation
  to application's class.

""")
  (0.43.0 2022-02-07
          """
Changed
=======

* Argument CACHE-IN-MEMORY was added to REBLOCKS/DEPENDENCIES:MAKE-DEPENDENCY function.
  It allows to compile local dependency into the memory. This way you can build a standalone
  executable webserver without JS and CSS dependencies!
* jQuery and some other dependencies now are cached in memory at compilation time.
""")
  (0.42.0 2022-02-06
          """
Fixed
=====

* Now each app set's its own root widget. Root widgets is choosen depending on current application.
  and current application depends on the url path prefix. Previously site with multiple apps
  always shown the same root widget.
* Fixed the error occured after you stopped the webserver and started it again using the same port.
  Previously Reblocks complained that the server is already running on this port.
* Fixed the way how Reblocks signal an error on missing action. Now it does this by default
  from the default implementation of the REBLOCKS/ACTIONS:ON-MISSING-ACTION generic-function.
  But if variable REBLOCKS/VARIABLES:*IGNORE-MISSING-ACTIONS* is `t`, then error will not be signaled
  and user will be redirected to the application's previx path.

Changed
=======

* Documentation examples server was changed to render a list of ASDF systems for which documentation
  was built.
* REBLOCKS/PREVIEW:PREVIEW is able to accept a widget's class as a symbol.
""")
  (0.41.0 2022-01-05
          """
Removed
=======

* Function `reblocks/utils/misc:asdf-system-directory`. Use `asdf:system-source-directory` instead.
""")
  (0.40.0 2022-01-03
          """
Added
=====

* Added a welcome screen app on "/" that lists the active apps.
* Generic-function REBLOCKS/WIDGET:CREATE-WIDGET-FROM now returns a more user-friendly error
  message, explaining that the widget is not of a good type, instead
  of "no applicable generic method".
* A new function REBLOCKS/PREVIEW:PREVIEW was added. Give it a widget and it will setup
  a server on a random port and show the widget in a browser!
* Ability to run many web servers each on it's own HTTP port. Each server can have
  it's own set of applications.
* Added a log in ``weblocks/app:start`` to show the prefix of the app
  that is started.
* Changed ``(weblocks/debug:on)`` and ``off`` so they set the log
  level to ``debug`` and ``warn``, respectively.
* Changed a library used to log unhandled errors.
  Now [log4cl-extras](https://github.com/40ants/log4cl-extras) is used,
  because it is a successor of [log4cl-json](https://github.com/40ants/log4cl-json).
* Function REBLOCKS/SERVER:STOP now accepts optional `interface` and `port` arguments.
* Function `reblocks/action::function-or-action->action` was renamed to REBLOCKS/ACTIONS:MAKE-ACTION and made public.
  Old `reblocks/action:make-action` was renamed to `reblocks/action::internal-make-action`.
* JS function initiateActionWithArgs now uses POST method by default.
* Function REBLOCKS/ACTIONS:MAKE-ACTION-URL is exported now and it keeps query params by default.

New Functions
=============

* REBLOCKS/SERVER:SERVERS
* REBLOCKS/SERVER:RUNNING-P

Removals
========

* Package `reblocks/widgets/base` was removed. All code now is in the `reblocks/widget` package,
  which was a nickname previously.
* `reblocks/app:get-active-apps`
* `reblocks/app:finalize-webapp`
* `reblocks/app:app-active-p`
* `reblocks/app:start`
* `reblocks/app:stop`
* `reblocks/app:restart`
* JS function `initiateActionWithArgsAndDeferredCallback` was removed because of leaky
  abstraction of a single deferred object per page.
* JS functions `initiateActionWithArgs` and `initiateActionWithArgsAndCallback` were combined
  into a single `initiateAction` function.
* JS functions `initiateFormAction` and `initiateFormActionWithCallback` were combined
  into a `initiateFormAction` function.

Also removed:

* A variable `*current-page-headers*`, define an :AFTER method for REBLOCKS/PAGE:RENDER-HEADERS instead.
* `*approved-return-codes*`
* `*current-page-keywords*`
* `*current-page-title*`
* `*style-warn-on-circular-dirtying*`
* `*style-warn-on-late-propagation*`
* `*rewrite-for-session-urls*`
* Functions `reblocks/routes:add-route` and `reblocks/routes:add-routes` were made internal.

Other changes
=============

If local dependency can't be found relative to the given ASDF system, we try to search
the same path relative to the current directory and throw error if file can't be found.
This could happen if you've compiled the webserver into a binary and moved it to another
container or machine.

REBLOCKS/SERVER:START function now accepts SAMESITE-POLICY argument which should be one of
keywords: :LAX, :STRICT or :NONE. This parameter modifies `SameSite` part of the cookies
sent to the client.

Hooks API was refactored in a backward incompatible manner. REBLOCKS/HOOKS:DEFHOOK macro now requires
to specify hook's arguments and all macros generated by the REBLOCKS/HOOKS:DEFHOOK also require arguments
specification.

jQuery was upgraded from 1.8.2 to 3.6.0. It's plugins jquery.ba-bbq.js and jquery-seq.js were
turned off.
""")
  (0.39.1 2020-01-20
          """
Fixed
-----

* Fixed processing of the value ``x-forwarded-port``. Previously, when Weblocks
  was used along with lastest (after the 2019-09-07) ``Woo`` server, this header
  lead to the 500 error. Because behavior of the ``Woo`` was changed:

  <https://github.com/fukamachi/woo/issues/84>

  Now Woo does not parses numeric headers and Weblocks has to do it itself.

""")
  (0.39.0 2019-09-16
          """
Changes
-------

* A new macro REBLOCKS/ROUTES:DEFROUTE was added.

  It defines a handler for a given route. By default route should return
  a serialized JSON:

```lisp
(defroute (app /api/data)
    "{\"my-data\": [1, 2, 3]}")
```

  but you can redefine the content type:

```lisp
(defroute (app /api/data :content-type "application/xml")
    "<my-data><item>1</item><item>2</item></my-data>")
```

  each route is associate with application class and these routes
  are added to the mapper when you call a ``start`` method.
""")
  (0.38.1 2019-08-02
          """
* A small fix to make it work with ``weblocks-websocket``.
""")
  (0.38.0 2019-07-07
          """
* Unhandled error processing was fixed. There were numerous problems with error processing:
  * Previosly server stopped to work because clack tried to write into a closed stream.
  * Also, before this fix, error page didn't showed propertly.
  * Any error in the route lead to a server stop because there wasn't any handler at all.
    Now they are handled with ``on-error`` method as errors in block/page rendering.

  All of them are fixed now.
""")
  (0.37.0 2019-06-01
          """
* Removed check if ``:bordeaux-threads`` is in the ``*features*``, because this does not work
  in runtime in precompiled binary.

""")
  (0.36.0 2019-05-03
          """
* Added function ``weblocks/session:expire``, which can be used to tell Weblocks to delete current session
  after request processing.
* Also, now session middleware is created explicitly along with memory store. This made possible to add
  another two functions to gather some statistics about number of sessions:

  * ``weblocks/session:get-number-of-sessions``;
  * ``weblocks/session:get-number-of-anonymous-sessions``;
  
* The function ``weblocks/widgets/base:get-html-tag`` was fixed to
  render tables correctly. If the widget's parent is a ``:table``
  element, then this function returns ``:tr`` (instead of a
  ``:div``). If it is ``:tr``, it returns ``:td``, and ``:div``
  otherwise.
* A new setting ``weblocks/html:*pretty-html*`` was created, to further control pretty printing of html
  both in code and in tests.

""")
  (0.35.3 2019-03-31
          """
* Added a ``weblocks/response:add-retpath-to`` function, useful to add a
  ``retpath`` GET parameter to the URL.

  For example, calling:

```lisp
(add-retpath-to "/login" :retpath
  "http://example.com:10050/foo/bar")

  Will return:

```lisp
"/login?retpath=http%3A%2F%2Fexample.com%3A10050%2Ffoo%2Fbar"
```

  Argument ``:retpath`` is optional. By default, function
  will take an URL of the current web page.

""")
  (0.35.2 2019-03-21
          """
* Subsystem ``weblocks/utils/i18n`` was added to asd file, because
  otherwise system ``weblocks-ui`` can't be installed from the [Ultralisp][ultralisp].
""")
  (0.35.1 2019-02-02
          """
* Previosly, when you called ``(weblocks/debug:off)``, subsequent
  call to ``(weblocks/debug:status)`` returned ``T``, but should return
  ``NIL``.

  This was fixed now.

"""
          )
  (0.35.0 2019-01-22
          """
Request handling pipeline was refactored.

The idea of this refactoring, is to separate roles of the functions
which process requests. Now ``weblocks/server:handle-http-request``
prepares data received from the ``Clack`` and calls
``weblocks/request-handler:handle-request`` to do the real job.

In it's turn, ``weblocks/request-handler:handle-request`` should return
an object of type ``weblocks/response:response`` containing a content,
HTTP status code and headers of the response. Any error signal, thrown
from the ``handle-request`` is considered by ``handle-http-request`` as
an "unhandled error" and returned with 500 HTTP status code.

Here is a list of changes:

* Macro ``weblocks/hooks:on-application-hook-handle-request`` was
  renamed to ``weblocks/hooks:on-application-hook-handle-http-request``.
* Generic-function ``weblocks/request-handler:handle-client-request``
  was renamed to ``weblocks/request-handler:handle-request`` and now
  this function should return either ``string`` or an object of type
  ``response``. Previously, it also made a non-local exit by throwing a
  tag, if request was aborted (for example to return a redirect
  response). But now it should only unwind a stack in case if some
  unhandled errow was thrown. Any condition of type
  ``weblocks/response:immediate-response`` will be catched inside a
  ``:around weblocks/request-handler:handle-request`` method and
  returned as usual ``response`` object.
* Function ``weblocks/response:abort-processing`` was renamed to
  ``weblocks/response:immediate-return``, symbols ``*code*``,
  ``*headers*`` and ``*content-type`` are not exported anymore. Instead
  of these global variables, use newly exported functions
  ``get-content``, ``get-code``, ``get-headers``, ``get-custom-headers``
  and ``get-content-type`` to extract information from the ``response`` object.
  * Package ``weblocks/response`` does not export symbols ``*code*`` and
  ``*content-type*`` anymore, but exports a function ``make-response``
  which can be used by a ``weblocks/request-handler:handle-reqiest`` to
  return response.

"""
          )
  (0.34.0 2019-01-19
          """
New
---

* Function ``weblocks/request:get-scheme`` was added and it is uses
  ``X-Forwarded-Proto`` header, if it is available.

Changes
-------

* Function ``weblocks/request:get-port`` now will use ``X-Forwarded-Port``
  header if it is available.
* Function ``weblocks/request:get-host`` now will use ``X-Forwarded-Host``
  header if it is available.

Fixes
-----

* Fixed issue introduced in ``0.33.2`` release - system
  ``lack-middleware-session`` was loaded successfully, but SBCL was
  unable to find ``lack-middleware-session`` package.
* Now HTML is pretty-printed by default on all implementations.
* Function ``weblocks/request:get-uri`` was fixed to work correctly when
  server is behind a reverse proxy which provides ``X-Forwarded-*`` headers.

"""
          )
  (0.33.2 2018-12-06
          """
Fixes
-----

* Added dependency on ``lack-middleware-session`` system
  because wee use it to store sessions.

"""
          )
  (0.33.1 2018-11-24
          """
Improvements
------------

* Now unhandled exceptions are logged with tracebacks if you are using
  ``log4cl-json`` library. To turn it on, just do:

```common-lisp
(log4cl-json:setup)
```

Fixes
-----

* Function ``response:redirect`` and it's JS counterpart was fixed
  and now work as expected.
* Request processing was streamlined and now
  ``weblocks/server:handle-request`` does not contain non-local
  exits. This fixes usage of the ``handle-request`` hook, because
  previously, if you wrap some code around ``(call-next-hook)``, then
  part following ``call-next-hook`` was ignored.

  For example:

```
(weblocks/hooks:on-application-hook-handle-request
  connect-to-database ()

  (let ((success nil))
    (unwind-protect (progn (setup-transaction)
                           (weblocks/hooks:call-next-hook)
                           (setf success t))
      (if success
          (commit)
          (rollback)))))
```

  Before this fix, ``rollback`` always called, because execution never
  hitted ``(setf success t)``. Now this is fixed.


"""
          )
  (0.33.0 2018-11-22
          """
Changes
-------

* Removed external symbol ``weblocks/app:make-uri`` because it wasn't
  fbound to any function after some refactoring.

  Use ``weblocks/response:make-uri`` instead.

Fixes
-----

* A separate file ``widget.lisp`` was added to define a
  ``weblocks/widget`` package. This solves issues when
  ASDF package inferred loading is unable to find ``weblocks/widget``
  system. Seems it plays badly when ``weblocks/widget`` is nickname for
  ``weblocks/widgets/base``.

"""
          )
  (0.32.1 2018-08-13
          """
Changes
-------

* A new hook was added - ``start-weblocks``. It is called around the
  code in ``(weblocks/server:start)`` and can be used to setup some
  additional routes, for example.

Fixes
-----

* Previosly, there was an error if you are using ASDF's package inferred
  system class and don't have explicit dependency on the ``weblocks``
  page.

  The error was raised during code loading:

  ```
  ;;; > Error: There is no applicable method for the generic function:
  >          #<STANDARD-GENERIC-FUNCTION WEBLOCKS/JS/BASE:MAKE-JS-BACKEND #x3020027E292F>
  >        when called with arguments:
  >          (:JQUERY)
  ```

  Now it is fixed and package ``weblocks/js/jquery`` is a dependency of
  ``weblocks/server`` and always loads.

"""
          )
  (0.32.0 2018-06-26
          """
* Now weblocks system explicitly requires ASDF >= 3.1, because it uses
  ``package-inferred-system`` class.

"""
          )
  (0.31.1 2018-06-16
          """
* Fixed error about missing 'bool type under SBCL.
"""
          )
  (0.31.0 2018-05-29
          """
* We don't enforce ``*print-pretty*`` inside of
  ``weblocks/html:with-html`` macro. This hack was required because
  the bug in the spinneret, which was fixed at 2018-01-04:

  <https://github.com/ruricolist/spinneret/commit/06b280612aff07cf376f593746d080230f2c7462>
"""
          )
  (0.30.1 2018-05-20
          """
* Error was fixed when you are trying to start a server which is already
  running. Now you have a "restart" to stop the old version of the
  server before starting the new one.

  However, it does not work with ``Woo`` server, because C library libev
  crashes with error:

```
Assertion failed: (("libev: a signal must not be attached to two different loops", !signals [w->signum - 1].loop || signals [w->signum - 1].loop == loop)), function ev_signal_start, file ev.c, line 4082
```


"""
          )
  (0.30.0 2018-05-19
          """
* Fixed the order of commands for frontend. Now they are returned in the chronological order.

"""
          )
  (0.29.0 2018-05-05
          """
Backward incompatibilities
--------------------------

Class ``weblocks/dependencies:dependency`` now requires that ``type``
attribute shoulde be of type ``(member :css :js :png :jpg :gif)``

Also, functions ``make-local-js-dependency``,
``make-local-css-dependency``, ``make-local-image-dependency``,
``make-remote-js-dependency`` and ``make-remote-css-dependency`` were
removed from package ``weblocks/dependencies``. Use ``make-dependency``
function, it will figure out which dependency type to create itself.

Improvements
------------

A new function ``weblocks/debug:get-session-value`` was added. It can be
used to get values from the last session seen by weblocks.

"""
          )
  (0.28.0 2018-04-23
          """
Error handling was fixed. Previously it aborted Woo's worker thread and
break the server.

Variable ``weblocks/variables::*catch-errors-p*`` was renamed to
``weblocks/variables::*invoke-debugger-on-error*`` and it's value was
inverted. If this variable is True, then debugger will be
invoked. Otherwise - Weblocks will return 500 error page.

Argument ``:invoke-debugger-on-errors`` of ``weblocks/debug:on``
function was renamed to ``:invoke-debugger-on-error``.

Method ``weblocks/error-handler:on-error`` now is called when you abort
request processing from the debugger. It is called with current app as
the first argument and the ``nil`` instead of condition.

"""
          )
  (0.27.2 2018-04-09
          """
Fixed a typo in  ``string-widget`` and ``funcall-widget`` package definitions.
"""
          )
  (0.27.1 2018-04-09
          """
Now ``string-widget`` and ``funcall-widget`` depends on
``weblocks/widgets/base`` instead of ``weblocks/widget``.

"""
          )
  (0.27.0 2018-03-11
          """
Reloading of the defapp definition now does not tries to restart an
application. Previously, restart caused the problem – when there is only
one application, whole Weblocks server was shut down. So, I've removed
this implicit action.

Code which logs action result on the client-side was improved.

"""
          )
  (0.26.0 2018-02-20
          """
Symbols ``add-application-hook``, ``add-request-hook``,
``add-session-hook``, ``prepare-hooks`` and ``call-hook``
aren't exported from ``weblocks/hooks`` anymore. Use new macro
``defhook`` instead.

Here is how it works:

You use ``defhook`` as the toplevel form of your file if you want to define
a new hook. This macro will create few other macroses in
``weblocks/hooks`` package and will export them. For example:

```common-lisp
(defhook database-opened
   "This hook is called when your application opens a database.")
```

This code will add these macroses into the ``weblocks/hooks`` package:
``on-session-hook-database-opened``,
``on-request-hook-database-opened``,
``on-application-hook-database-opened``,
``with-database-opened-hook`` and
``call-database-opened-hook``.

You need to wrap code, which opens a database, with
``with-database-opened-hook``:

```common-lisp
(weblocks/hooks:with-database-opened-hook ()
   (do-some-staff-to-open-database))
```

And in any other piece of code, you can define callbacks, using one of
other three macroses:

```common-lisp
(weblocks/hooks:on-session-hook-database-opened
    log-database-opening ()

  (weblocks/hooks:call-next-hook)
  (log:info "Database was opened"))
```

Usage of ``defhook`` macro gives more transparency to all defined hooks,
because all of them now visible as external symbols in
``weblocks/hooks`` package.
"""
          )
  (0.25.2 2018-02-04
          """
System ``weblocks/hooks`` now depends on ``log4cl`` and
``metatilities``, because previously sometimes it was impossible to load ``weblocks``.

"""
          )
  (0.25.1 2018-02-04
          """
Old tests for widgets, removed from core framework were removed.

Tests for widget's MOP methods were ported to Rove.

Package ``weblocks/utils/close`` now loaded with main ``:weblocks``
system.

Few old widget tests were removed.

Added function ``weblocks/session:reset`` which resets current session.

"""
          )
  (0.25.0 2018-01-31
          """
Good news, everyone!
--------------------

Quickstart tutorial was fixed!

Widget related changes
----------------------

Function ``render-widget`` was replaced with ``weblocks/widget:render
:around`` method. Method implementation were moved to
``weblocks/widgets/render-methods``.

Funcall-widget's package was refactored to conform package inferred
requirements.

Actions processing
------------------

Function ``make-js-action`` was moved to ``weblocks/actions`` package.

Application and server restarting
---------------------------------

Package ``weblocks/app`` now exports functions ``stop`` and ``restart``.
Previously they were internal and were called like ``stop-webapp`` and
``restart-webapp``.

Package ``weblocks/server`` does not export ``start-weblocks`` and
``stop-weblocks`` functions. They were replaced with just ``start`` and
``stop``. Also, it's internal generics ``start`` and ``stop`` were
renamed and transformed to ``start-server`` and ``stop-server``
functions.

Session initialization changes
------------------------

File ``default-application.lisp`` was removed, because now every
application has default session initialization method.

File ``default-init.lisp`` was refactored. Now it provides default
method for ``weblocks/session:init`` generic and ``:around`` method for
same generic, which allows end user just to return a string or a
function from his ``init`` method.

Now user can return any string or function from his method
``weblocks/session:init`` and it will be passed to a new generic
``weblocks/widget:create-widget-from`` to create a root widget. You can
also define ``create-widget-from`` for you custom types.

Debug
-----

Function ``weblocks/debug:on`` now turns on mode when Weblocks does not
intercept unhandled conditions, but calls an interactive debugger.

Removals
--------

* ``session-name-string-pair`` this function was removed in previous
  releases, but didn't find it's way to the changelog, still used in
  some older tests not ported to the Rove.

* Macro ``defrender`` was removed.

"""
          )
  (0.24.0 2018-01-29
          """
All rendering code was refactored.

Macroses ``with-html`` and ``with-html-to-string`` replaced
with ``weblocks.html:with-html`` and ``weblocks.html:with-html-string``.
Stream ``*weblocks-output-stream*`` was moved to
``weblocks.html::*stream*`` and is not external anymore. Please, don't
use it directly.

Widget refactorings
===================

Procedure ``update-widget-tree`` was removed and not widgets can't
change html header's tags, description, title, etc. If you need this,
change them in the ``render`` method.

Macro ``root-widget`` was removed and replaced with function
``weblocks.widgets.root:get``.

Request level
=============

Functions ``post-action-redirect``, ``post-render-redirect`` and
``initial-request-p`` were removed from ``weblocks`` package.

Function ``pure-request-p`` was moved to ``weblocks.request`` package.

Variable ``*json-content-type*`` was removed.

Variable ``*latest-request*`` was moved to
``weblocks.debug:*latest-request*``.

Functions ``parse-location-hash``, ``ajax-request-p`` were moved to
``weblocks.request`` package.

Function ``redirect`` was moved to ``weblocks.response:redirect``.
Functionality, related to opening another window instead of redirection
or deferring redirection until the end of action or rendering was
removed.

Request handler
===============

Functions ``remove-duplicate-dirty-widgets``,
``update-location-hash-dependents`` and ``update-widget-tree`` were
removed.

Call to ``weblocks::update-dialog-on-request`` from
``handle-client-request`` was commented.


Error handler
=============

Generic method ``weblocks/error-handler:on-error`` now accepts two
arguments - application object and condition.


Application level
=================

All code from ``uri-parameters-slotmap.lisp`` was removed.

All code, related to application class, was moved to the package
``weblocks.app``. Base aplication class was renamed to
``weblocks.app:app``, and macro for definition of the new
application was renamed to ``weblocks.app:defapp``.

All code related to application's metaclass, was moved to
the package ``weblocks.app-mop``. Metaclass was renamed to
``weblocks.app-mop:app-class``.

Application's slot ``html-indent-p`` and corresponding accessor
``weblocks-webapp-html-indent-p`` were removed because now spinneret
generates non indented code.

Slot ``init-user-session`` was completely removed and replace with a generic
``weblocks.session:init``.

These dependency related slots and accessors were removed:

* ``application-dependencies``
* ``weblocks-webapp-application-dependencies``
* ``bundle-dependency-types``
* ``version-dependency-types``
* ``gzip-dependency-types``

And macro for defining a special readers for them was removed as well:
``def-debug-p-slot-readers``.

Also, these arguments to ``defapp`` was removed:
``:ignore-default-dependencies``, ``:dependencies``

Function ``update-thread-status`` and method ``webapp-update-thread-status``
were removed.

Function ``get-webapps-for-class`` was renamed to ``app-active-p`` and
now returns ``t`` if application of given class is already active.

Function ``start-webapp`` was renamed to ``weblocks.app:start``.

Function ``get-webapp`` was renamed to ``get-active-app`` and optional
argument ``error-p`` was renamed to keyword argument ``signal-error``.

Function ``find-app`` was removed.

Function ``in-webapp`` was moved to ``weblocks.debug:in-app``.

Variable ``*default-webapp*`` was removed.

Variable ``*active-webapps*`` was renamed to
``weblocks.app::*active-apps*`` and made internal. Use
``weblocks.app:get-active-apps`` function.

Reader ``weblocks-webapp-prefix`` was renamed to
``weblocks.app:get-prefix``.

Slot ``default-store-name`` and its accessor
``webapp-default-store-name`` were removed.

Variable ``*current-webapp*`` was moved to
``weblocks.variables::*current-app*``.

Functions ``compute-webapp-public-files-uri-prefix``,
``compute-webapp-public-files-uri-prefix``,
``compute-webapp-public-files-path``,
``make-webapp-public-file-uri``,
``weblocks-webapp-public-files-cache-time`` and variable
``*default-public-files-path*`` were removed because
now there is another way to serve static.

Function ``webapp-serves-hostname`` was renamed to
``weblocks.app:app-serves-hostname-p`` and now accepts app as the first
argument and hostname as the second.


Variable ``*uri-tokens*`` was removed and weblocks does not set
'last-request-uri session value to all uri tokens anymore.

Macro ``with-webapp`` was moved to ``weblocks.app:with-app``.

Function ``webapp-permanent-action`` was moved to
``weblocks.app-actions:get-action``.

Function ``add-webapp-permanent-action`` was moved to
``weblocks.app-actions:add-action`` and ``remove-webapp-permanent-action`` to
``weblocks.app-actions:remove-action``.

Macroses ``define-permanent-action`` and ``define-permanent-action/cc``
were moved to ``weblocks.app-actions:define-action`` and
``weblocks.app-actions:define-action/cc``.

Function ``make-webapp-uri`` was removed, use
``weblocks/response:make-uri`` instedad.

Accessor ``webapp-js-backend`` was renamed to get-js-backend

These functions were moved into the separate package
``weblocks.current-app`` and renamed:

* ``webapp-prefix`` -> ``get-prefix``;

Actions and commands
====================

Function ``weblocks.actions:add-command`` was moved to
``weblocks.commands``.

Function ``weblocks:get-request-action`` was moved to
``weblocks/actions:get-request-action``

Keyword argment ``:action`` was removed from action calls.

Javascript
==========

Package ``weblocks.js`` was renamed to ``weblocks/js/base``.

Functions ``escape-script-tags``, ``%js`` and macroses
``with-javascript``, ``with-javascript-to-string`` were moved to the
package ``weblocks/js/base``.

Variables
=========

These variables were moved from ``weblocks`` package to
``weblocks/variables``:

* ``*current-page-title*``
* ``*current-page-keywords*``
* ``*current-page-headers*``
* ``*rewrite-for-session-urls*``
* ``*default-content-type*``
* ``*ignore-missing-actions*``

Symbols moved from :weblocks to other packages
==============================================

To :weblocks/widgets/dom
------------------------

* ``dom-object-mixin``
* ``dom-id``

To :weblocks/utils/uri
----------------------

* ``request-uri-path``
* ``add-get-param-to-url``
* ``remove-parameter-from-uri``

To :weblocks/linguistic/grammar
-------------------------------

* ``pluralize``
* ``singularize``
* ``proper-number-form``
* ``vowelp``
* ``consonantp``
* ``proper-indefinite-article``
* ``articlize``
* ``*current-locale*``
* ``current-locale``
* ``russian-proper-number-form``
* ``noun-vocative-to-genitive``
* ``*debug-words-forms*``
* ``*debug-words-genders*``
* ``determine-gender``

To weblocks/utils/warn
----------------------

* ``style-warn``
* ``webapp-style-warning`` renamed to ``style-warning``.
* ``non-idempotent-rendering``
* ``misunderstood-action``

To weblocks/actions
-------------------

* ``function-or-action->action``
* ``make-action``
* ``generate-action-code``


Removals
========

To make Weblocks core smaller, many files were removed: ``views``, ``widgets``,
``html-parts``, ``utilities``.

Systems ``weblocks-util``, ``weblocks-testutils`` were removed.

Accessor ``dom-class`` and generic function ``dom-classes`` were removed
and replaced with generic function ``weblocks/widget:get-css-classes``.

Generic function ``weblocks:handle-error-condition`` was removed.

Variable ``*dirty-widgets*`` was removed along with
``render-dirty-widgets`` function.


Dependencies
============

Rendering of remote (non cached) dependencies was fixed.

"""
          )
  (0.23.0 2018-01-11
          """
* Symbol ``weblocks.routes:*routes*`` is not external anymore.
  Use ``weblocks.routes:add-route`` and ``weblocks.routes:get-route``
  to add new routes and to search a route matched on a path.
* Fixed getting the rendered widgets in ``weblocks.widget:update``
  method when making ``:update-widget`` or ``:insert-widget (before)``
  commands.
* Temporary added method ``weblocks::child-of-p`` for new type of
  widget. This should fix some issues, with widgets updating.

"""
          )
  (0.22.2 2018-01-07
          """
* Class ``weblocks.widget:widget`` was exported, to make possible to
  define widgets based on it and some mixins.

"""
          )
  (0.22.1 2018-01-07
          """
* Code broken in previos release was fixed.
"""
          )
  (0.22.0 2018-01-06
          """
Most functions from ``weblocks.request`` were refactored and renamed:

* ``request-parameters`` -> ``get-parameters``;
* ``request-parameter`` -> ``get-parameter``;
* ``request-header`` -> ``get-header``;
* ``remove-request-header`` -> ``remove-header``;
* ``request-server-name`` -> ``get-host``;
* ``request-server-port`` -> ``get-port``;
* ``request-uri`` -> ``get-uri`` (and now it returns full URI with
  scheme, host and port;
* ``request-path-info`` -> ``get-path`` (and now it has keyword argument
  ``with-params`` to copy behaviour of old ``request-uri`` and return
  strings like ``/some/path?with=parameters``;
* ``request-method`` -> ``get-method``.

  All these function now accept keyword argument ``:request``. Previously
  it was ``&optional``.

  Another change is a new function ``weblocks.response:make-uri``. It can
  be used to build new uri, based on the uri of the current request. This
  can be useful when embedding links into emails, for example.

   **Warning!** These changes require a newer version of Lack.

   I've made a pull request <https://github.com/fukamachi/lack/pull/31>
   it is not merged yet, so, alternative version of Lack can be used, by
   installing it using Qlot, from here:

   <https://github.com/40ants/lack>

"""
          )
  (0.21.0 2018-01-01
          """
* Macro ``weblocks.session:get-value`` was replaced with a regular
  function.
* Function ``weblocks.session:set-value`` was removed and replaced with
  a setter ``(setf get-value)``.

"""
          )
  (0.20.1 2017-12-20
          """
* Removed debug these debug messages from client-side JS:

  * LOADED;
  * Starting AJAX;
  * Stopping AJAX progress;
  * Some AJAX error;
  * Action success.

"""
          )
  (0.20.0 2017-12-15
          """
* Package ``weblocks.debug`` now does not export ``*on`` variable,
  but provides three functions ``on``, ``off`` and ``status``.
* New method ``weblocks.server:serve-static-file`` was introduced.
  It can be used to create static routes which will respond with
  file's content. For example, you could add this to your app's
  ``initialize-instance`` method:

```common-lisp
(weblocks.server:serve-static-file
   "/favicon.png"
   (asdf:system-relative-pathname :app "favicon.png"))
```
"""
          )
  (0.19.2 2017-11-29
          """
* Now weblocks rebinds ``*random-state*`` to itself for each request to
  allow it to use ``setf`` and change ``*random-state*`` until the end
  of request processing.

"""
          )
  (0.19.1 2017-11-23
          """
* Dirty widgets rendering was fixed.
"""
          )
  (0.19.0 2017-11-13
          """
* Variable ``*expired-action-handler*``, method
  ``expired-action-handler`` and function
  ``default-expired-action-handler`` were replaced with method
  ``weblocks.actions:on-missing-action``.
* Now we are trying to call action only if action's name was given.
* Old method ``weblocks:handle-client-request ((app weblocks-webapp))``
  was removed. Look at it's newer version in ``weblocks.request-handler``.

"""
          )
  (0.18.0 2017-11-12
          """
* Commented out call to ``update-widget-tree`` inside of ``(setf
  widget-children)``, because it breaks on
  ``(get-widgets-by-type 'selector :root obj)`` sometimes. Seems this is
  because I've removed selector's code previously.

  **Warning!** Probably parent/children handling code will be removed soon.

* Backtrace printing code was replaced with direct usage of
  ``trivial-backtrace:print-backtrace``.

* Call to ``prepare-hooks`` was moved from ``weblocks.request-handler:handle-client-request``
  to the the weblocks.server:handler-request, to fix session hooks processing when
  ``:process-request`` hook is called.

"""
          )
  (0.17.2 2017-11-11
          """
* Error handling code was fixed. It was broken in 0.17.1 and prevented
  system loading.

"""
          )
  (0.17.1 2017-11-11
          """
* Fixed error handling when debug mode is "off". Now weblocks returns
  result of ``(weblocks.error-handler:on-error app)`` call.

"""
          )
  (0.17.0 2017-11-11
          """
* Added a ``weblocks.actions`` package.
* Also, a ``commands`` were introduced. Commands describe remote calls
  which have to be applied on a client as a result of action's
  execution. Previously, weblocks used similar technic to replace dirty
  widgets and to execute some javascript code before or after
  replacement. The new mechanism of "commands" is more generic and uses
  the JSON-RPC to pass function name and parameters from backend to
  client-side.
* Added ``weblocks.session:in-session-p`` function which returns ``t``
  if session data can be retrived or stored without error.
* Now function ``initiateActionWithArgsAndCallback`` send arguments as
  JSON payload. This allows to send any datastructures as action's params.
* Function ``weblocks.response:send-script`` was changed to use new
  mechanizm with commands. When you send script from the action, it will
  add a command ``:execute-code`` to the stack. All commands are
  executed in same order as they were added. If you want some code to be
  executed before widget update, just execute ``send-code`` before
  ``weblocks.widget:update``.
"""
          )
  (0.16.0 2017-11-04
          """
* New package was introduced - ``weblocks.widget`` it contains a new
  ``widget`` class with simplified rendering based on ``spinneret``
  templating library.
* Now class ``route`` is exported from ``weblocks.routes`` and should be
  used instead of ``routes:route``.
* New package ``weblocks.error-handler`` was introduced. It contains a
  ``on-error`` generic method which is called when some unhandled error
  raise by application.
* Fixed issue of adding multuple routes mapped to the same path. Now if
  url mapper already have a route all subsequent attempts to add a route
  with same path are ignored.
* Fixed error:

  ```
  Undefined function WEBLOCKS:WEBAPP-SESSION-KEY called with arguments
  (#<APP::APP #x3020052F01DD>)
  ```

* Fixed ``Content-Type`` of the default 500 page. Previously it was
  ``plain/text`` and browser didn't undestand that and downloaded the
  file.

  Now ``Content-Type`` is ``text/plain``.

"""
          )
  (0.15.0 2017-11-03
          """
* Now weblocks does not checks if all tokens from URL were consumed by
  app during root widget rendering. Previously it returned 404 if some
  token weren't consumed. Implement this logic in your app if needed.
* Macro `assert-hooks-called` was changed to return hooks in the order
  they were called. Also, now it waits hooks description as a DSL,
  like:

  ```lisp
  (assert-hooks-called
    (:fact-created contact "vasya@pupkin.com")
    (:fact-removed contact "vasya@pupkin.com"))
  ```

* New system `weblocks-testutils` was introduced. It
  compiles `weblocks.t.utils` package which macroses useful for
  unittesting.

  Also, a new macro `catch-hooks` was added to check if some
  hooks were called during a unittest.
* Now weblocks does not open a new tab or window on 500 error
  during an action execution.

"""
          )
  (0.14.4 2017-10-07
          """
* No more ``declaim optimize`` in different places. These
  declarations changed compiler's settings at unexpected moments.
* Fixed error happened when "File not found", and now
  ``with-hook`` macro returns the value of the last form's evaluation.

"""
          )
  (0.14.3 2017-09-23
          """
* Default method of ``render-page`` was fixed to really wrap
  page with ``<html>...`` block.

* Fixed a way how weblocks.debug:*latest-session* is
  processed.

* Function ``weblocks.request:remove-request-header`` now
  returns a new instance of request object and does not modify the
  original request. This fixes issue in ``weblocks-websocket``.

"""
          )
  (0.14.2 2017-09-22
          """
* Added package ``weblocks.debug`` and keeping latest
  session was rewritten using ``:process-request`` hook.

"""
          )
  (0.14.1 2017-09-22
          """
* Added function ``weblocks.request:remove-request-header``.
* Added a hook ``(:reset-session session)``, which is
  called around a code for clearing given session. Right now it is
  called only from ``weblocks.sessions:reset-latest-session``.

"""
          )
  (0.14.0 2017-09-20
          """
* ``html``, ``menu``, ``suggest`` and ``repl`` utilities
  were excluded.
* Code which was in ``request-handler.lisp``, was excluded
  from build and partly moved to ``request-handler2.lisp``.
* Added ``:stop-weblocks`` hook.
* Misc helper for repl were removed: ``sessions``,
  ``in-session`` and ``pt``. May be the will be restored in separate
  package.
* Page boilerplate rendering method ``render-page`` now
  does not use complex templating with contextes.
* Symbols refactoring:
  * ``*style-warn-on-circular-dirtying*`` variable ->
    ``weblocks.variables``;
  * ``*style-warn-on-late-propagation*`` variable ->
    ``weblocks.variables``;
  * ``gen-id`` function -> ``weblocks.session``;
  * ``send-script`` function -> ``weblocks.response``;
  * ``with-html-form`` macro -> ``weblocks-ui``;
  * ``*approved-return-codes*`` variable -> ``weblocks.variables``;
  * ``handle-ajax-request`` method -> ``weblocks.request-handler``;
  * ``update-location-hash-dependents`` function ->
    ``weblocks.request-handler``.
  * ``render-link`` function was moved to ``weblocks-ui.form`` in
    separate system.

"""
          )
  (0.13.11 2017-09-12
           """
* Added ``weblocks.hooks:call-hook`` helper.
* Now ``call-next-hook`` is called automatically if it
  wasn't called explicitly.

"""
           )
  (0.13.10 2017-09-06
           """
Changes in weblocks.request-hooks:
----------------------------------

* Package ``weblocks.request-hooks`` was renamed to ``weblocks.hooks``.
* Macro ``with-dynamic-hooks`` was renamed to ``with-hook``.
* Functions add-application-hook, add-session-hook, add-request-hook
  became a macroses and their argument lists were changed. Now the
  should be used like:

  .. code-block:: lisp

     (weblocks.hooks:add-session-hook
              :some-hook
              my-beautiful-callback (param)
            (do-something-useful-with param))

  ``weblocks.request-hooks:eval-hooks`` was renamed to
  ``weblocks.hooks:call`` and now can be called with params:

  .. code-block:: lisp

     (weblocks.hooks:call :some-hook
           first-param
           second-param)

"""
           )
  (0.13.10 2017-09-06
           """
* Added ``:handle-request`` dynamic hook called around request handling code.

  Called when ``weblocks.request:*request*`` and ``weblocks.session:*session*`` are already bound.

"""
           )
  (0.13.9 2017-09-02
          """
* Added integrity field for remove javascript dependencies.
  Also, ``get-cross-origin`` and ``:cross-origin`` were removed to
  ``get-crossorigin`` and ``:crossorigin``, to conform the html
  attibute's spelling.

"""
          )
  (0.13.8 2017-09-02
          """
* Fixed error on ``(weblocks:redirect...)`` call.
* Fixed dependency handling in ajax requests.
* Now if unhandled exception occure, Woo's handler remains
  working. Previously, handler's thread shut down on any unhandled exception.
* Ajax progress now does not inserted into the document,
  but if element with id ``ajax-progress`` is present, it will be shown
  and hidden by jQuery's ``.show`` and ``.hide`` methods. Also, they
  take optional speed parameters from ``data-*`` attributes
  ``data-show-speed`` and ``data-hide-speed``.

* Reformatted documentation. Started to [keep a changelog](http://keepachangelog.com/).

"""
          )
  (0.13.7 2017-04-15
          """
* Previous history wasn't tracked.
"""))
