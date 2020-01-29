==========================
 Contributing to Weblocks
==========================

0. Discuss a problem in the chat https://gitter.im/40ants/weblocks
1. Checkout branch.
2. Make changes.
3. Add tests into the ``t/`` subdirectory.
4. Test changes::

     rove weblocks.asd

   or::

     (asdf:test-system :weblocks)

5. Ensure you've updated documentation in the ``docs`` folder if you've
   changed or added functionality.
6. Describe your changes for in a readable way ontop of the
   ``ChangeLog.rst``. Increment a version number according to `semver`_.
   Also, update a ``version.lisp-expr`` file.
7. Make a pull-request and make the world a better place!


.. _semver: https://semver.org/
