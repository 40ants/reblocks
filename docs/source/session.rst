===================
 Using the Session
===================

The functions and macros to manipulate the session are defined in the
``weblocks/session`` package. Its exported symbols are the following:

* ``with-session``
* ``delete-value``
* ``get-value``
* ``gen-id``
* ``in-session-p``
* ``init``
* ``get-session-id``
* ``reset``
* ``expire``
* ``get-number-of-sessions``
* ``make-session-middleware``
* ``get-number-of-anonymous-sessions``


You can store any kind of structure into a session.

To set a value, use ``setf`` in conjonction with ``get-value``.

We saw in the Quickstart how to use the ``init`` method to start a
Weblocks application.
