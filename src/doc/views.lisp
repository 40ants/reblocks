(uiop:define-package #:reblocks/doc/views
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection))
(in-package #:reblocks/doc/views)


(defsection @views (:title "Views"
                    :ignore-words ("HTML"))
  "
Views provided convenient ways to define how data object are to be rendered.

Originally, Weblocks supported three types of views:

- data views;
- table views;
- form views.

Form views were especially useful because they let you build forms
in a declarative manner, error checking and reporting included. 

Views mainly consist of a rendering framework unique to the
view and a series of view fields. The table view for example knows
how to render a HTML table.

View fields usually map to the slots of your data class but they were
flexible enough to be used in any other way. Associated with each view field
regardless of the view type are things like a human-readable label and
information on how to render the field (presentation).

Form views included additional parameters like how to translate user
input to a proper model value (parser) and constraints on the data
(satisfies).")
