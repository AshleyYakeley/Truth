PACKAGENAME (PACKAGEVERSION) DEBIANREL; urgency=medium

  * Language
    - add subsumption expressions
    - allow module-qualified names
    - improve type simplification
  * Library
    - Std
      . add "index" function for lists
      . add "forWhole" function
      . change some constructor names not to clash with type name
      . new ListRef type, with functions
      . rename various reference-related functions
    - UI
      . separate module
      . rename "UI" type to "Element"
      . elements for CSS styling
      . openWindow now takes size
      . notebook can track page selection
  * Fixes
    - fix defect involving export of open expressions
    - fix serious defect in type unifier
    - fix defect involving imported types

 -- Ashley Yakeley <ashley@semantic.org>  RELEASEDATE

pinafore (0.2) buster; urgency=medium

  * Module system
  * Type system
    - Types now have "dynamic supertypes", with cast functions.
    - Generalised subtype relations
    - Dynamic entity types
  * Allow passing of command-line arguments to scripts
  * Command-line completion for bash
  * library improvements
  * bug fixes

 -- Ashley Yakeley <ashley@semantic.org>  Mon, 21 Dec 2020 10:52:24 -0800

pinafore (0.1) buster; urgency=medium

  * Initial release

 -- Ashley Yakeley <ashley@semantic.org>  Mon, 21 Sep 2020 12:00:00 -0800
