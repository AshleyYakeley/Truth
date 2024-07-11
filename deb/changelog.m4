PACKAGENAME (PACKAGEVERSION) DEBIANREL; urgency=medium

  * Language
    - add implicit parameters
    - add documentation sections in declarations
    - add record functions
  * Library
    - rename libraries to remove "pinafore-"
    - built-in
      . add Blob type and functions
      . add asBlob.Entity
      . add utf8.Text
      . Showable subtypes: don't put spaces in for list and product types
      . add Result type and functions
      . add cell.Store, set.Store, fetch.Store
      . add StoppableTask type and functions
      . add subtype Store <: Model
    - media
      , add Media type and functions
      . add HTMLText type and functions
      . add CommonMarkText type and functions
    - gnome
      . rename textArea.Widget to textView.Widget, wrap text
      . add WebView.WebKit GTK element
    - UILib
      . update
  * VS Code Extension
    - add extension and file icons
  * Install
    - rename executable pinafore-docgen to pinadoc
    - include pinadoc in Debian and Nix packages
    - clean up Nix flake outputs
  * Fixes
    - fix defect with expressions with free variables in expose declarations
    - fix library names that were keywords
    - make PNGImage & JPEGImage types storable as they should be

 -- Ashley Yakeley <ashley@semantic.org>  RELEASEDATE

pinafore (0.5) bookworm; urgency=medium

  * Language
    - redesign dynamic types
    - allow decarations in do-blocks
  * Library
    - built-in
      . add collect.FiniteSetModel
      . add newClockUTC.Date, newClockLocal.Date
      . improve type of getList.FiniteSetModel
      . add DynamicType and functions
    - GTK
      . allow checkbox menus
    - UILib
      . update

 -- Ashley Yakeley <ashley@semantic.org>  Sun, 14 Apr 2024 13:57:24 -0700

pinafore (0.4.1) bookworm; urgency=medium

  * Install
    - Nix flake outputs for VS Code extension
  * Language
    - improve recursive type simplification
    - clean up error messages
    - reserve "type" keyword
  * Library
    - add constructors for Time type
  * Fixes
    - fix type solver
    - reject "rec a, a" types
    - fix fromList.Map

 -- Ashley Yakeley <ashley@semantic.org>  Mon, 30 Oct 2023 14:29:57 -0700

pinafore (0.4) bookworm; urgency=medium

  * Install
    - Debian package works on:
      . Ubuntu 22.04 LTS "jammy"
      . Debian 12 "bookworm"
    - add Nix flake
  * Language
    - overhaul of declarations
      . separate namespaces from modules,
        with "namespace" declarations and "with" and "import" declarators
      . both non-recursive ("let") and recursive ("let rec") declarators
      . "expose" declarations
      . allow declaration documentation with #| and {#| #} comments
    - datatype declarations
      . "closedtype" now "datatype storable"
      . can now have parameters
      . can now have subtypes
      . record constructors/patterns for datatypes
      . allow recursive types in datatypes
    - can declare arbitrary subtype relations
    - import lists
    - syntax
      . allow defintion of new operators
      . changed recursive type syntax from "rec v. T" to "rec v, T"
      . type names (+:), (*:), List, Unit
      . tuple constructor/pattern (,,) etc.
      . type signatures now attach to bindings, not stand-alone
      . separate syntax for static ":" and dynamic ":?" pattern typing
      . new syntax for function expressions: fn, match, =>
      . new syntax for datatype definitions
      . generalised "{}" and "do" syntax to any namespace
    - reject rather than mutate uninvertible type signatures
    - allow polymorphic recursion with type signatures
  * Interactive
    - :doc to retrieve name documentation
  * Library
    - Std
      . Literal type now byte array rather than text
      . Literal types now have GDS Literal
      . add Showable type for showing, show replacing toText
      . add min/max/lesser/greater functions
      . add List1 type for non-empty lists, subtype of List
      . rename "Ref" types and functions to "Model"
      . add TextModel type & associated functions, use for uiTextArea
    - new Task module
      . add Task type & associated functions
    - new Stream module
      . add sinks & sources
    - new Env module
      . move invocation-type stuff here
      . add stdin, stdout, stderr
    - new Eval module
      . move evaluate here
    - new Colour module
      . add Colour & AlphaColour types, etc.
    - new GIO module
      . add GIO File type and functions
    - new Cairo module
      . add Cairo-based functions for creating drawings
    - new Image module
      . add Image, HasMetadata, PNGImage, JPEGImage types, etc.
    - GTK
      . rename (from "UI")
      . explicit control over context
      . menu bar is just ordinary element
      . element for Image
  * Storage
    - Anchors now 256 bit, hash using BLAKE3
    - Store literals as binary rather than as text
    - Embed smaller literals directly in the anchor
  * Fixes
    - fix defect in lexical scoping

 -- Ashley Yakeley <ashley@semantic.org>  Sun, 27 Aug 2023 12:59:37 -0700

pinafore (0.3.1) buster; urgency=medium

  * Fixes
    - fix serious defect in type unifier (really this time)

 -- Ashley Yakeley <ashley@semantic.org>  Sat, 29 May 2021 15:11:02 -0700

pinafore (0.3) buster; urgency=medium

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

 -- Ashley Yakeley <ashley@semantic.org>  Sat, 10 Apr 2021 18:08:24 -0700

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
