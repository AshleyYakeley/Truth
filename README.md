This repository contains Truth and Pinafore.

## Truth
Truth is a library of APIs for reading and changing infomation.
An API is defined by an *edit* type, for changes, and an associated *reader* type, for reads.
*Edit lenses* build APIs from other APIs.
*User interface specifiers* define user interface elements and layout, which are then constructed with the GTK3 framework.

## Pinafore
Pinafore is a triple-store database for storing knowledge, and a language ([example](test.pui)) for specifying ontologies and user interfaces.
It is built on Truth.
It is currently focused on personal information management, such as contact, calendar, to-do, photo collection, etc.

## Running
```shell
sudo apt-get install -y gnome-platform-devel
stack setup
stack install alex happy
stack install gtk2hs-buildtools
stack build pinafore
./testpinafore
```
(Note: the New button currently creates blank entries: you can still double-click on them.)
