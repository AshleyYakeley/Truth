This repository contains Truth and Pinafore.

## Truth
Truth is a library of APIs for reading and changing infomation.
An API is defined by an *edit* type, for changes, and an associated *reader* type, for reads.
*Edit lenses* build APIs from other APIs.
*User interface specifiers* define user interface elements and layout, which are then constructed with the GTK3 framework.

## Pinafore
Pinafore is a triple-store database for storing knowledge, and a language ([example](test/test.pui)) for specifying ontologies and user interfaces.
It is built on Truth.
It is currently focused on personal information management, such as contact, calendar, to-do, photo collection, etc.

## Running
```shell
sudo apt-get install -y haskell-stack gnome-platform-devel libgirepository1.0-dev libwebkit2gtk-4.0-dev
stack setup
# stack install alex happy
# stack install gtk2hs-buildtools
stack build pinafore
./testpinafore
```
