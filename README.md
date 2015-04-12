Allehanda
=========

Miscellaneous algorithms and data structures.

Test
====

    sbt test

Build executable
================

    sbt stage

which will create target/universal/stage/{bin,lib} folders. An executable will
be put in bin, dependency jars in lib.


Build tarball
=============

    sbt universal:package-xz-tarball

Which will produce a tarball with a run script and all dependency jars, placed
in target/universal.
