Scala-Parser
============

This repository contains the source code and documentation for a Scala based reference implementation

Compiling
=========

The project layout uses SBT as build system.
You can compile it calling "sbt compile" on the command line.

Running testcases
=================

You can call "sbt test" on the console to run the test cases.

Using eclipse for development
=============================

Firstly install ScalaIDE (Eclipse-Plugin) from scala-ide.org

Calling "sbt eclipse" on the command line creates an eclipse project in the current directory.
This project then can be imported into eclipse using the "File->Import" wizard.
Please don't copy the project files into workspace, but use the original directory where SBT created the eclipse project.

The .gitignore file ensures that the eclipse project files aren't committed to the GIT repository. So everyone has its own eclipse project and its own eclipse settings.
