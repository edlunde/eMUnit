#!/bin/bash
# MathematicaScript and MathKernel 9 have a bug that prevents output sent
# from a Mathematica script to stdout from being catched on a pipe or
# redirected to a file. This workaround runs the script inline into a 
# MathKernel session.
/Applications/Mathematica.app/Contents/MacOS/MathKernel -noprompt -run "commandLine={${1+\"$1\"}}; $(sed '1,/^exit/d' $0) ; Exit[]"
exit $?

Get[DirectoryName[$InputFileName] <> "eMUnit.m"];
Print/@ eMUnit`TestEMUnitPackage[][[1,2;;]];