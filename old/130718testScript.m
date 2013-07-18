#!/Applications/Mathematica.app/Contents/MacOS/MathematicaScript -script

Get[DirectoryName[$InputFileName] <> "eMUnit.m"];
Print/@ eMUnit`TestEMUnitPackage[][[1,2;;]];