#!/Applications/Mathematica.app/Contents/MacOS/MathematicaScript -script

Run["'" <> DirectoryName[$InputFileName] <> "'" <> "makeScript.sh"]

Get[ParentDirectory@DirectoryName[$InputFileName] <> "/eMUnit.m"];

Print /@ TestEMUnitPackage[][[1,2;;]];
