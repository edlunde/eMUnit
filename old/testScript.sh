dir=$(pwd)
load="Get[\"$dir/eMUnit.m\"];"
test="Print/@eMUnit\`TestEMUnitPackage[][[1,2;;]];"
quit="Quit[];"

/Applications/Mathematica.app/Contents/MacOS/MathKernel -run \
$load\
$test\
$quit