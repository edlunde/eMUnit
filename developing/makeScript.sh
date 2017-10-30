#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

targetFile=$(dirname "${DIR}")/eMUnit.m

declarationsFile=${DIR}"/eMUnit declarations.m"
implementationsFile=${DIR}"/eMUnit implementations.m"
testsFile=${DIR}"/eMUnit tests.m"


echo 'BeginPackage["eMUnit`"];' > "${targetFile}"

cat "${declarationsFile}" >> "${targetFile}"

echo 'Begin["`Private`"];' >> "${targetFile}"
cat "${implementationsFile}" >> "${targetFile}"
echo 'End[];' >> "${targetFile}"

echo 'Begin["`PackageTests`"];' >> "${targetFile}"
cat "${testsFile}" >> "${targetFile}"
echo 'End[];' >> "${targetFile}"


echo '(* ::Section::Closed:: *)' >> "${targetFile}"
echo '(*Tail*)' >> "${targetFile}"
echo 'EndPackage[]' >> "${targetFile}"