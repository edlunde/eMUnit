#!/bin/bash

scriptDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# We place the combined file one folder level up
targetFile=$(dirname "${scriptDir}")/eMUnit.m

# The source files
declarationsFile=${scriptDir}"/eMUnit declarations.m"
implementationsFile=${scriptDir}"/eMUnit implementations.m"
testsFile=${scriptDir}"/eMUnit tests.m"


# Glue everything together

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
 

# Cleaning up by closing all subsections and below
# Escaping * as it has special meaning in regexps
sed -i '' -e 's/(\* ::Subsection:: \*)/(\* ::Subsection::Closed:: \*)/g' "${targetFile}"
sed -i '' -e 's/(\* ::Subsubsection:: \*)/(\* ::Subsubsection::Closed:: \*)/g' "${targetFile}"
