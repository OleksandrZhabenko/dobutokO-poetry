# Revision history for dobutokO-poetry

## 0.1.0.0 -- 2020-06-01

* First version. Released on an unsuspecting world.

## 0.2.0.0 -- 2020-06-01

* Second version. Fixed issue with the wrong convertion to the WAV sound file. Added mmsyn3 as an explicit dependency (it was implicit) and changed the 
number of the minimum mmsyn6ukr package version.

## 0.3.0.0 -- 2020-06-02

* Third version. Added new generalized functions to the DobutokO.Poetry module and extended with them the functionality in the Main module. 

## 0.3.1.0 -- 2020-06-02

* Third version revised A. Fixed issue with being printed also the second command line argument for the Main.main function.

## 0.3.2.0 -- 2020-06-02

* Third version revised B. Fixed issues with being not accurate printing.

## 0.4.0.0 -- 2020-06-03

* Fourth version. Changed the module structure to simplify the usage in research purposes. Added new generalized variants of the functions with different
norms and different String -> [Int] conversion functions. Some documentation improvements.

## 0.5.0.0 -- 2020-06-04

* Fifth version. Fixed issues with inappropriate message information in some functions. Added new norms and possibility to use non-negative lists and to
split them to make possible a technique of multiple metrics united in one function. Added generalized variant of the 'uniquenessPeriods' and some additional 
functions to work with String data in Ukrainian. Some minor code improvements.

## 0.6.0.0 -- 2020-07-25

* Sixth version. Added three new functions to the DobutokO.Poetry module: uniqInMaxPoeticalNLine, uniqNPoeticalNLine and uniqNPoetical2GNLine. They are 
similar to the respective functions without 'Line' ending in their name but prints their output on the same line. 

## 0.7.0.0 -- 2020-07-29

* Seventh version. Changed the module structure. Added new modules and rewritten the old ones. Fixed unexact or not complete documentation for some functions.

## 0.8.0.0 -- 2020-07-31

* Eigth version. Changed the module structure. Removed the module DobutokO.Poetry.PrependAppend and added the module DobutokO.Poetry.General with more extended 
possibilities. Addad also README.md file with some useful information. Some code and documentation improvements.

## 0.8.1.0 -- 2020-07-31

* Eigth version revised A. Fixed issue with being not compiled fo GHC versions prior to 8.4* series. 

## 0.9.0.0 -- 2020-08-01

* Ninth version. Fixed issues with empty lists in the norms for some variants in DobutokO.Poetry.Norms module. Some documentation improvements.

## 0.9.0.1 -- 2020-08-05

* Ninth version revised A. Some documentation improvements for README.md file.

## 0.10.0.0 -- 2020-08-14

* Tenth version. Added a new module DobutokO.Poetry.Norms.Extended, added a new possibility to split norms and to use more complex than just one norm evaluation 
procedure on the each stage of evaluation. Added some new norms to the corresponding modules. Some minor code improvements. 

## 0.11.0.0 -- 2020-08-15

* Eleventh version. Rearranged the library and somewhat changed the executable. Removed deprecated modules DobutokO.Poetry.Basic and DobutokO.Poetry 
(instead you can use a generalized variant DobutokO.Poetry.General). Added new modules DobutokO.Poetry.Data and DobutokO.Poetry.General.Debug. 
Added more functions to DobutokO.Poetry.Auxiliary module. Fixed issue with norms combining in the DobutokO.Poetry.Norms module and added the new function 
combineNorms with the needed functionality. Changed the norm7 function in the DobutokO.Poetry.Norms.Extended module. Some documentation improvements. 

## 0.12.0.0 -- 2020-08-16

* Twelfth version. Divided a package into four different packages: uniqueness-periods, uniqueness-periods-general, dobutokO-poetry-general and dobutokO-poetry. 
Removed mmsyn7s from the dependencies. Changed the dependencies. 

## 0.13.0.0 -- 2020-08-16

* Thirteenth version. Fixed issues with being not compiled. 

## 0.14.0.0 -- 2020-08-16

* Fourteenth version. Fixed issues with being not compiled. 

## 0.15.0.0 -- 2020-08-18

* Fifteenth version. Added new functions uniqueness2nG and uniquenessPeriodsN to the DobutokO.Poetry.UniquenessPeriodsG module. Some documentation improvements for the 
module. Changed the dependency boundaries for uniqueness-periods.

## 0.16.0.0 -- 2020-08-24

* Sixteenth version. Added new module DobutokO.Poetry.Ukranian.PrepareText that allows to prepare the text to preserve possibly the needed grammar. 
Added explicit dependencies (they were earler implicit) of mmsyn2 and mmsyn5. 

## 0.16.1.0 -- 2020-08-24

* Sixteenth version revised A. Fixed an issue with usage of the jottedConv function in the DobutokO.Poetry.Ukranian.PrepareText module. 

## 0.16.2.0 -- 2020-08-24

* Sixteenth version revised B. Fixed an issue with usage of the auxiliary1 function in the DobutokO.Poetry.Ukranian.PrepareText module. 

## 0.16.3.0 -- 2020-08-25

* Sixteenth version revised C. Fixed issues with the isConcatenated and isPreposition functions in the DobutokO.Poetry.Ukranian.PrepareText module. Some documentation 
improvements.

## 0.17.0.0 -- 2020-09-07

* Seventeenth version. Moved the module DobutokO.Poetry.Ukranian.PrepareText to another new package phonetic-languages-ukrainian as a module 
Languages.Phonetic.Ukrainian.PrepareText and its explicit dependencies mmsyn2 and mmsyn5 (they become, nevertheless, the implicit ones for the dobutokO-poetry
package because they are dependencies for mmsyn6ukr package). This allows to use the moved functionality with less dependencies by some other packages.
