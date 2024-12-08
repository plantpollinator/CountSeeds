# CountSeeds
R script written for counting orchid seeds as fertile or infertile from microscopy images, but could be used for other similar purposes (e.g. counting birds in a field from images). This is released under the Artistic License 2.0 (https://opensource.org/license/artistic-2-0), please see the LICENSE for details.

To use, copy the script into the same directory as the JPEG images you wish to score and ensure that the ```jpeg``` library is installed in your R library. We recommend that JPEG images are saved with filenames of the following format: ```SAMPLENAME-SAMPLENUMBER.jpg```, e.g. ```DAV22W-01.jpg```. Open R and type 
```source("CountSeeds_A_03.02.R")```
and begin scoring images. A left mouse click will mark a fertile seed with a blue cross, a right mouse click an infertile seed with a red x. Keyboard bindings are below (e.g. ```x``` or ```Delete``` will delete the last point clicked). Once done scoring an image, hit ```Enter``` to move to the next image and continue scoring. 

Note that while the script should run in Rstudio and bare console R in Linux (last tested in Debian 6.11.2-amd64) and Windows (last tested in Windows 10), it DOES NOT work on Mac (it has debugging-resistant issues detecting the mouse buttons properly, alas).

For each sample (each individual seed was collected from), the software aims to have the user score 100 seeds minimum for fertility before moving on to the next sample; it will automatically move on at 150 seeds scored. These values can be modified within the script. It is strongly recommended that you write the summary report using the ```w``` keybinding at the end of each session, and end a session at the end of a sample so as to not lose track of where you were in scoring images. The summary reports (TSV files) contain the sample name, proportion of good seeds, number of seeds scored, and number of images scored, and are named uniquely with a time and date stamp to avoid overwriting of data.

Keyboard bindings to use while the script is running:

|Binding|What it does                                   |
|-------|-----------------------------------------------|
|Enter  |image complete/done, go to next image          |
|r      |Toggle Random/List mode                        |
|i      |show image info                                |
|p      |show progress                                  |
|t      |show image info in title                       |
|s      |save to next file                              |
|l      |load from/revert to last file                  |
|v      |verify load/save functions are working properly|
|d      |turn debugging functions on/off                |
|w      |write a summary report                         |
|q/Esc  |quit recording                                 |
|c      |clear all data points for current image        |
|h      |help                                           |
|x/Del  |delete last point recorded in image            |

Have fun!
