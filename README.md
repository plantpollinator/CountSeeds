# CountSeeds
R script for counting seeds as fertile or infertile. This is released under the Artistic License 2.0 (https://opensource.org/license/artistic-2-0), which is not listed by GitHub, rather than the listed MIT License.

To use, copy the script into the same directory as the seed jpeg images. Open R and type 
```source("CountSeeds_A_03.02.R")```
and follow the prompts. 

Note that while the script should run in Rstudio and bare R in Linux (tested in Debian 6.11.2-amd64) and Windows (tested in Windows 10), it DOES NOT work on Mac (it has debugging-resistant issues detecting the mouse buttons properly, alas).

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
