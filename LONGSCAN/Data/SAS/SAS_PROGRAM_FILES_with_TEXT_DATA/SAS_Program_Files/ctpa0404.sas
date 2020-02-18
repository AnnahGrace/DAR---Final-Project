/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\ctpa0404.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = ctpa0404 ;

DATA library.&dataset ;
INFILE datafile LRECL=63;
INPUT
   id $ 1-7                 center $ 8-9             visit  10             
   ctpa0  11                ctpa1a  12               ctpa1b  13            
   ctpa2a  14               ctpa2b  15               ctpa3a  16            
   ctpa3b  17               ctpa4a  18               ctpa4b  19            
   ctpa5a  20               ctpa5b  21               ctpa6a  22            
   ctpa6b  23               ctpa7a  24               ctpa7b  25            
   ctpa8a  26               ctpa8b  27               ctpa9a  28            
   ctpa9b  29               ctpa10a  30              ctpa10b  31           
   ctpa11a  32              ctpa11b  33              ctpa12a  34           
   ctpa12b  35              ctpa13a  36              ctpa13b  37           
   ctpa14a  38              ctpa14b  39              ctpa15a  40           
   ctpa15b  41              ctpa16a  42              ctpa16b  43           
   ctpa17a  44              ctpa17b  45              ctpa18a  46           
   ctpa18b  47              ctpa19a  48              ctpa19b  49           
   ctpa20 $ 50-52           ctpa21 DATE11.         ;                       

LABEL
   id = "longscan subject id"
   center = "field center"
   visit = "visit number"
   ctpa0 = "respondent lives with spouse-partner"
   ctpa1a = "# times respon. discussed issue calmly"
   ctpa1b = "# times partner discussed issue calmly"
   ctpa2a = "# of times respon. got info. to back up"
   ctpa2b = "# of times partner got info to back up"
   ctpa3a = "# times respon. brought in other person"
   ctpa3b = "# times partner brought in other person"
   ctpa4a = "# of times respon. insulted-swore"
   ctpa4b = "# of times partner insulted-swore"
   ctpa5a = "# times respon. sulked-refused to talk"
   ctpa5b = "# times partner sulked-refused to talk"
   ctpa6a = "# of times respon. stomped away"
   ctpa6b = "# of times partner stomped away"
   ctpa7a = "# of times respon. cried"
   ctpa7b = "# of times partner cried"
   ctpa8a = "# times respon. do-say something w spite"
   ctpa8b = "# times partner do-say something w spite"
   ctpa9a = "# of times respon. threaten to hit-throw"
   ctpa9b = "# of times partner threaten to hit-throw"
   ctpa10a = "# of times respon. throw-hit something"
   ctpa10b = "# of times partner throw-hit something"
   ctpa11a = "# times respon. throw something at part."
   ctpa11b = "# times partner throw something at resp."
   ctpa12a = "# of times respon. push-shove-grab part."
   ctpa12b = "# of times partner push-shove-grab resp."
   ctpa13a = "# of times respon. slap partner"
   ctpa13b = "# of times partner slap resp."
   ctpa14a = "# times respon. kick-bit-hit with fist"
   ctpa14b = "# times partner kick-bit-hit with fist"
   ctpa15a = "# times respon. hit part. with something"
   ctpa15b = "# times partner hit resp. with something"
   ctpa16a = "# of times respon. beat partner up"
   ctpa16b = "# of times partner beat resp. up"
   ctpa17a = "# of times respon. choke partner"
   ctpa17b = "# of times partner choke resp."
   ctpa18a = "# times respon. threaten with knife-gun"
   ctpa18b = "# times partner threaten with knife-gun"
   ctpa19a = "# of times respon. used knife-gun"
   ctpa19b = "# of times partner used knife-gun"
   ctpa20 = "interviewer initials"
   ctpa21 = "date of interview" ;

FORMAT
   ctpa21 DATE9. ;
     

LENGTH
   visit 3                  ctpa0 3                  ctpa1a 3              
   ctpa1b 3                 ctpa2a 3                 ctpa2b 3              
   ctpa3a 3                 ctpa3b 3                 ctpa4a 3              
   ctpa4b 3                 ctpa5a 3                 ctpa5b 3              
   ctpa6a 3                 ctpa6b 3                 ctpa7a 3              
   ctpa7b 3                 ctpa8a 3                 ctpa8b 3              
   ctpa9a 3                 ctpa9b 3                 ctpa10a 3             
   ctpa10b 3                ctpa11a 3                ctpa11b 3             
   ctpa12a 3                ctpa12b 3                ctpa13a 3             
   ctpa13b 3                ctpa14a 3                ctpa14b 3             
   ctpa15a 3                ctpa15b 3                ctpa16a 3             
   ctpa16b 3                ctpa17a 3                ctpa17b 3             
   ctpa18a 3                ctpa18b 3                ctpa19a 3             
   ctpa19b 3 ;

       

RUN ;
