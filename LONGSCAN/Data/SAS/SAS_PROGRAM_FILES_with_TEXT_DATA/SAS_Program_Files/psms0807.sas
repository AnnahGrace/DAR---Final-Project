/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\psms0807.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = psms0807 ;

DATA library.&dataset ;
INFILE datafile LRECL=25;
INPUT
   ID $ 1-7                 CENTER $ 8-9             VISIT  10-11          
   MISS_PSY  12-13          PSMATOTE  14-15          PSMATOTB  16-17       
   PSMATOTS  18-19          PSMATOTL  20-21          PSMA_IE  22           
   PSMA_IB  23              PSMA_IS  24              PSMA_IL  25           
 ;                       

LABEL
   ID = "LONGSCAN SUBJECT ID"
   CENTER = "FIELD CENTER"
   VISIT = "VISIT NUMBER"
   MISS_PSY = "# of Missing Psych Abuse Stem Items"
   PSMATOTE = "Psychological Abuse Score (ever)"
   PSMATOTB = "Psychological Abuse Score (< elem)"
   PSMATOTS = "Psychological Abuse Score (> elem)"
   PSMATOTL = "Psychological Abuse Score (last year)"
   PSMA_IE = "Indicator of Psych Abuse (ever)"
   PSMA_IB = "Indicator of Psych Abuse (< elem)"
   PSMA_IS = "Indicator of Psych Abuse (> elem)"
   PSMA_IL = "Indicator of Psych Abuse (last yr)" ;

LENGTH
   VISIT 3                  MISS_PSY 3               PSMATOTE 3            
   PSMATOTB 3               PSMATOTS 3               PSMATOTL 3            
   PSMA_IE 3                PSMA_IB 3                PSMA_IS 3             
   PSMA_IL 3 ;

       

RUN ;
