/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\pans1201.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = pans1201 ;

DATA library.&dataset ;
INFILE datafile LRECL=31;
INPUT
   CENTER $ 1-2             ID $ 3-9                 VISIT  10-11          
   YPA001  12               YPA002  13               YPA003  14            
   YPA1A1  15               YPA2A1  16               YPA3A1  17            
   YPA1C  18                YPA2C  19                YPA3C  20             
   YPAY  21                 YPAM  22                 YPANAGY  23           
   YPANAGM  24              YPAWAGY  25              YPAWAGM  26           
   YAGNPAY  27              YAGNPAM  28              YPACRITY  29          
   YPACRITM  30             YPASYMP  31            ;                       

LABEL
   CENTER = "FIELD CENTER"
   ID = "LONGSCAN SUBJECT ID"
   VISIT = "VISIT NUMBER"
   YPA001 = "In last year: felt very afraid/strange"
   YPA002 = "In last year: felt like asthma attack"
   YPA003 = "In last yr: heart ever beat very fast"
   YPA1A1 = "Panic attack type 1"
   YPA2A1 = "Panic attack type 2"
   YPA3A1 = "Panic attack type 3"
   YPA1C = "Not due to substance or medical condition - type 1"
   YPA2C = "Not due to substance or medical condition - type 2"
   YPA3C = "Not due to substance or medical condition - type 3"
   YPAY = "Past year: panic disorder"
   YPAM = "Past month: panic disorder"
   YPANAGY = "Past year: panic without agoraphobia"
   YPANAGM = "Past month: panic without agoraphobia"
   YPAWAGY = "Past year: panic with agoraphobia"
   YPAWAGM = "Past month: panic with agoraphobia"
   YAGNPAY = "Past year: agoraphobia without panic"
   YAGNPAM = "Past month: agoraphobia without panic"
   YPACRITY = "Youth Panic Disorder Criteria Count - Last Year"
   YPACRITM = "Youth Panic Disorder Criteria Count - Last Month"
   YPASYMP = "Youth Panic Disorder Symptom Count" ;

LENGTH
   VISIT 3                  YPA001 3                 YPA002 3              
   YPA003 3                 YPA1A1 3                 YPA2A1 3              
   YPA3A1 3                 YPA1C 3                  YPA2C 3               
   YPA3C 3                  YPAY 3                   YPAM 3                
   YPANAGY 3                YPANAGM 3                YPAWAGY 3             
   YPAWAGM 3                YAGNPAY 3                YAGNPAM 3             
   YPACRITY 3               YPACRITM 3               YPASYMP 3 ;

       

RUN ;
