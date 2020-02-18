/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\rbfa1201.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = rbfa1201 ;

DATA library.&dataset ;
INFILE datafile LRECL=39;
INPUT
   CENTER $ 1-2             ID $ 3-9                 VISIT  10-11          
   RBFA1  12                RBFA2  13                RBFA3  14             
   RBFA4  15                RBFA5  16                RBFA6  17             
   RBFA7  18                RBFA8  19                RBFA9  20             
   RBFA9A  21               RBFA10  22               RBFA11  23            
   RBFA12  24               RBFA13  25               RBFA14  26            
   RBFA15  27               RBFA16  28               RBFA17  29            
   RBFA18  30               RBFA19  31               RBFA20  32            
   RBFA21  33               RBFA22  34               RBFA23  35            
   RBFA24  36               RBFA25  37               RBFA26  38            
   RBFA27  39             ;                       

LABEL
   CENTER = "FIELD CENTER"
   ID = "LONGSCAN SUBJECT ID"
   VISIT = "VISIT NUMBER"
   RBFA1 = "Anyone you live with smoke cigarettes"
   RBFA2 = "Anyone you live with chew tobacco/snuff"
   RBFA3 = "Anyone you live with drink alcohol"
   RBFA4 = "Anyone you live with smoke marijuana"
   RBFA5 = "Anyone you live with use cocaine/crack"
   RBFA6 = "Anyone you live with use meth/speed/etc."
   RBFA7 = "Anyone you live with inject drugs"
   RBFA8 = "Anyone you live with use other drugs"
   RBFA9 = "Anyone you live with get drunk/high"
   RBFA9A = "How often someone in house drunk or high"
   RBFA10 = "# of friends get good grades"
   RBFA11 = "# of friends behave well in school"
   RBFA12 = "# of friends attend church"
   RBFA13 = "# of friends participate in school clubs"
   RBFA14 = "# of friends participate in sports"
   RBFA15 = "# of friends smoke cigarettes"
   RBFA16 = "# of friends drink alcohol"
   RBFA17 = "# of friends have had sexual intercourse"
   RBFA18 = "# of friends carry guns or other weapons"
   RBFA19 = "# of friends smoke marijuana"
   RBFA20 = "# of friends use cocaine or crack"
   RBFA21 = "# of friends use heroin"
   RBFA22 = "# of friends use other drugs"
   RBFA23 = "# of friends sell or deliver drugs"
   RBFA24 = "# of friends shoplift or steal"
   RBFA25 = "# of friends set fires"
   RBFA26 = "# of friends get into fights"
   RBFA27 = "# of friends damage or destroy things" ;

LENGTH
   VISIT 3                  RBFA1 3                  RBFA2 3               
   RBFA3 3                  RBFA4 3                  RBFA5 3               
   RBFA6 3                  RBFA7 3                  RBFA8 3               
   RBFA9 3                  RBFA9A 3                 RBFA10 3              
   RBFA11 3                 RBFA12 3                 RBFA13 3              
   RBFA14 3                 RBFA15 3                 RBFA16 3              
   RBFA17 3                 RBFA18 3                 RBFA19 3              
   RBFA20 3                 RBFA21 3                 RBFA22 3              
   RBFA23 3                 RBFA24 3                 RBFA25 3              
   RBFA26 3                 RBFA27 3 ;

        

RUN ;
