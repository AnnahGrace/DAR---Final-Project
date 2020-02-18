/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\pcpa1002.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = pcpa1002 ;

DATA library.&dataset ;
INFILE datafile LRECL=45;
INPUT
   CENTER $ 1-2             ID $ 3-9                 PCPA23 $ 10-12        
   VISIT  13-14             PCPA3  15                PCPA4  16             
   PCPA5  17                PCPA6  18                PCPA7  19             
   PCPA8  20                PCPA9  21                PCPA10  22            
   PCPA11  23               PCPA12  24               PCPA13  25            
   PCPA14  26               PCPA15  27               PCPA16  28            
   PCPA17  29               PCPA18  30               PCPA19  31            
   PCPA20  32               PCPA21  33               PCPA22  34            
   PCPA24 DATE11.         ;                       

LABEL
   CENTER = "FIELD CENTER"
   ID = "LONGSCAN SUBJECT ID"
   PCPA23 = "Interviewer initials"
   VISIT = "VISIT NUMBER"
   PCPA3 = "How close do you feel to child"
   PCPA4 = "How much do you care about child"
   PCPA5 = "How often child interferes w/activities"
   PCPA6 = "How often do you trust child"
   PCPA7 = "How often feel you understand child"
   PCPA8 = "How often do you and s/he get along well"
   PCPA9 = "How often do you make decisions together"
   PCPA10 = "Feel you interfere w/childs activities"
   PCPA11 = "Pst 4wks:gone shopping with him/her"
   PCPA12 = "Pst 4wks:played a sport with him/her"
   PCPA13 = "Pst 4wks:gone to religious event w/"
   PCPA14 = "Pst 4wks:talked about his/her friends"
   PCPA15 = "Pst 4wks:gone to movie/event w/"
   PCPA16 = "Pst 4wks:talked about personal problem"
   PCPA17 = "Pst 4wks:had a serious argument"
   PCPA18 = "Pst 4wks:talked ab. his/her school work"
   PCPA19 = "Pst 4wks:worked on a project"
   PCPA20 = "Talked things s/he is doing in school"
   PCPA21 = "How disappoin./child didnt grad college"
   PCPA22 = "How disappoin./child didnt grad hi schl"
   PCPA24 = "Date of Interview" ;

FORMAT
   PCPA24 DATE9. ;
     

LENGTH
   VISIT 3                  PCPA3 3                  PCPA4 3               
   PCPA5 3                  PCPA6 3                  PCPA7 3               
   PCPA8 3                  PCPA9 3                  PCPA10 3              
   PCPA11 3                 PCPA12 3                 PCPA13 3              
   PCPA14 3                 PCPA15 3                 PCPA16 3              
   PCPA17 3                 PCPA18 3                 PCPA19 3              
   PCPA20 3                 PCPA21 3                 PCPA22 3 ;

        

RUN ;
