/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\date1201.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = date1201 ;

DATA library.&dataset ;
INFILE datafile LRECL=163;
INPUT
   CENTER $ 1-2             ID $ 3-9                 C_DATE4 DATE11.       
   C_DATE6 DATE11.          C_DATE8 DATE11.          C_DATE12 DATE11.      
   C_DATE14 DATE11.         C_DATE16 DATE11.         C_DATE18 DATE11.      
   P_DATE4 DATE11.          P_DATE6 DATE11.          P_DATE8 DATE11.       
   P_DATE12 DATE11.         P_DATE14 DATE11.         P_DATE16 DATE11.      
   P_DATE18 DATE11.       ;                       

LABEL
   CENTER = "FIELD CENTER"
   ID = "LONGSCAN SUBJECT ID"
   C_DATE4 = "Child Interview Date (age 4)"
   C_DATE6 = "Child Interview Date (age 6)"
   C_DATE8 = "Child Interview Date (age 8)"
   C_DATE12 = "Child Interview Date (age 12)"
   C_DATE14 = "Child Interview Date (age 14)"
   C_DATE16 = "Child Interview Date (age 16)"
   C_DATE18 = "Child Interview Date (age 18)"
   P_DATE4 = "Caregiver or Parent Interview  Date (age 4)"
   P_DATE6 = "Caregiver or Parent Interview  Date (age 6)"
   P_DATE8 = "Caregiver or Parent Interview  Date (age 8)"
   P_DATE12 = "Caregiver or Parent Interview  Date (age 12)"
   P_DATE14 = "Caregiver or Parent Interview  Date (age 14)"
   P_DATE16 = "Caregiver or Parent Interview  Date (age 16)"
   P_DATE18 = "Caregiver or Parent Interview  Date (age 18)" ;

FORMAT
   C_DATE4 DATE9.           C_DATE6 DATE9.           C_DATE8 DATE9.        
   C_DATE12 DATE9.          C_DATE14 DATE9.          C_DATE16 DATE9.       
   C_DATE18 DATE9.          P_DATE4 DATE9.           P_DATE6 DATE9.        
   P_DATE8 DATE9.           P_DATE12 DATE9.          P_DATE14 DATE9.       
   P_DATE16 DATE9.          P_DATE18 DATE9. ;
   

RUN ;
