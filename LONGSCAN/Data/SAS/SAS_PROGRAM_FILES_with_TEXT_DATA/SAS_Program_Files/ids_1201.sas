/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\ids_1201.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = ids_1201 ;

DATA library.&dataset ;
INFILE datafile LRECL=80;
INPUT
   CENTER $ 1-2             ID $ 3-9                 SUBJDOB DATE11.       
   SUBJGEND  21             STATINIT $ 22-29         LSRACE  30            
   YDISC_14  31             YDISC_18  32             PDISC_14  33          
   AGE_4  34-37             AGE_6  38-41             AGE_8  42-46          
   AGE_12  47-51            AGE_14  52-56            AGE_16  57-61         
   AGE_18  62-66            CH_AGE4  67              CG_AGE4  68           
   CH_AGE6  69              CG_AGE6  70              CH_AGE8  71           
   CG_AGE8  72              CH_AGE12  73             CG_AGE12  74          
   CH_AGE14  75             CG_AGE14  76             CH_AGE16  77          
   CG_AGE16  78             CH_AGE18  79             CG_AGE18  80          
 ;                       

LABEL
   CENTER = "FIELD CENTER"
   ID = "LONGSCAN SUBJECT ID"
   SUBJDOB = "Master Date of Birth"
   SUBJGEND = "Master Gender"
   STATINIT = "Status of Subject at Initial Interview"
   LSRACE = "Child's race as reported at baseline"
   YDISC_14 = "Youth DISC Observation at T14"
   YDISC_18 = "Youth DISC Observation at T18"
   PDISC_14 = "Caregiver DISC Observation at T14"
   AGE_4 = "child age at T4 interview"
   AGE_6 = "child age at T6 interview"
   AGE_8 = "child age at T8 interview"
   AGE_12 = "child age at T12 interview"
   AGE_14 = "child age at T14 interview"
   AGE_16 = "child age at T16 interview"
   AGE_18 = "child age at T18 interview"
   CH_AGE4 = "Child interview indicator at age 4"
   CG_AGE4 = "Caregiver interview indicator at age 4"
   CH_AGE6 = "Child interview indicator at age 6"
   CG_AGE6 = "Caregiver interview indicator at age 6"
   CH_AGE8 = "Child interview indicator at age 8"
   CG_AGE8 = "Caregiver interview indicator at age 8"
   CH_AGE12 = "Child interview indicator at age 12"
   CG_AGE12 = "Caregiver interview indicator at age 12"
   CH_AGE14 = "Child interview indicator at age 14"
   CG_AGE14 = "Caregiver interview indicator at age 14"
   CH_AGE16 = "Child interview indicator at age 16"
   CG_AGE16 = "Caregiver interview indicator at age 16"
   CH_AGE18 = "Child interview indicator at age 18"
   CG_AGE18 = "Caregiver interview indicator at age 18" ;

FORMAT
   SUBJDOB DATE9.         ;
                     

LENGTH
   SUBJGEND 3               LSRACE 3                 YDISC_14 3            
   YDISC_18 3               PDISC_14 3               CH_AGE4 3             
   CG_AGE4 3                CH_AGE6 3                CG_AGE6 3             
   CH_AGE8 3                CG_AGE8 3                CH_AGE12 3            
   CG_AGE12 3               CH_AGE14 3               CG_AGE14 3            
   CH_AGE16 3               CG_AGE16 3               CH_AGE18 3            
   CG_AGE18 3 ;

      

RUN ;
