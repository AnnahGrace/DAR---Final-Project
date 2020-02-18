/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\tsss1201.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = tsss1201 ;

DATA library.&dataset ;
INFILE datafile LRECL=82;
INPUT
   CENTER $ 1-2             ID $ 3-9                 VISIT  10-11          
   AGE  12-27               AGEYRS  28-29            TSCRUND  30-31        
   TSCRHYP  32              TSCRANX  33-34           TSCRDEP  35-36        
   TSCRANG  37-38           TSCRPTS  39-40           TSCRDIS  41-42        
   TSCRDISO  43-44          TSCRDISF  45             TSCRSC  46-47         
   TSCRSCP  48-49           TSCRSCD  50-51           TSCTUND  52-53        
   TSCTHYP  54-56           TSCTANX  57-59           TSCTDEP  60-62        
   TSCTANG  63-64           TSCTPTS  65-66           TSCTDIS  67-68        
   TSCTDISO  69-70          TSCTDISF  71-72          TSCTSC  73-75         
   TSCTSCP  76-78           TSCTSCD  79-81           TSC_SEX  82           
 ;                       

LABEL
   CENTER = "FIELD CENTER"
   ID = "LONGSCAN SUBJECT ID"
   VISIT = "VISIT NUMBER"
   AGE = "Age in Months - Child"
   AGEYRS = "Age in Years  - Child"
   TSCRUND = "Underresponse Raw Score"
   TSCRHYP = "Hyperresponse Raw Score"
   TSCRANX = "Anxiety Raw Score"
   TSCRDEP = "Depression Raw Score"
   TSCRANG = "Anger Raw Score"
   TSCRPTS = "Post-traumatic Stress Raw Score"
   TSCRDIS = "Dissociation Raw Score"
   TSCRDISO = "Overt Dissociation Raw Score"
   TSCRDISF = "Fantasy Dissociation Raw Score"
   TSCRSC = "Sexual Concerns Raw Score"
   TSCRSCP = "Sexual Preoccupation Raw Score"
   TSCRSCD = "Sexual Distresx Raw Score"
   TSCTUND = "Underresponse T-Score"
   TSCTHYP = "Hyperresponse T-Score"
   TSCTANX = "Anxiety T-Score"
   TSCTDEP = "Depression T-Score"
   TSCTANG = "Anger T-Score"
   TSCTPTS = "Post-traumatic Stress T-Score"
   TSCTDIS = "Dissociation T-Score"
   TSCTDISO = "Overt Dissociation T-Score"
   TSCTDISF = "Fantasy Dissociation T-Score"
   TSCTSC = "Sexual Concerns T-Score"
   TSCTSCP = "Sexual Preoccupation T-Score"
   TSCTSCD = "Sexual Distress T-Score"
   TSC_SEX = "Subject's Gender" ;

LENGTH
   VISIT 3                  AGEYRS 3                 TSCRUND 3             
   TSCRHYP 3                TSCRANX 3                TSCRDEP 3             
   TSCRANG 3                TSCRPTS 3                TSCRDIS 3             
   TSCRDISO 3               TSCRDISF 3               TSCRSC 3              
   TSCRSCP 3                TSCRSCD 3                TSCTUND 3             
   TSCTHYP 4                TSCTANX 4                TSCTDEP 4             
   TSCTANG 3                TSCTPTS 3                TSCTDIS 3             
   TSCTDISO 3               TSCTDISF 3               TSCTSC 4              
   TSCTSCP 4                TSCTSCD 4                TSC_SEX 3 ;

       

RUN ;
