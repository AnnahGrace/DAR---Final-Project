/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\ysrs1201.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = ysrs1201 ;

DATA library.&dataset ;
INFILE datafile LRECL=118;
INPUT
   CENTER $ 1-2             GENDER $ 3               AGE $ 4-5             
   YSRWIT_R  6-7            YSRWIT_T  8-9            YSRSOM_R  10-11       
   YSRSOM_T  12-13          YSRANX_R  14-15          YSRANX_T  16-17       
   YSRSOC_R  18-19          YSRSOC_T  20-21          YSRTHT_R  22-23       
   YSRTHT_T  24-26          YSRATT_R  27-28          YSRATT_T  29-31       
   YSRDEL_R  32-33          YSRDEL_T  34-35          YSRAGG_R  36-37       
   YSRAGG_T  38-39          YSC_ACTR  40-42          YSC_ACTT  43-44       
   YSC_SOCR  45-48          YSC_SOCT  49-50          YSC_SCHR  51-53       
   YSC_TOTR  54-57          YSC_TOTT  58-59          ID $ 60-66            
   VISIT  67-68             YSRSD_R  69-70           YSRINT_R  71-72       
   YSREXT_R  73-74          YSRTOT_R  75-77          YSRSD_T  78-79        
   YSRINT_T  80-81          YSREXT_T  82-83          YSRTOT_T  84-85       
   AGEMNTHS  86-88          YREXT_BL  89             YREXT_CR  90          
   YRINT_BL  91             YRINT_CR  92             YRTOT_BL  93          
   YRTOT_CR  94             YRAGG_BL  95             YRAGG_CR  96          
   YRATT_BL  97             YRATT_CR  98             YRANX_BL  99          
   YRANX_CR  100            YRDEL_BL  101            YRDEL_CR  102         
   YRSOC_BL  103            YRSOC_CR  104            YRSOM_BL  105         
   YRSOM_CR  106            YRTHT_BL  107            YRTHT_CR  108         
   YRWIT_BL  109            YRWIT_CR  110            YRSD_BL  111          
   YRSD_CR  112             YCACT_BL  113            YCACT_CR  114         
   YCSOC_BL  115            YCSOC_CR  116            YCTOT_BL  117         
   YCTOT_CR  118          ;                       

LABEL
   CENTER = "FIELD CENTER"
   GENDER = "gender"
   AGE = "age"
   YSRWIT_R = "YSR withdrawn raw score"
   YSRWIT_T = "YSR withdrawal T score"
   YSRSOM_R = "YSR somatic raw score"
   YSRSOM_T = "YSR somatic T score"
   YSRANX_R = "YSR Anxious/depressed raw score"
   YSRANX_T = "YSR Anxious/depressed T score"
   YSRSOC_R = "YSR social problems raw score"
   YSRSOC_T = "YSR social problems T score"
   YSRTHT_R = "YSR thought problems raw score"
   YSRTHT_T = "YSR thought problems T score"
   YSRATT_R = "YSR attention problems raw score"
   YSRATT_T = "YSR attention problems T score"
   YSRDEL_R = "YSR delinquent raw score"
   YSRDEL_T = "YSR delinquent T score"
   YSRAGG_R = "YSR Aggression raw score"
   YSRAGG_T = "YSR Aggression T score"
   YSC_ACTR = "YSCA Activity Scale Raw Score"
   YSC_ACTT = "YSCA Activity Scale T-Score"
   YSC_SOCR = "YSCA Social Scale Raw Score"
   YSC_SOCT = "YSCA Social Scale T-Score"
   YSC_SCHR = "YSCA School Scale Raw Score"
   YSC_TOTR = "YSCA Total Competency Raw Score"
   YSC_TOTT = "YSCA Total Competency T-Score"
   ID = "LONGSCAN SUBJECT ID"
   VISIT = "VISIT NUMBER"
   YSRSD_R = "YSR Self-destructive/Identity raw score"
   YSRINT_R = "YSR internalizing raw score"
   YSREXT_R = "YSR externalizing raw score"
   YSRTOT_R = "YSR total problems raw score"
   YSRSD_T = "YSR Self-destructive/Identity T score"
   YSRINT_T = "YSR internalizing T score"
   YSREXT_T = "YSR externalizing T score"
   YSRTOT_T = "YSR total problems T score"
   AGEMNTHS = "Age in Months"
   YREXT_BL = "YSR Externalizing: Borderline Range"
   YREXT_CR = "YSR Externalizing: Clinical Range (> 63)"
   YRINT_BL = "YSR Internalizing: Borderline Range"
   YRINT_CR = "YSR Internalizing: Clinical Range (> 63)"
   YRTOT_BL = "YSR Total Score: Borderline Range"
   YRTOT_CR = "YSR Total Score: Clinical Range (> 63)"
   YRAGG_BL = "YSR Aggressive Beh: Borderline Range"
   YRAGG_CR = "YSR Aggressive Beh: Clinical Range"
   YRATT_BL = "YSR Attention Prob: Borderline Range"
   YRATT_CR = "YSR Attention Prob: Clinical Range"
   YRANX_BL = "YSR Anx/Depressed: Borderline Range"
   YRANX_CR = "YSR Anx/Depressed: Clinical Range (> 70)"
   YRDEL_BL = "YSR Delinquent Beh: Borderline Range"
   YRDEL_CR = "YSR Delinquent Beh: Clinical Range"
   YRSOC_BL = "YSR Social Prob: Borderline Range"
   YRSOC_CR = "YSR Social Prob: Clinical Range (> 70)"
   YRSOM_BL = "YRS Somatic: Borderline Range (67-70)"
   YRSOM_CR = "YRS Somatic: Clinical Range (> 70)"
   YRTHT_BL = "YSR Thought Prob: Borderline Range"
   YRTHT_CR = "YRS Thought Prob: Clinical Range (> 70)"
   YRWIT_BL = "YSR Withdrawn: Borderline Range (67-70)"
   YRWIT_CR = "YSR Withdrawn: Clinical Range (> 70)"
   YRSD_BL = "YSR Self-destr: Borderline Range (67-70)"
   YRSD_CR = "YSR Self-destr: Clinical Range (> 70)"
   YCACT_BL = "YSR Social_Activity: Borderline Range"
   YCACT_CR = "YSR Social_Activity: Clinical Range"
   YCSOC_BL = "YSR Social_Social: Borderline Range"
   YCSOC_CR = "YSR Social_Social: Clinical Range (< 30)"
   YCTOT_BL = "YSR Social_Total: Borderline Range"
   YCTOT_CR = "YSR Social_Total: Clinical Range (< 37)" ;

LENGTH
   YSRWIT_R 3               YSRWIT_T 3               YSRSOM_R 3            
   YSRSOM_T 3               YSRANX_R 3               YSRANX_T 3            
   YSRSOC_R 3               YSRSOC_T 3               YSRTHT_R 3            
   YSRTHT_T 3               YSRATT_R 3               YSRATT_T 3            
   YSRDEL_R 3               YSRDEL_T 3               YSRAGG_R 3            
   YSRAGG_T 3               YSC_ACTT 3               YSC_SOCT 3            
   YSC_TOTT 3               VISIT 3                  YSRSD_R 3             
   YSRINT_R 3               YSREXT_R 3               YSRTOT_R 4            
   YSRSD_T 3                YSRINT_T 3               YSREXT_T 3            
   YSRTOT_T 3               AGEMNTHS 4               YREXT_BL 3            
   YREXT_CR 3               YRINT_BL 3               YRINT_CR 3            
   YRTOT_BL 3               YRTOT_CR 3               YRAGG_BL 3            
   YRAGG_CR 3               YRATT_BL 3               YRATT_CR 3            
   YRANX_BL 3               YRANX_CR 3               YRDEL_BL 3            
   YRDEL_CR 3               YRSOC_BL 3               YRSOC_CR 3            
   YRSOM_BL 3               YRSOM_CR 3               YRTHT_BL 3            
   YRTHT_CR 3               YRWIT_BL 3               YRWIT_CR 3            
   YRSD_BL 3                YRSD_CR 3                YCACT_BL 3            
   YCACT_CR 3               YCSOC_BL 3               YCSOC_CR 3            
   YCTOT_BL 3               YCTOT_CR 3 ;

      

RUN ;
