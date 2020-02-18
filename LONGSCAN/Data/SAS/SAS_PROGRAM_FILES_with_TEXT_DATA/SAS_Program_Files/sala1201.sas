/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\sala1201.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = sala1201 ;

DATA library.&dataset ;
INFILE datafile LRECL=360;
INPUT
   CENTER $ 1-2             ID $ 3-9                 VISIT  10-11          
   SALA1  12                SALA2  13                SALA3  14             
   SALA4  15                SALA5  16                SALA6  17             
   SALA8  18                SALA10A  19              SALA10B  20           
   SALA11A  21              SALA11B  22              SALA12A  23           
   SALA12B  24              SALA12C  25              SALA12D  26           
   SALA13A  27              SALA13B  28              SALA13C  29           
   SALA13D  30              SALA14A  31              SALA14B  32           
   SALA14C  33              SALA14D  34              SALA14E  35           
   SALA14F  36              SALA14G  37              SALA14H  38           
   SALA14I  39              SALA14J  40              SALA14K  41           
   SALA15A  42              SALA15B  43              SALA15C  44           
   SALA15D  45              SALA15E  46              SALA15F  47           
   SALA15G  48              SALA15H  49              SALA15I  50           
   SALA15J  51              SALA15K  52              SALA16A  53           
   SALA16B  54              SALA16C  55              SALA16D  56           
   SALA16E  57              SALA16F  58              SALA16G  59           
   SALA16H  60              SALA16I  61              SALA16J  62           
   SALA16K  63              SALA17A  64              SALA17B  65           
   SALA17C  66              SALA17D  67              SALA17E  68           
   SALA17F  69              SALA17G  70              SALA17H  71           
   SALA17I  72              SALA17J  73              SALA17K  74           
   SALA18A  75              SALA18B  76              SALA18C  77           
   SALA18D  78              SALA19A  79              SALA19B  80           
   SALA19C  81              SALA19D  82              SALA20A  83           
   SALA20B  84              SALA20C  85              SALA20D  86           
   SALA20E  87              SALA20F  88              SALA20G  89           
   SALA20H  90              SALA20I  91              SALA20J  92           
   SALA20K  93              SALA21A  94              SALA21B  95           
   SALA21C  96              SALA21D  97              SALA21E  98           
   SALA21F  99              SALA21G  100             SALA21H  101          
   SALA21I  102             SALA21J  103             SALA21K  104          
   SALA22A  105             SALA22B  106             SALA22C  107          
   SALA22D  108             SALA22E  109             SALA22F  110          
   SALA7A  111              SALA7B  112              SALA9A  113           
   SALA9B  114              SALA22G  115             SALA22H  116          
   SALA22I  117             SALA22J  118             SALA22K  119          
   SALA23A  120             SALA23B  121             SALA23C  122          
   SALA23D  123             SALA23E  124             SALA23F  125          
   SALA23G  126             SALA23H  127             SALA23I  128          
   SALA23J  129             SALA23K  130             SALA24A  131          
   SALA24B  132             SALA24C  133             SALA24D  134          
   SALA25A  135             SALA25B  136             SALA25C  137          
   SALA25D  138             SALA26A  139             SALA26B  140          
   SALA26C  141             SALA26D  142             SALA26E  143          
   SALA26F  144             SALA26G  145             SALA26H  146          
   SALA26I  147             SALA26J  148             SALA26K  149          
   SALA27A  150             SALA27B  151             SALA27C  152          
   SALA27D  153             SALA27E  154             SALA27F  155          
   SALA27G  156             SALA27H  157             SALA27I  158          
   SALA27J  159             SALA27K  160             SALA28A  161          
   SALA28B  162             SALA28C  163             SALA28D  164          
   SALA28E  165             SALA28F  166             SALA28G  167          
   SALA28H  168             SALA28I  169             SALA28J  170          
   SALA28K  171             SALA29A  172             SALA29B  173          
   SALA29C  174             SALA29D  175             SALA29E  176          
   SALA29F  177             SALA29G  178             SALA29H  179          
   SALA29I  180             SALA29J  181             SALA29K  182          
   SALA30A  183             SALA30B  184             SALA30C  185          
   SALA30D  186             SALA31A  187             SALA31B  188          
   SALA31C  189             SALA31D  190             SALA32A  191          
   SALA32B  192             SALA32C  193             SALA32D  194          
   SALA32E  195             SALA32F  196             SALA32G  197          
   SALA32H  198             SALA32I  199             SALA32J  200          
   SALA32K  201             SALA33A  202             SALA33B  203          
   SALA33C  204             SALA33D  205             SALA33E  206          
   SALA33F  207             SALA33G  208             SALA33H  209          
   SALA33I  210             SALA33J  211             SALA33K  212          
   SALA34A  213             SALA34B  214             SALA34C  215          
   SALA34D  216             SALA34E  217             SALA34F  218          
   SALA34G  219             SALA34H  220             SALA34I  221          
   SALA34J  222             SALA34K  223             SALA35A  224          
   SALA35B  225             SALA35C  226             SALA35D  227          
   SALA35E  228             SALA35F  229             SALA35G  230          
   SALA35H  231             SALA35I  232             SALA35J  233          
   SALA35K  234             SALA36A  235             SALA36B  236          
   SALA36C  237             SALA36D  238             SALA37A  239          
   SALA37B  240             SALA37C  241             SALA37D  242          
   SALA38A  243             SALA38B  244             SALA38C  245          
   SALA38D  246             SALA38E  247             SALA38F  248          
   SALA38G  249             SALA38H  250             SALA38I  251          
   SALA38J  252             SALA38K  253             SALA39A  254          
   SALA39B  255             SALA39C  256             SALA39D  257          
   SALA39E  258             SALA39F  259             SALA39G  260          
   SALA39H  261             SALA39I  262             SALA39J  263          
   SALA39K  264             SALA40A  265             SALA40B  266          
   SALA40C  267             SALA40D  268             SALA40E  269          
   SALA40F  270             SALA40G  271             SALA40H  272          
   SALA40I  273             SALA40J  274             SALA40K  275          
   SALA41A  276             SALA41B  277             SALA41C  278          
   SALA41D  279             SALA41E  280             SALA41F  281          
   SALA41G  282             SALA41H  283             SALA41I  284          
   SALA41J  285             SALA41K  286             SALA42A  287          
   SALA42B  288             SALA42C  289             SALA42D  290          
   SALA43A  291             SALA43B  292             SALA43C  293          
   SALA43D  294             SALA44A  295             SALA44B  296          
   SALA44C  297             SALA44D  298             SALA44E  299          
   SALA44F  300             SALA44G  301             SALA44H  302          
   SALA44I  303             SALA44J  304             SALA44K  305          
   SALA45A  306             SALA45B  307             SALA45C  308          
   SALA45D  309             SALA45E  310             SALA45F  311          
   SALA45G  312             SALA48  313              SALA49  314           
   SALA50  315              SALA51  316              SALA52  317           
   SALA53  318              SALA54  319              SALA55  320           
   SALA56  321              SALA57  322              SALA58  323           
   SALA59  324              SALA60  325              SALA61  326           
   SALA62  327              SALA63  328              SALA64  329           
   SALA65  330              SALA66  331              SALA67  332           
   SALA68  333              SALA69  334              SALA45H  335          
   SALA45I  336             SALA45J  337             SALA45K  338          
   SALA46A  339             SALA46B  340             SALA46C  341          
   SALA46D  342             SALA46E  343             SALA46F  344          
   SALA46G  345             SALA46H  346             SALA46I  347          
   SALA46J  348             SALA46K  349             SALA47A  350          
   SALA47B  351             SALA47C  352             SALA47D  353          
   SALA47E  354             SALA47F  355             SALA47G  356          
   SALA47H  357             SALA47I  358             SALA47J  359          
   SALA47K  360           ;                       

LABEL
   CENTER = "FIELD CENTER"
   ID = "LONGSCAN SUBJECT ID"
   VISIT = "VISIT NUMBER"
   SALA1 = "Look at your private parts in sexual way"
   SALA2 = "Tried to get you to look at private prts"
   SALA3 = "Hit/punched you with hand or fist"
   SALA4 = "Say inappropriate sexual things to you"
   SALA5 = "Touch/feel your priv.parts"
   SALA6 = "Get you to touch their priv.parts"
   SALA8 = "Do something sexual to you"
   SALA10A = "G-Have sex with you"
   SALA10B = "B-Have sex with you"
   SALA11A = "G-Put finger in priv.parts"
   SALA11B = "B-Put finger in priv.parts"
   SALA12A = "Look priv.parts/happen:5/<5yrs"
   SALA12B = "Look priv.parts/happen:6-12yrs"
   SALA12C = "Look priv.parts/happen:13-16yrs"
   SALA12D = "Look priv.parts/happen:17/>17yrs"
   SALA13A = "Look priv.parts/# times:5/< 5yrs"
   SALA13B = "Look priv.parts/# times:6-12yrs"
   SALA13C = "Look priv.parts/# times:13-16yrs"
   SALA13D = "Look priv.parts/# times:17/>17yrs"
   SALA14A = "Look priv.parts/5/<5yr-Mother"
   SALA14B = "Look priv.parts/5/<5yr-Stepmother"
   SALA14C = "Look priv.parts/5/<5yr-Foster mother"
   SALA14D = "Look priv.parts/5/<5yr-Parnt grlfrend"
   SALA14E = "Look priv.parts/5/<5yr-Father"
   SALA14F = "Look priv.parts/5/<5yr-Stepfather"
   SALA14G = "Look priv.parts/5/<5yr-Foster father"
   SALA14H = "Look priv.parts/5/<5yr-Parnt boyfrend"
   SALA14I = "Look priv.parts/5/<5yr-Othr famly membr"
   SALA14J = "Look priv.parts/5/<5yr-Teachr/coach etc"
   SALA14K = "Look priv.parts/5/<5yr-Someone else"
   SALA15A = "Look priv.parts/6-12yr-Mother"
   SALA15B = "Look priv.parts/6-12yr-Stepmother"
   SALA15C = "Look priv.parts/6-12yr-Foster mother"
   SALA15D = "Look priv.parts/6-12yr-Parnt grlfrend"
   SALA15E = "Look priv.parts/6-12yr-Father"
   SALA15F = "Look priv.parts/6-12yr-Stepfather"
   SALA15G = "Look priv.parts/6-12yr-Foster father"
   SALA15H = "Look priv.parts/6-12yr-Parnt boyfrend"
   SALA15I = "Look priv.parts/6-12yr-Othr famly membr"
   SALA15J = "Look priv.parts/6-12yr-Teachr/coach etc"
   SALA15K = "Look priv.parts/6-12yr-Someone else"
   SALA16A = "Look priv.parts/13-16yr-Mother"
   SALA16B = "Look priv.parts/13-16yr-Stepmother"
   SALA16C = "Look priv.parts/13-16yr-Foster mother"
   SALA16D = "Look priv.parts/13-16yr-Paret grlfrend"
   SALA16E = "Look priv.parts/13-16yr-Father"
   SALA16F = "Look priv.parts/13-16yr-Stepfather"
   SALA16G = "Look priv.parts/13-16yr-Foster father"
   SALA16H = "Look priv.parts/13-16yr-Parnt boyfrend"
   SALA16I = "Look priv.parts/13-16yr-Othr famly membr"
   SALA16J = "Look priv.parts/13-16yr-Teachr/coach etc"
   SALA16K = "Look priv.parts/13-16yr-Someone else"
   SALA17A = "Look priv.parts/17/>17yr-Mother"
   SALA17B = "Look priv.parts/17/>17yr-Stepmother"
   SALA17C = "Look priv.parts/17/>17yr-Foster mother"
   SALA17D = "Look priv.parts/17/>17yr-Parnt grlfrend"
   SALA17E = "Look priv.parts/17/>17yr-Father"
   SALA17F = "Look priv.parts/17/>17yr-Stepfather"
   SALA17G = "Look priv.parts/17/>17yr-Foster father"
   SALA17H = "Look priv.parts/17/>17yr-Parnt boyfrend"
   SALA17I = "Look priv.prts/17/>17yr-Othr famly membr"
   SALA17J = "Look priv.prts/17/>17yr-Teachr/coach etc"
   SALA17K = "Look priv.parts/17/>17yr-Someone else"
   SALA18A = "Said sexual things/happen:5/<5yrs"
   SALA18B = "Said sexual things/happen:6-12yrs"
   SALA18C = "Said sexual things/happen:13-16yrs"
   SALA18D = "Said sexual things/happen:17/>17yrs"
   SALA19A = "Said sexual things/# times:5/< 5yrs"
   SALA19B = "Said sexual things/# times:6-12yrs"
   SALA19C = "Said sexual things/# times:13-16 yrs"
   SALA19D = "Said sexual things/# times:17/>17 yrs"
   SALA20A = "Said sexual thngs/5/<5yr-Mother"
   SALA20B = "Said sexual thngs/5/<5yr-Stepmother"
   SALA20C = "Said sexual thngs/5/<5yr-Foster mother"
   SALA20D = "Said sexual thngs/5/<5yr-Parnt grlfrend"
   SALA20E = "Said sexual thngs/5/<5yr-Father"
   SALA20F = "Said sexual thngs/5/<5yr-Stepfather"
   SALA20G = "Said sexual thngs/5/<5yr-Foster father"
   SALA20H = "Said sexual thngs/5/<5yr-Parnt boyfrend"
   SALA20I = "Said sexual thngs/5/<5yr-Othr famlymembr"
   SALA20J = "Said sexual thngs/5/<5yr-Teacher/coach"
   SALA20K = "Said sexual thngs/5/<5yr-Someone else"
   SALA21A = "Said sexual thngs/6-12yr-Mother"
   SALA21B = "Said sexual thngs/6-12yr-Stepmother"
   SALA21C = "Said sexual thngs/6-12yr-Foster mother"
   SALA21D = "Said sexual thngs/6-12yr-Parnt grlfrend"
   SALA21E = "Said sexual thngs/6-12yr-Father"
   SALA21F = "Said sexual thngs/6-12yr-Stepfather"
   SALA21G = "Said sexual thngs/6-12yr-Foster father"
   SALA21H = "Said sexual thngs/6-12yr-Parent boyfrend"
   SALA21I = "Said sexual thngs/6-12yr-Othr famlymembr"
   SALA21J = "Said sexual thngs/6-12yr-Teachr/coach"
   SALA21K = "Said sexual thngs/6-12yr-Someone else"
   SALA22A = "Said sexual thngs/13-16yr-Mother"
   SALA22B = "Said sexual thngs/13-16yr-Stepmother"
   SALA22C = "Said sexual thngs/13-16yr-Foster mother"
   SALA22D = "Said sexual thngs/13-16yr-Parnt grlfren"
   SALA22E = "Said sexual thngs/13-16yr-Father"
   SALA22F = "Said sexual thngs/13-16yr-Stepfather"
   SALA7A = "G-Hurt/try to hurt your private parts"
   SALA7B = "B-Hurt/try to hurt your private parts"
   SALA9A = "G-Kiss/put mouth on your breasts"
   SALA9B = "B-Kiss/put mouth on butt"
   SALA22G = "Said sexual thngs/13-16yr-Foster father"
   SALA22H = "Said sexual thngs/13-16yr-Parnt boyfrend"
   SALA22I = "Said sexual thngs/13-16yr-Othr famlymembr"
   SALA22J = "Said sexual thngs/13-16yr-Teachr/coach"
   SALA22K = "Said sexual thngs/13-16yr-Someone else"
   SALA23A = "Said sexual thngs/17/>17yr-Mother"
   SALA23B = "Said sexual thngs/17/>17yr-Stepmother"
   SALA23C = "Said sexual thngs/17/>17yr-Foster mother"
   SALA23D = "Said sexual thngs/17/>17yr-Parnt grlfrnd"
   SALA23E = "Said sexual thngs/17/>17yr-Father"
   SALA23F = "Said sexual thngs/17/>17yr-Stepfather"
   SALA23G = "Said sexual thngs/17/>17yr-Foster father"
   SALA23H = "Said sexual thngs/17/>17yr-Parnt boyfrnd"
   SALA23I = "Said sexual thngs/17/>17yr-Othr fam.memb"
   SALA23J = "Said sexual thngs/17/>17yr-Teachr/coach"
   SALA23K = "Said sexual thngs/17/>17yr-Someone else"
   SALA24A = "Touch priv.parts/happen: 5/<5yrs"
   SALA24B = "Touch priv.parts/happen: 6-12yrs"
   SALA24C = "Touch priv.parts/happen: 13-16yrs"
   SALA24D = "Touch priv.parts/happen: 17/>17yrs"
   SALA25A = "Touch priv.parts/# times: 5/< 5yrs"
   SALA25B = "Touch priv.parts/# times: 6-12 yrs"
   SALA25C = "Touch priv.parts/# times: 13-17 yrs"
   SALA25D = "Touch priv.parts/# times: 17/>17 yrs"
   SALA26A = "Touch priv.parts/5/<5yr-Mother"
   SALA26B = "Touch priv.parts/5/<5yr-Stepmother"
   SALA26C = "Touch priv.parts/5/<5yr-Foster mother"
   SALA26D = "Touch priv.parts/5/<5yr-Parnt grlfrend"
   SALA26E = "Touch priv.parts/5/<5yr-Father"
   SALA26F = "Touch priv.parts/5/<5yr-Stepfather"
   SALA26G = "Touch priv.parts/5/<5yr-Foster father"
   SALA26H = "Touch priv.parts/5/<5yr-Parent boyfrend"
   SALA26I = "Touch priv.parts/5/<5yr-Othr famlymembr"
   SALA26J = "Touch priv.parts/5/<5yr-Teacher/coach"
   SALA26K = "Touch priv.parts/5/<5yr-Someone else"
   SALA27A = "Touch priv.parts/6-12yr-Mother"
   SALA27B = "Touch priv.parts/6-12yr-Stepmother"
   SALA27C = "Touch priv.parts/6-12yr-Foster mother"
   SALA27D = "Touch priv.parts/6-12yr-Parnt grlfrend"
   SALA27E = "Touch priv.parts/6-12yr-Father"
   SALA27F = "Touch priv.parts/6-12yr-Stepfather"
   SALA27G = "Touch priv.parts/6-12yr-Foster father"
   SALA27H = "Touch priv.parts/6-12yr-Parent boyfrend"
   SALA27I = "Touch priv.parts/6-12yr-Othr famlymembr"
   SALA27J = "Touch priv.parts/6-12yr-Teachr/coach"
   SALA27K = "Touch priv.parts/6-12yr-Someone else"
   SALA28A = "Touch priv.parts/13-16yr-Mother"
   SALA28B = "Touch priv.parts/13-16yr-Stepmother"
   SALA28C = "Touch priv.parts/13-16yr-Foster mother"
   SALA28D = "Touch priv.parts/13-16yr-Parnt grlfrend"
   SALA28E = "Touch priv.parts/13-16yr-Father"
   SALA28F = "Touch priv.parts/13-16yr-Stepfather"
   SALA28G = "Touch priv.parts/13-16yr-Foster father"
   SALA28H = "Touch priv.parts/13-16yr-Parnt boyfrend"
   SALA28I = "Touch priv.parts/13-16yr-Othr famlymembr"
   SALA28J = "Touch priv.parts/13-16yr-Teachr/coach"
   SALA28K = "Touch priv.parts/13-16yr-Someone else"
   SALA29A = "Touch priv.parts/17/>17yr-Mother"
   SALA29B = "Touch priv.parts/17/>17yr-Stepmother"
   SALA29C = "Touch priv.parts/17/>17yr-Foster mother"
   SALA29D = "Touch priv.parts/17/>17yr-Parnt grlfrend"
   SALA29E = "Touch priv.parts/17/>17yr-Father"
   SALA29F = "Touch priv.parts/17/>17yr-Stepfather"
   SALA29G = "Touch priv.parts/17/>17yr-Foster father"
   SALA29H = "Touch priv.parts/17/>17yr-Parnt boyfrnd"
   SALA29I = "Touch priv.parts/17/>17yr-Othr fam.memb"
   SALA29J = "Touch priv.parts/17/>17yr-Teachr/coach"
   SALA29K = "Touch priv.parts/17/>17yr-Someone else"
   SALA30A = "Hurt priv.parts/happen: 5/<5yrs"
   SALA30B = "Hurt priv.parts/happen: 6-12yrs"
   SALA30C = "Hurt priv.parts/happen: 13-16yrs"
   SALA30D = "Hurt priv.parts/happen: 17/>17yrs"
   SALA31A = "Hurt priv.parts/# times: 5/< 5yrs"
   SALA31B = "Hurt priv.parts/# times: 6-12yrs"
   SALA31C = "Hurt priv.parts/# times: 13-16yrs"
   SALA31D = "Hurt priv.parts/# times: 17/>17 yrs"
   SALA32A = "Hurt priv.parts/5/<5yr-Mother"
   SALA32B = "Hurt priv.parts/5/<5yr-Stepmother"
   SALA32C = "Hurt priv.parts/5/<5yr-Foster mother"
   SALA32D = "Hurt priv.parts/5/<5yr-Parnt grlfrend"
   SALA32E = "Hurt priv.parts/5/<5yr-Father"
   SALA32F = "Hurt priv.parts/5/<5yr-Stepfather"
   SALA32G = "Hurt priv.parts/5/<5yr-Foster father"
   SALA32H = "Hurt priv.parts/5/<5yr-Parent boyfrend"
   SALA32I = "Hurt priv.parts/5/<5yr-Othr famlymembr"
   SALA32J = "Hurt priv.parts/5/<5yr-Teacher/coach"
   SALA32K = "Hurt priv.parts/5/<5yr-Someone else"
   SALA33A = "Hurt priv.parts/6-12yr-Mother"
   SALA33B = "Hurt priv.parts/6-12yr-Stepmother"
   SALA33C = "Hurt priv.parts/6-12yr-Foster mother"
   SALA33D = "Hurt priv.parts/6-12yr-Parnt grlfrend"
   SALA33E = "Hurt priv.parts/6-12yr-Father"
   SALA33F = "Hurt priv.parts/6-12yr-Stepfather"
   SALA33G = "Hurt priv.parts/6-12yr-Foster father"
   SALA33H = "Hurt priv.parts/6-12yr-Parent boyfrend"
   SALA33I = "Hurt priv.parts/6-12yr-Othr famlymembr"
   SALA33J = "Hurt priv.parts/6-12yr-Teachr/coach"
   SALA33K = "Hurt priv.parts/6-12yr-Someone else"
   SALA34A = "Hurt priv.parts/13-16yr-Mother"
   SALA34B = "Hurt priv.parts/13-16yr-Stepmother"
   SALA34C = "Hurt priv.parts/13-16yr-Foster mother"
   SALA34D = "Hurt priv.parts/13-16yr-Parnt grlfrend"
   SALA34E = "Hurt priv.parts/13-16yr-Father"
   SALA34F = "Hurt priv.parts/13-16yr-Stepfather"
   SALA34G = "Hurt priv.parts/13-16yr-Foster father"
   SALA34H = "Hurt priv.parts/13-16yr-Parnt boyfrend"
   SALA34I = "Hurt priv.parts/13-16yr-Othr famlymembr"
   SALA34J = "Hurt priv.parts/13-16yr-Teachr/coach"
   SALA34K = "Hurt priv.parts/13-16yr-Someone else"
   SALA35A = "Hurt priv.parts/17/>17yr-Mother"
   SALA35B = "Hurt priv.parts/17/>17yr-Stepmother"
   SALA35C = "Hurt priv.parts/17/>17yr-Foster mother"
   SALA35D = "Hurt priv.parts/17/>17yr-Parnt grlfrend"
   SALA35E = "Hurt priv.parts/17/>17yr-Father"
   SALA35F = "Hurt priv.parts/17/>17yr-Stepfather"
   SALA35G = "Hurt priv.parts/17/>17yr-Foster father"
   SALA35H = "Hurt priv.parts/17/>17yr-Parnt boyfrnd"
   SALA35I = "Hurt priv.parts/17/>17yr-Othr fam.memb"
   SALA35J = "Hurt priv.parts/17/>17yr-Teachr/coach"
   SALA35K = "Hurt priv.parts/17/>17yr-Someone else"
   SALA36A = "Had sex with you/happen: 5/<5yrs"
   SALA36B = "Had sex with you/happen: 6-12yrs"
   SALA36C = "Had sex with you/happen: 13-16yrs"
   SALA36D = "Had sex with you/happen: 17/>17yrs"
   SALA37A = "Had sex with you/# times: 5/< 5yrs"
   SALA37B = "Had sex with you/# times: 6-12yrs"
   SALA37C = "Had sex with you/# times: 13-16yrs"
   SALA37D = "Had sex with you/# times: 17/>17 yrs"
   SALA38A = "Had sex with you/5/<5yr-Mother"
   SALA38B = "Had sex with you/5/<5yr-Stepmother"
   SALA38C = "Had sex with you/5/<5yr-Foster mother"
   SALA38D = "Had sex with you/5/<5yr-Parnt grlfrend"
   SALA38E = "Had sex with you/5/<5yr-Father"
   SALA38F = "Had sex with you/5/<5yr-Stepfather"
   SALA38G = "Had sex with you/5/<5yr-Foster father"
   SALA38H = "Had sex with you/5/<5yr-Parent boyfrend"
   SALA38I = "Had sex with you/5/<5yr-Othr famlymembr"
   SALA38J = "Had sex with you/5/<5yr-Teacher/coach"
   SALA38K = "Had sex with you/5/<5yr-Someone else"
   SALA39A = "Had sex with you/6-12yr-Mother"
   SALA39B = "Had sex with you/6-12yr-Stepmother"
   SALA39C = "Had sex with you/6-12yr-Foster mother"
   SALA39D = "Had sex with you/6-12yr-Parnt grlfrend"
   SALA39E = "Had sex with you/6-12yr-Father"
   SALA39F = "Had sex with you/6-12yr-Stepfather"
   SALA39G = "Had sex with you/6-12yr-Foster father"
   SALA39H = "Had sex with you/6-12yr-Parent boyfrend"
   SALA39I = "Had sex with you/6-12yr-Othr famlymembr"
   SALA39J = "Had sex with you/6-12yr-Teachr/coach"
   SALA39K = "Had sex with you/6-12yr-Someone else"
   SALA40A = "Had sex with you/13-16yr-Mother"
   SALA40B = "Had sex with you/13-16yr-Stepmother"
   SALA40C = "Had sex with you/13-16yr-Foster mother"
   SALA40D = "Had sex with you/13-16yr-Parnt grlfrend"
   SALA40E = "Had sex with you/13-16yr-Father"
   SALA40F = "Had sex with you/13-16yr-Stepfather"
   SALA40G = "Had sex with you/13-16yr-Foster father"
   SALA40H = "Had sex with you/13-16yr-Parnt boyfrend"
   SALA40I = "Had sex with you/13-16yr-Othr famlymembr"
   SALA40J = "Had sex with you/13-16yr-Teachr/coach"
   SALA40K = "Had sex with you/13-16yr-Someone else"
   SALA41A = "Had sex with you/17/>17yr-Mother"
   SALA41B = "Had sex with you/17/>17yr-Stepmother"
   SALA41C = "Had sex with you/17/>17yr-Foster mother"
   SALA41D = "Had sex with you/17/>17yr-Parnt grlfrend"
   SALA41E = "Had sex with you/17/>17yr-Father"
   SALA41F = "Had sex with you/17/>17yr-Stepfather"
   SALA41G = "Had sex with you/17/>17yr-Foster father"
   SALA41H = "Had sex with you/17/>17yr-Parnt boyfrnd"
   SALA41I = "Had sex with you/17/>17yr-Othr fam.memb"
   SALA41J = "Had sex with you/17/>17yr-Teachr/coach"
   SALA41K = "Had sex with you/17/>17yr-Someone else"
   SALA42A = "Had sex with you/happen: 5/<5yrs"
   SALA42B = "Had sex with you/happen: 6-12yrs"
   SALA42C = "Had sex with you/happen: 13-16yrs"
   SALA42D = "Had sex with you/happen: 17/>17yrs"
   SALA43A = "Had sex with you/# times: 5/< 5yrs"
   SALA43B = "Had sex with you/# times: 6-12yrs"
   SALA43C = "Had sex with you/# times: 13-16yrs"
   SALA43D = "Had sex with you/# times: 17/>17 yrs"
   SALA44A = "Had sex with you/5/<5yr-Mother"
   SALA44B = "Had sex with you/5/<5yr-Stepmother"
   SALA44C = "Had sex with you/5/<5yr-Foster mother"
   SALA44D = "Had sex with you/5/<5yr-Parnt grlfrend"
   SALA44E = "Had sex with you/5/<5yr-Father"
   SALA44F = "Had sex with you/5/<5yr-Stepfather"
   SALA44G = "Had sex with you/5/<5yr-Foster father"
   SALA44H = "Had sex with you/5/<5yr-Parent boyfrend"
   SALA44I = "Had sex with you/5/<5yr-Othr famlymembr"
   SALA44J = "Had sex with you/5/<5yr-Teacher/coach"
   SALA44K = "Had sex with you/5/<5yr-Someone else"
   SALA45A = "Had sex with you/6-12yr-Mother"
   SALA45B = "Had sex with you/6-12yr-Stepmother"
   SALA45C = "Had sex with you/6-12yr-Foster mother"
   SALA45D = "Had sex with you/6-12yr-Parnt grlfrend"
   SALA45E = "Had sex with you/6-12yr-Father"
   SALA45F = "Had sex with you/6-12yr-Stepfather"
   SALA45G = "Had sex with you/6-12yr-Foster father"
   SALA48 = "How much what happen/father affect you"
   SALA49 = "How much your fault/father"
   SALA50 = "How much what happen/stepfathr affect"
   SALA51 = "How much your fault/stepfather"
   SALA52 = "How much what happen/fosterfathr affect"
   SALA53 = "How much your fault/fosterfather"
   SALA54 = "How much what happen/parnt b/frnd affect"
   SALA55 = "How much your fault/parent's boyfriend"
   SALA56 = "How much what happen/mother affect"
   SALA57 = "How much your fault/mother"
   SALA58 = "How much what happen/stepmother affect"
   SALA59 = "How much your fault/stepmother"
   SALA60 = "How much what happen/fostermother affect"
   SALA61 = "How much your fault/fostermother"
   SALA62 = "How much what happen/parnt g/frnd affect"
   SALA63 = "How much your fault/parent g/friend"
   SALA64 = "How much wht happn/other fam. memb afect"
   SALA65 = "How much your fault/other family memb"
   SALA66 = "How much what happen/teacher etc affect"
   SALA67 = "How much your fault/teacher ect"
   SALA68 = "How much what happen/someone else affect"
   SALA69 = "How much your fault/someone else"
   SALA45H = "Had sex with you/6-12yr-Parent boyfrend"
   SALA45I = "Had sex with you/6-12yr-Othr famlymembr"
   SALA45J = "Had sex with you/6-12yr-Teachr/coach"
   SALA45K = "Had sex with you/6-12yr-Someone else"
   SALA46A = "Had sex with you/13-16yr-Mother"
   SALA46B = "Had sex with you/13-16yr-Stepmother"
   SALA46C = "Had sex with you/13-16yr-Foster mother"
   SALA46D = "Had sex with you/13-16yr-Parnt grlfrend"
   SALA46E = "Had sex with you/13-16yr-Father"
   SALA46F = "Had sex with you/13-16yr-Stepfather"
   SALA46G = "Had sex with you/13-16yr-Foster father"
   SALA46H = "Had sex with you/13-16yr-Parnt boyfrend"
   SALA46I = "Had sex with you/13-16yr-Othr famlymembr"
   SALA46J = "Had sex with you/13-16yr-Teachr/coach"
   SALA46K = "Had sex with you/13-16yr-Someone else"
   SALA47A = "Had sex with you/17/>17yr-Mother"
   SALA47B = "Had sex with you/17/>17yr-Stepmother"
   SALA47C = "Had sex with you/17/>17yr-Foster mother"
   SALA47D = "Had sex with you/17/>17yr-Parnt grlfrend"
   SALA47E = "Had sex with you/17/>17yr-Father"
   SALA47F = "Had sex with you/17/>17yr-Stepfather"
   SALA47G = "Had sex with you/17/>17yr-Foster father"
   SALA47H = "Had sex with you/17/>17yr-Parnt boyfrnd"
   SALA47I = "Had sex with you/17/>17yr-Othr fam.memb"
   SALA47J = "Had sex with you/17/>17yr-Teachr/coach"
   SALA47K = "Had sex with you/17/>17yr-Someone else" ;

LENGTH
   VISIT 3                  SALA1 3                  SALA2 3               
   SALA3 3                  SALA4 3                  SALA5 3               
   SALA6 3                  SALA8 3                  SALA10A 3             
   SALA10B 3                SALA11A 3                SALA11B 3             
   SALA12A 3                SALA12B 3                SALA12C 3             
   SALA12D 3                SALA13A 3                SALA13B 3             
   SALA13C 3                SALA13D 3                SALA14A 3             
   SALA14B 3                SALA14C 3                SALA14D 3             
   SALA14E 3                SALA14F 3                SALA14G 3             
   SALA14H 3                SALA14I 3                SALA14J 3             
   SALA14K 3                SALA15A 3                SALA15B 3             
   SALA15C 3                SALA15D 3                SALA15E 3             
   SALA15F 3                SALA15G 3                SALA15H 3             
   SALA15I 3                SALA15J 3                SALA15K 3             
   SALA16A 3                SALA16B 3                SALA16C 3             
   SALA16D 3                SALA16E 3                SALA16F 3             
   SALA16G 3                SALA16H 3                SALA16I 3             
   SALA16J 3                SALA16K 3                SALA17A 3             
   SALA17B 3                SALA17C 3                SALA17D 3             
   SALA17E 3                SALA17F 3                SALA17G 3             
   SALA17H 3                SALA17I 3                SALA17J 3             
   SALA17K 3                SALA18A 3                SALA18B 3             
   SALA18C 3                SALA18D 3                SALA19A 3             
   SALA19B 3                SALA19C 3                SALA19D 3             
   SALA20A 3                SALA20B 3                SALA20C 3             
   SALA20D 3                SALA20E 3                SALA20F 3             
   SALA20G 3                SALA20H 3                SALA20I 3             
   SALA20J 3                SALA20K 3                SALA21A 3             
   SALA21B 3                SALA21C 3                SALA21D 3             
   SALA21E 3                SALA21F 3                SALA21G 3             
   SALA21H 3                SALA21I 3                SALA21J 3             
   SALA21K 3                SALA22A 3                SALA22B 3             
   SALA22C 3                SALA22D 3                SALA22E 3             
   SALA22F 3                SALA7A 3                 SALA7B 3              
   SALA9A 3                 SALA9B 3                 SALA22G 3             
   SALA22H 3                SALA22I 3                SALA22J 3             
   SALA22K 3                SALA23A 3                SALA23B 3             
   SALA23C 3                SALA23D 3                SALA23E 3             
   SALA23F 3                SALA23G 3                SALA23H 3             
   SALA23I 3                SALA23J 3                SALA23K 3             
   SALA24A 3                SALA24B 3                SALA24C 3             
   SALA24D 3                SALA25A 3                SALA25B 3             
   SALA25C 3                SALA25D 3                SALA26A 3             
   SALA26B 3                SALA26C 3                SALA26D 3             
   SALA26E 3                SALA26F 3                SALA26G 3             
   SALA26H 3                SALA26I 3                SALA26J 3             
   SALA26K 3                SALA27A 3                SALA27B 3             
   SALA27C 3                SALA27D 3                SALA27E 3             
   SALA27F 3                SALA27G 3                SALA27H 3             
   SALA27I 3                SALA27J 3                SALA27K 3             
   SALA28A 3                SALA28B 3                SALA28C 3             
   SALA28D 3                SALA28E 3                SALA28F 3             
   SALA28G 3                SALA28H 3                SALA28I 3             
   SALA28J 3                SALA28K 3                SALA29A 3             
   SALA29B 3                SALA29C 3                SALA29D 3             
   SALA29E 3                SALA29F 3                SALA29G 3             
   SALA29H 3                SALA29I 3                SALA29J 3             
   SALA29K 3                SALA30A 3                SALA30B 3             
   SALA30C 3                SALA30D 3                SALA31A 3             
   SALA31B 3                SALA31C 3                SALA31D 3             
   SALA32A 3                SALA32B 3                SALA32C 3             
   SALA32D 3                SALA32E 3                SALA32F 3             
   SALA32G 3                SALA32H 3                SALA32I 3             
   SALA32J 3                SALA32K 3                SALA33A 3             
   SALA33B 3                SALA33C 3                SALA33D 3             
   SALA33E 3                SALA33F 3                SALA33G 3             
   SALA33H 3                SALA33I 3                SALA33J 3             
   SALA33K 3                SALA34A 3                SALA34B 3             
   SALA34C 3                SALA34D 3                SALA34E 3             
   SALA34F 3                SALA34G 3                SALA34H 3             
   SALA34I 3                SALA34J 3                SALA34K 3             
   SALA35A 3                SALA35B 3                SALA35C 3             
   SALA35D 3                SALA35E 3                SALA35F 3             
   SALA35G 3                SALA35H 3                SALA35I 3             
   SALA35J 3                SALA35K 3                SALA36A 3             
   SALA36B 3                SALA36C 3                SALA36D 3             
   SALA37A 3                SALA37B 3                SALA37C 3             
   SALA37D 3                SALA38A 3                SALA38B 3             
   SALA38C 3                SALA38D 3                SALA38E 3             
   SALA38F 3                SALA38G 3                SALA38H 3             
   SALA38I 3                SALA38J 3                SALA38K 3             
   SALA39A 3                SALA39B 3                SALA39C 3             
   SALA39D 3                SALA39E 3                SALA39F 3             
   SALA39G 3                SALA39H 3                SALA39I 3             
   SALA39J 3                SALA39K 3                SALA40A 3             
   SALA40B 3                SALA40C 3                SALA40D 3             
   SALA40E 3                SALA40F 3                SALA40G 3             
   SALA40H 3                SALA40I 3                SALA40J 3             
   SALA40K 3                SALA41A 3                SALA41B 3             
   SALA41C 3                SALA41D 3                SALA41E 3             
   SALA41F 3                SALA41G 3                SALA41H 3             
   SALA41I 3                SALA41J 3                SALA41K 3             
   SALA42A 3                SALA42B 3                SALA42C 3             
   SALA42D 3                SALA43A 3                SALA43B 3             
   SALA43C 3                SALA43D 3                SALA44A 3             
   SALA44B 3                SALA44C 3                SALA44D 3             
   SALA44E 3                SALA44F 3                SALA44G 3             
   SALA44H 3                SALA44I 3                SALA44J 3             
   SALA44K 3                SALA45A 3                SALA45B 3             
   SALA45C 3                SALA45D 3                SALA45E 3             
   SALA45F 3                SALA45G 3                SALA48 3              
   SALA49 3                 SALA50 3                 SALA51 3              
   SALA52 3                 SALA53 3                 SALA54 3              
   SALA55 3                 SALA56 3                 SALA57 3              
   SALA58 3                 SALA59 3                 SALA60 3              
   SALA61 3                 SALA62 3                 SALA63 3              
   SALA64 3                 SALA65 3                 SALA66 3              
   SALA67 3                 SALA68 3                 SALA69 3              
   SALA45H 3                SALA45I 3                SALA45J 3             
   SALA45K 3                SALA46A 3                SALA46B 3             
   SALA46C 3                SALA46D 3                SALA46E 3             
   SALA46F 3                SALA46G 3                SALA46H 3             
   SALA46I 3                SALA46J 3                SALA46K 3             
   SALA47A 3                SALA47B 3                SALA47C 3             
   SALA47D 3                SALA47E 3                SALA47F 3             
   SALA47G 3                SALA47H 3                SALA47I 3             
   SALA47J 3                SALA47K 3 ;

       

RUN ;
