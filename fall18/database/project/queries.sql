--1.  Write a query to display the current salary for each employee in department 300. Assume that only current employees are kept in the system, 
--    and therefore the most current salary for each employee is the entry in the salary history with a NULL end date. Sort the output in descending
--    order by salary amount.

select * from
(
    select
    a.dept_num as department_number,
    a.emp_num as employee_number,
    a.emp_fname as employee_first_name,
    a.emp_lname as employee_last_name,
    b.sal_amount as latest_salary
    from employee as a
    left join salary_history as b
    on a.emp_num = b.emp_num
    where a.dept_num = 300
    and b.sal_end is null
) x
group by x.department_number, x.employee_number, x.employee_first_name, x.employee_last_name, x.latest_salary
order by x.latest_salary desc;

+-------------------+-----------------+---------------------+--------------------+---------------+
| department_number | employee_number | employee_first_name | employee_last_name | latest_salary |
+-------------------+-----------------+---------------------+--------------------+---------------+
|               300 |           83746 | SEAN                | RANKIN             |         95550 |
|               300 |           84328 | FERN                | CARPENTER          |         94090 |
|               300 |           83716 | HENRY               | RIVERA             |         85920 |
|               300 |           84432 | MERLE               | JAMISON            |         85360 |
|               300 |           83902 | ROCKY               | VARGAS             |         79540 |
|               300 |           83695 | CARROLL             | MENDEZ             |         79200 |
|               300 |           84500 | CHRISTINE           | WESTON             |         78690 |
|               300 |           84594 | ODELL               | TIDWELL            |         77400 |
|               300 |           83910 | LAUREN              | AVERY              |         76110 |
|               300 |           83359 | MERLE               | WATTS              |         72240 |
|               300 |           83790 | LAVINA              | ACEVEDO            |         72000 |
|               300 |           83433 | RONNA               | NORWOOD            |         68870 |
|               300 |           84521 | DELFINA             | JUDD               |         66000 |
|               300 |           83653 | LEEANN              | HORN               |         61920 |
|               300 |           83738 | PORTER              | STACY              |         58200 |
|               300 |           83788 | LANA                | DOWDY              |         56760 |
|               300 |           83867 | TRACIE              | KELLY              |         56750 |
|               300 |           84234 | LUISA               | MINER              |         54720 |
|               300 |           83637 | TANIKA              | CRANE              |         52870 |
|               300 |           83877 | STEPHAINE           | DUNLAP             |         52650 |
|               300 |           84035 | HAL                 | FISHER             |         51600 |
|               300 |           83729 | CORRINA             | RAMEY              |         48500 |
|               300 |           83732 | SAMMY               | DIGGS              |         44720 |
|               300 |           83644 | WILLA               | MAXWELL            |         43200 |
|               300 |           83312 | ROSALBA             | BAKER              |         42400 |
+-------------------+-----------------+---------------------+--------------------+---------------+
25 rows in set (0.00 sec)



--2.  Write a query to display the starting salary for each employee. The starting salary would be the entry in the salary history with
--    the oldest salary start date for each employee. Sort the output by employee number.

select
    b.emp_num as employee_number,
    b.emp_fname as employee_first_name,
    b.emp_lname as employee_last_name,
    x.sal_amount as first_salary
from 
(   select b.*,
    (
        select count(*)
        from salary_history a
        where a.emp_num = b.emp_num 
        and a.sal_from <= b.sal_from 
        order by a.emp_num,a.sal_from
    ) as ranked
    from salary_history b 
    order by b.emp_num,b.sal_from
)x
left join employee as b
on x.emp_num = b.emp_num
where x.ranked = 1
order by b.emp_num;

+-----------------+---------------------+--------------------+--------------+
| employee_number | employee_first_name | employee_last_name | first_salary |
+-----------------+---------------------+--------------------+--------------+
|           83304 | TAMARA              | MCDONALD           |        19770 |
|           83308 | CONNIE              | LOVE               |        11230 |
|           83312 | ROSALBA             | BAKER              |        39260 |
|           83314 | CHAROLETTE          | DAVID              |        15150 |
|           83318 | DARCIE              | PECK               |        22330 |
|           83321 | ANGELINA            | FARMER             |        18250 |
|           83332 | WILLARD             | LONG               |        23380 |
|           83341 | CHRISTINE           | CORTEZ             |        14510 |
|           83347 | QUINTIN             | WINN               |        17010 |
|           83349 | JENNIFFER           | SINGH              |        21220 |
|           83359 | MERLE               | WATTS              |        25370 |
|           83366 | PHOEBE              | BLEDSOE            |        23200 |
|           83371 | ROXANE              | MATHEWS            |        18140 |
|           83372 | CLAUDINE            | DAHL               |        25780 |
|           83374 | DARRON              | TILLEY             |        16940 |
|           83378 | FELICIA             | DUNHAM             |        34050 |
|           83382 | STELLA              | CONKLIN            |        12270 |
|           83385 | BRODERICK           | COLBERT            |        17350 |
|           83398 | ZACK                | GILES              |        34950 |
|           83403 | FELICITA            | PONCE              |        11700 |
|           83404 | LIZ                 | FENTON             |        32360 |
|           83411 | ROGELIO             | REDMOND            |         8990 |
|           83413 | KYRA                | MARINO             |        19250 |
|           83415 | CHASITY             | WHALEN             |        32930 |
|           83419 | EUGENE              | SNEED              |        15940 |
|           83423 | LINDSAY             | GOOD               |        26890 |
|           83428 | SHERRILL            | CRUM               |        10640 |
|           83432 | SAL                 | BENAVIDES          |        20530 |
|           83433 | RONNA               | NORWOOD            |        27850 |
|           83434 | GERALD              | WHALEY             |        40620 |
|           83437 | JUNE                | SWANSON            |        53450 |
|           83445 | QUINTIN             | STAHL              |        46560 |
|           83446 | CHARLINE            | CRAFT              |        20010 |
|           83451 | ROSALIE             | ELLIS              |        10440 |
|           83453 | TAMARA              | CARPENTER          |        31060 |
|           83465 | HELEN               | TRUJILLO           |        39000 |
|           83476 | DULCE               | MCCULLOUGH         |        19220 |
|           83477 | VELIA               | DOTSON             |        23250 |
|           83480 | DOLLY               | KING               |        22560 |
|           83495 | ABRAHAM             | STAHL              |         7520 |
|           83503 | GERARD              | CAMP               |        17100 |
|           83504 | RICHELLE            | WALKER             |        20770 |
|           83509 | FRANKLYN            | STOVER             |        72360 |
|           83511 | SHERMAN             | MICHEL             |        12900 |
|           83517 | SONDRA              | ALBRIGHT           |        23060 |
|           83519 | TAMMI               | KEYES              |        18840 |
|           83521 | PALMER              | DODGE              |        44210 |
|           83527 | LAN                 | STROUD             |         9260 |
|           83529 | LASHANDA            | BRITT              |        18260 |
|           83532 | STUART              | KENDRICK           |        17550 |
|           83534 | BOBBY               | NADEAU             |        24580 |
|           83537 | CLEO                | ENGLISH            |        58700 |
|           83542 | TOSHA               | LONGORIA           |         6940 |
|           83543 | ALICIA              | OROZCO             |        10250 |
|           83545 | JOVITA              | TAYLOR             |        16370 |
|           83547 | LON                 | HARRISON           |         6520 |
|           83555 | LUANNE              | SPEARS             |        11510 |
|           83558 | CRISTAL             | STALLINGS          |        30500 |
|           83562 | FELICITA            | MANUEL             |        68950 |
|           83564 | EDGARDO             | CONWAY             |         9060 |
|           83565 | LOURDES             | ABERNATHY          |        48080 |
|           83569 | ROSALINE            | TOMLINSON          |        34210 |
|           83573 | VALARIE             | BLEDSOE            |        23400 |
|           83575 | LINDSEY             | GILLIAM            |        26490 |
|           83583 | MARGARETE           | ROLLINS            |        19710 |
|           83593 | ROSANNE             | NASH               |        55250 |
|           83595 | LEEANN              | CLINTON            |        24180 |
|           83602 | BECKY               | SPEARS             |        33120 |
|           83603 | TANIKA              | PADGETT            |        29320 |
|           83607 | ALENE               | PEACOCK            |        24290 |
|           83608 | DARREL              | MAYFIELD           |        15900 |
|           83609 | SHAUN               | GIBBS              |        81950 |
|           83614 | ARIELLE             | FARLEY             |        25070 |
|           83615 | IGNACIO             | PECK               |        17460 |
|           83618 | CHRISTIAN           | DUNLAP             |        47500 |
|           83621 | FONDA               | GONZALEZ           |        50960 |
|           83624 | GERTRUDE            | ELLIOT             |        17870 |
|           83628 | FELICITA            | WALTERS            |        12900 |
|           83630 | BRENDAN             | DOVE               |         9960 |
|           83636 | ROSALIE             | SOSA               |        14690 |
|           83637 | TANIKA              | CRANE              |        11380 |
|           83639 | EVETTE              | MIRANDA            |        38220 |
|           83644 | WILLA               | MAXWELL            |        15580 |
|           83645 | BRYON               | BRADY              |        24270 |
|           83646 | NIGEL               | BURNS              |        14080 |
|           83647 | FRANCIS             | HALE               |        20310 |
|           83649 | DELMA               | JACOB              |        51680 |
|           83650 | GERALYN             | SHELDON            |        24090 |
|           83652 | GEORGINA            | EASON              |        20690 |
|           83653 | LEEANN              | HORN               |        42660 |
|           83656 | NATHALIE            | BOYLE              |        21130 |
|           83658 | SANTIAGO            | MARINO             |        15330 |
|           83661 | DAN                 | FINN               |        15100 |
|           83664 | STEPHENIE           | FINN               |        24220 |
|           83669 | MITCHELL            | RUSHING            |         6400 |
|           83677 | HERB                | MANNING            |        51010 |
|           83683 | MAYA                | STONE              |        54630 |
|           83691 | ROY                 | HEWITT             |        15190 |
|           83695 | CARROLL             | MENDEZ             |        29430 |
|           83696 | MIMI                | GONZALEZ           |        64350 |
|           83703 | TONEY               | HELTON             |         8830 |
|           83705 | JOSE                | BARR               |        70610 |
|           83707 | ARMAND              | ALDRICH            |         6640 |
|           83713 | KRISTEN             | LAIRD              |        40830 |
|           83716 | HENRY               | RIVERA             |        35080 |
|           83719 | JETTIE              | HICKS              |        15020 |
|           83721 | KERRI               | EASON              |        19820 |
|           83723 | MARYLIN             | MCCLELLAN          |        17360 |
|           83724 | SHERRYL             | PUCKETT            |        28630 |
|           83728 | SAMANTHA            | KERNS              |        12540 |
|           83729 | CORRINA             | RAMEY              |        16450 |
|           83731 | SHERON              | VARGAS             |        43740 |
|           83732 | SAMMY               | DIGGS              |        23040 |
|           83733 | YASMIN              | HORTON             |        26290 |
|           83734 | INEZ                | ROCHA              |        50910 |
|           83735 | MAVIS               | TAYLOR             |        17380 |
|           83737 | FAUSTO              | MICHEL             |        31000 |
|           83738 | PORTER              | STACY              |        35360 |
|           83739 | YESENIA             | BRIGGS             |        10390 |
|           83741 | VEDA                | LOPEZ              |        29650 |
|           83745 | DWAIN               | SPICER             |        56020 |
|           83746 | SEAN                | RANKIN             |        22200 |
|           83748 | CHRISTOPER          | WEIR               |         9010 |
|           83757 | KENDALL             | MORIN              |        37730 |
|           83758 | OLIVIA              | DELEON             |        23950 |
|           83759 | THOMAS              | CHARLES            |        18020 |
|           83763 | JAIME               | FELTON             |        52570 |
|           83767 | ELLA                | WILLARD            |        32570 |
|           83770 | HOLLIE              | CRAIG              |        35240 |
|           83773 | ARETHA              | BAKER              |         7160 |
|           83777 | KARRIE              | COOLEY             |        15170 |
|           83783 | AARON               | CARROLL            |        32380 |
|           83785 | ELISHA              | MCMULLEN           |        44440 |
|           83788 | LANA                | DOWDY              |        33260 |
|           83790 | LAVINA              | ACEVEDO            |        24000 |
|           83792 | WALLY               | ANDERSEN           |        53030 |
|           83797 | GALE                | KIRK               |        18750 |
|           83808 | JACINTO             | TAYLOR             |        46380 |
|           83813 | JERRI               | BENAVIDES          |        13970 |
|           83818 | KRIS                | BRIGHT             |        16660 |
|           83820 | TOM                 | HOBSON             |        44320 |
|           83824 | PHYLISS             | BOUCHARD           |        23030 |
|           83827 | SHELBY              | PRESLEY            |        40290 |
|           83838 | LEN                 | FLORES             |        16930 |
|           83839 | RONNY               | TANNER             |        40510 |
|           83841 | EDDY                | GOULD              |        27020 |
|           83844 | THOMAS              | HILTON             |        34920 |
|           83845 | MICHEAL             | MAYBERRY           |         9340 |
|           83850 | RUSTY               | MILES              |        47590 |
|           83858 | DAMARIS             | MCQUEEN            |        46350 |
|           83867 | TRACIE              | KELLY              |        41640 |
|           83868 | ROBIN               | CAMPBELL           |        14240 |
|           83870 | MALISSA             | CLARK              |        17880 |
|           83873 | DORTHA              | MAYBERRY           |        24580 |
|           83877 | STEPHAINE           | DUNLAP             |        34590 |
|           83878 | PAT                 | MARIN              |        53170 |
|           83879 | CORNELIA            | FOOTE              |        12580 |
|           83886 | CHRISTINE           | LEBLANC            |        35300 |
|           83893 | SHERRILL            | CHATMAN            |        30840 |
|           83894 | GALE                | DEWITT             |        55820 |
|           83895 | RASHIDA             | MCNEAL             |        46500 |
|           83896 | JAMAAL              | KENNEDY            |        33000 |
|           83901 | ZORA                | WOODALL            |         8620 |
|           83902 | ROCKY               | VARGAS             |        58890 |
|           83903 | LEE                 | CONNOR             |        52220 |
|           83904 | ISREAL              | WHITTAKER          |        29200 |
|           83906 | DELLA               | SIMONS             |        50300 |
|           83907 | IRVING              | SYLVESTER          |        25550 |
|           83910 | LAUREN              | AVERY              |        13500 |
|           83914 | REYNA               | DUKE               |        21700 |
|           83917 | PALMER              | NADEAU             |        11880 |
|           83923 | GERALD              | DOTY               |        32950 |
|           83926 | JAE                 | JIMENEZ            |        34400 |
|           83929 | RETA                | SALTER             |        11160 |
|           83936 | BRADFORD            | BRAY               |        57030 |
|           83941 | TRACIE              | OBRIEN             |        15800 |
|           83943 | TILLIE              | DODD               |        32240 |
|           83948 | SHANNA              | BOOKER             |        26190 |
|           83952 | MONROE              | GOODWIN            |        19890 |
|           83957 | WANDA               | HOLLINGSWORTH      |        29670 |
|           83961 | SHERRILL            | FOOTE              |        36310 |
|           83962 | VALENCIA            | WATTS              |        16190 |
|           83963 | MARVIN              | HAYNES             |        15200 |
|           83964 | HAILEY              | SWEENEY            |        47750 |
|           83973 | EMILY               | GREENWOOD          |        10670 |
|           83977 | JUSTIN              | SHEARER            |        16290 |
|           83978 | KASEY               | CASH               |        33730 |
|           83990 | LACEY               | HINKLE             |        67950 |
|           83991 | PANSY               | BOLTON             |         9290 |
|           83993 | SANG                | CORTES             |        45440 |
|           83995 | KASEY               | CRAFT              |        10040 |
|           83996 | DOTTIE              | WESTON             |         9340 |
|           83998 | EVELIA              | MOCK               |        38220 |
|           84001 | MICHAELA            | FARMER             |        23610 |
|           84005 | ALIDA               | BLACKWELL          |        20850 |
|           84007 | LELA                | PETERS             |         6400 |
|           84009 | KERI                | DUBOIS             |        18770 |
|           84011 | LOUELLA             | ROBERSON           |         8920 |
|           84017 | IGNACIO             | WALDRON            |        28570 |
|           84021 | JAROD               | DICKINSON          |        49200 |
|           84023 | JAROD               | KEYES              |        40970 |
|           84024 | SANJUANITA          | FIELDS             |        99510 |
|           84031 | GAYLE               | PECK               |         8520 |
|           84035 | HAL                 | FISHER             |        23700 |
|           84039 | HANNAH              | COLEMAN            |        47380 |
|           84041 | DEE                 | STILES             |        14850 |
|           84042 | LORRI               | PETTIT             |        38140 |
|           84046 | FRANCESCO           | ELLIOT             |        40570 |
|           84047 | EMMITT              | HOPPER             |        15840 |
|           84049 | LANE                | BRANDON            |        64880 |
|           84052 | RUBY                | FORD               |        23730 |
|           84055 | PHILLIS             | SHAW               |        70990 |
|           84058 | CHRISTOPER          | HAWKINS            |        16480 |
|           84067 | SAMANTHA            | ALBRIGHT           |        74600 |
|           84078 | DIEGO               | ERWIN              |        42160 |
|           84085 | JOSEFA              | MCGHEE             |        24700 |
|           84093 | JUSTIN              | HULL               |        34390 |
|           84094 | YONG                | MCDONALD           |        32550 |
|           84098 | STELLA              | PHELPS             |        56500 |
|           84100 | GILLIAN             | PADGETT            |        20040 |
|           84105 | BERTRAM             | MOCK               |        50840 |
|           84106 | FELICE              | SAMUEL             |        47480 |
|           84110 | BRAIN               | ROY                |        49000 |
|           84114 | CLEMENT             | VOGT               |        10980 |
|           84116 | CARLOTTA            | ASH                |        13600 |
|           84120 | ALANA               | FOREMAN            |        13450 |
|           84123 | MARGOT              | PLATT              |        10960 |
|           84130 | ADA                 | JOYNER             |        12910 |
|           84132 | BRENDAN             | GUERRA             |        12740 |
|           84133 | QUINN               | ROSEN              |        39790 |
|           84134 | ROSALIE             | GARLAND            |        43540 |
|           84156 | ISSAC               | CORTEZ             |        15780 |
|           84162 | GIL                 | OSBORN             |        41630 |
|           84163 | GWEN                | EASLEY             |        38830 |
|           84178 | ALYSON              | WILLARD            |        29230 |
|           84180 | JANNETTE            | CURRIE             |        36310 |
|           84184 | DEANDRE             | MOSLEY             |        32380 |
|           84185 | MADGE               | KANE               |        13480 |
|           84186 | SOFIA               | BARTLETT           |         8750 |
|           84187 | VILMA               | BUNCH              |        27500 |
|           84191 | ROXANA              | HOLBROOK           |        41760 |
|           84193 | ANTOINETTE          | RUIZ               |        40300 |
|           84196 | LELIA               | ALVARADO           |         9650 |
|           84199 | NAN                 | CORNETT            |         9670 |
|           84202 | CLEVELAND           | SAMUELS            |         8690 |
|           84204 | EZEKIEL             | JAMISON            |        52310 |
|           84205 | CESAR               | COLE               |        16810 |
|           84206 | NANCY               | HEALY              |         9960 |
|           84208 | GIL                 | BRUNER             |        10090 |
|           84213 | LES                 | FIELD              |        13920 |
|           84214 | CECELIA             | LACEY              |        40700 |
|           84219 | THURMAN             | WILKINSON          |        40350 |
|           84223 | ANDY                | FARRELL            |        19500 |
|           84224 | ELTON               | LOVE               |        83070 |
|           84233 | ALIDA               | WELCH              |         9990 |
|           84234 | LUISA               | MINER              |        21250 |
|           84235 | JANNETTE            | HARRISON           |        26120 |
|           84236 | ALISHA              | CRUM               |        61830 |
|           84238 | EDNA                | WILEY              |        10520 |
|           84240 | REID                | COLEMAN            |        23080 |
|           84248 | DANICA              | CASTLE             |        44520 |
|           84249 | CORRINA             | BURKE              |        15020 |
|           84251 | MONROE              | KNUTSON            |         8020 |
|           84253 | SADIE               | COVINGTON          |        36230 |
|           84254 | CLINTON             | SEYMOUR            |         8810 |
|           84256 | EDGAR               | DODGE              |        40710 |
|           84259 | CORINA              | CUNNINGHAM         |        38100 |
|           84263 | BRYON               | DANIEL             |        17180 |
|           84264 | MARCOS              | LEVINE             |        26730 |
|           84265 | PAM                 | CASH               |         6530 |
|           84266 | ALANNA              | WISE               |        33520 |
|           84268 | TRISHA              | ALVAREZ            |        61270 |
|           84276 | ROSALIND            | VILLARREAL         |        38770 |
|           84278 | IRENA               | TOBIN              |        26880 |
|           84280 | LIZ                 | AYERS              |        32520 |
|           84286 | PATRICA             | PATE               |        13210 |
|           84287 | FRANK               | PETTY              |        16150 |
|           84291 | ELEANORE            | MACIAS             |         7130 |
|           84294 | KIETH               | MCKENZIE           |        10070 |
|           84298 | JENNA               | LYLES              |         7670 |
|           84300 | ALEXANDRA           | SEAY               |        10880 |
|           84302 | KIP                 | OGDEN              |        16210 |
|           84306 | ROWENA              | MEDINA             |        42290 |
|           84312 | KURTIS              | REDMOND            |        36180 |
|           84316 | BRIDGET             | NIXON              |        22670 |
|           84320 | LUCIO               | CAUDILL            |        26240 |
|           84327 | RUPERT              | STONE              |         9700 |
|           84328 | FERN                | CARPENTER          |        37040 |
|           84329 | MARGOT              | HATFIELD           |        13290 |
|           84330 | DENNA               | CRAFT              |        15270 |
|           84333 | PHILLIS             | CONKLIN            |        48590 |
|           84334 | LINNIE              | GOLDMAN            |        37530 |
|           84340 | LATOYA              | KEENAN             |        51340 |
|           84343 | ELDA                | THORPE             |        20200 |
|           84356 | BERNITA             | CONWAY             |        38930 |
|           84363 | AWILDA              | MORRIS             |        25900 |
|           84364 | LESLIE              | VARNER             |        43540 |
|           84369 | FERNANDO            | GRANGER            |        34760 |
|           84372 | JOSEPHINA           | FAULKNER           |        71940 |
|           84374 | DESSIE              | PAULSON            |        45760 |
|           84380 | ALICIA              | SHEARER            |        12840 |
|           84383 | LUCRETIA            | WASHINGTON         |        17500 |
|           84386 | DELFINA             | RIVERA             |        13900 |
|           84389 | STEFANIE            | DUKE               |         8750 |
|           84392 | ALEJANDRA           | WHALEY             |        38960 |
|           84394 | PARIS               | STONE              |         8100 |
|           84396 | ROBERTA             | NEFF               |        30360 |
|           84397 | SUE                 | ASH                |        28820 |
|           84405 | LAWANDA             | RUCKER             |        27010 |
|           84406 | KAREN               | HINKLE             |         6190 |
|           84417 | ADELA               | CHU                |        15680 |
|           84419 | MITCHELL            | ROLAND             |        27330 |
|           84420 | DOUG                | CAUDILL            |        40090 |
|           84432 | MERLE               | JAMISON            |        53340 |
|           84441 | BURT                | HEALY              |        12930 |
|           84442 | ANGEL               | GREGORY            |        17890 |
|           84448 | WALTON              | BATEMAN            |        11230 |
|           84453 | SANTIAGO            | BRIGGS             |        51000 |
|           84458 | JERRY               | WEIR               |        13140 |
|           84459 | EMILIO              | GILLIAM            |        25750 |
|           84460 | LAVERNE             | TANNER             |         7550 |
|           84462 | ELYSE               | HOPKINS            |         7300 |
|           84463 | LUANNE              | LOVE               |        18320 |
|           84465 | GRAIG               | BLOCK              |        32440 |
|           84476 | FERNE               | BLANKENSHIP        |        10720 |
|           84477 | GALEN               | DUGAN              |        31590 |
|           84484 | MARVIN              | GLOVER             |        50850 |
|           84488 | SHANA               | CORNELL            |        41720 |
|           84494 | NICOLETTE           | FIGUEROA           |        10290 |
|           84500 | CHRISTINE           | WESTON             |        36940 |
|           84501 | DENNA               | MCGRAW             |        47830 |
|           84502 | SAL                 | FITZPATRICK        |        14000 |
|           84506 | AISHA               | ERICKSON           |        77390 |
|           84510 | KRISTINA            | TRUJILLO           |        16080 |
|           84511 | ILEANA              | CHURCH             |       106640 |
|           84515 | TONEY               | TANNER             |        21510 |
|           84520 | ALVA                | NICHOLS            |        49440 |
|           84521 | DELFINA             | JUDD               |        62210 |
|           84526 | FERNE               | LASSITER           |        35210 |
|           84530 | HAL                 | HINKLE             |        70490 |
|           84543 | KYLA                | GOODMAN            |        22630 |
|           84544 | GERTRUDE            | BAUER              |        11940 |
|           84545 | IRENA               | BURKETT            |        45950 |
|           84554 | MICKEY              | CRAFT              |        30340 |
|           84557 | PRUDENCE            | TANNER             |        44740 |
|           84563 | STEPHAINE           | YODER              |        10940 |
|           84564 | WILLY               | OAKES              |        31520 |
|           84565 | CHELSEA             | GAY                |        44330 |
|           84576 | DARELL              | MERCER             |        16170 |
|           84580 | LORENZO             | PECK               |        25200 |
|           84583 | LINDSEY             | YAZZIE             |        44660 |
|           84585 | MARIAM              | HANCOCK            |         7730 |
|           84586 | REYNA               | BERNAL             |         7650 |
|           84587 | LORRI               | SHANNON            |        26860 |
|           84589 | TRICIA              | CONNORS            |        27010 |
|           84591 | LEOPOLDO            | SMALL              |        25430 |
|           84592 | CRISTAL             | CALLAHAN           |        24260 |
|           84593 | TONJA               | PERKINS            |        34160 |
|           84594 | ODELL               | TIDWELL            |        22790 |
|           84596 | PRECIOUS            | FARMER             |        36350 |
|           84597 | WILFORD             | BURGOS             |        23460 |
|           84598 | EDNA                | SARGENT            |       126000 |
|           84599 | ELEANORE            | FENTON             |        33190 |
+-----------------+---------------------+--------------------+--------------+
363 rows in set (0.12 sec)


--3.  Write a query to display the invoice number, line numbers, product SKUs, product descriptions, and brand ID for sales of sealer
--    and top coat products of the same brand on the same invoice. 

select
distinct
l1.inv_num,
l1.line_num,
p1.prod_sku,
p1.prod_descript,
p1.prod_category,
l2.line_num,
p2.prod_sku,
p2.prod_descript,
p2.prod_category,
p1.brand_id
from (line as l1 left join product as p1 on l1.prod_sku = p1.prod_sku)
inner join (line as l2 left join product as p2 on l2.prod_sku = p2.prod_sku)
on l1.inv_num = l2.inv_num
where p1.prod_category = 'Sealer'
and p2.prod_category = 'Top Coat'
and p1.brand_id = p2.brand_id;

Empty set (0.00 sec)

--4.  The Binder Prime Company wants to recognize the employee who sold the most of their products during a specified period. 
--    Write a query to display the employee number, employee first name, employee last name, e-mail address, 
--    and total units sold for the employee who sold the most Binder Prime brand products between November 1, 2015, and December 5, 2015. If there is a tie for most units sold, sort the output by employee last name. 

select
e.emp_num,
e.emp_fname,
e.emp_lname,
e.emp_email,
y.line_tot
from employee as e
left join 
(
    select a.emp_num,sum(b.line_qty) as line_tot
    from invoice as a
    left join line as b
    on a.inv_num = b.inv_num
    left join product as c
    on b.prod_sku = c.prod_sku
    join brand as d
    on c.brand_id = d.brand_id
    where d.brand_name = 'BINDER PRIME'
    and a.inv_date >= '2015-11-01'
    and a.inv_date <= '2015-12-05'
    group by a.emp_num
)y
on e.emp_num = y.emp_num
where y.line_tot = 
(
    select max(x.line_tot) from
(
    select a.emp_num,sum(b.line_qty) as line_tot
    from invoice as a
    left join line as b
    on a.inv_num = b.inv_num
    left join product as c
    on b.prod_sku = c.prod_sku
    join brand as d
    on c.brand_id = d.brand_id
    where d.brand_name = 'BINDER PRIME'
    and a.inv_date >= '2015-11-01'
    and a.inv_date <= '2015-12-05'
    group by a.emp_num
)x
)

Empty set (0.01 sec)

--5.  Write a query to display the customer code, first name, and last name of all customers who have had at 
--    least one invoice completed by employee 83649 and at least one invoice completed by employee 83677. 
--    Sort the output by customer last name and then first name. 

select
a.cust_code,
a.cust_fname,
a.cust_lname,
b.emp_num
from customer as a
left join invoice as b
on a.cust_code = b.cust_code
where a.cust_code in
(select cust_code from invoice where emp_num = 83649)
and b.emp_num = 83677;

Empty set (0.00 sec)

--6.  LargeCo is planning a new promotion in Alabama (AL) and wants to know about the largest purchases made by customers in that state. 
--    Write a query to display the customer code, customer first name, last name, full address, invoice date, and invoice total of the largest
--    purchase made by each customer in Alabama. Be certain to include any customers in Alabama who have never made a purchase 
--    (their invoice dates should be NULL and the invoice totals should display as 0). 

select
a.cust_code,
a.cust_fname,
a.cust_lname,
concat(a.cust_street,', ',a.cust_city,', ',a.cust_state,', ',a.cust_zip) as cust_address,
b.inv_date,
b.inv_total
from customer as a
left join invoice as b
on a.cust_code = b.cust_code
where a.cust_state = 'AL'
and b.inv_total = (select max(inv_total) from invoice as c where c.cust_code = a.cust_code)
union
select
d.cust_code,
d.cust_fname,
d.cust_lname,
concat(d.cust_street,', ',d.cust_city,', ',d.cust_state,', ',d.cust_zip) as cust_address,
NULL,
0
from customer as d
left join invoice as e
on d.cust_code = e.cust_code
where d.cust_state = 'AL'
and d.cust_code not in (select cust_code from invoice);

+-----------+------------+------------+-----------------------------------------------------+---------------------+-----------+
| cust_code | cust_fname | cust_lname | cust_address                                        | inv_date            | inv_total |
+-----------+------------+------------+-----------------------------------------------------+---------------------+-----------+
|        89 | MONICA     | CANTRELL   | 697 ADAK CIRCLE, Loachapoka, AL, 36865              | 2014-01-14 00:00:00 |       314 |
|       152 | LISETTE    | WHITTAKER  | 339 NORTHPARK DRIVE, Montgomery, AL, 36197          | 2013-11-21 00:00:00 |       139 |
|       169 | ROSS       | LANG       | 1991 EASTWIND COURT, Higdon, AL, 35979              | 2013-11-16 00:00:00 |       177 |
|       188 | LUANNE     | GOODWIN    | 293 KIANA AVENUE, Pinegrove, AL, 36507              | 2013-08-13 00:00:00 |       202 |
|       218 | LUPE       | SANTANA    | 1292 WEST 70TH PLACE, Phenix City, AL, 36867        | 2013-11-02 00:00:00 |       270 |
|       219 | CATHI      | WHITEHEAD  | 760 WOODCLIFF DRIVE, Huntsville, AL, 35893          | 2013-11-21 00:00:00 |       273 |
|       286 | JEANNE     | STEINER    | 1974 SCHUSS DRIVE, Carrollton, AL, 35447            | 2013-03-22 00:00:00 |       226 |
|       295 | DORTHY     | AUSTIN     | 829 BIG BEND LOOP, Diamond Shamrock, AL, 36614      | 2013-11-03 00:00:00 |        87 |
|       304 | GERTRUDE   | CONNORS    | 1042 PLEASANT DRIVE, Georgiana, AL, 36033           | 2013-12-31 00:00:00 |       376 |
|       364 | DELLA      | MAYO       | 543 STELIOS CIRCLE, Birmingham, AL, 35214           | 2013-07-10 00:00:00 |        95 |
|       367 | HOLLIS     | STILES     | 1493 DOLLY MADISON CIRCLE, Snow Hill, AL, 36778     | 2013-07-19 00:00:00 |       312 |
|       380 | ALBINA     | ENGLE      | 670 UPPER BOWERY LANE, Clanton, AL, 35045           | 2013-04-14 00:00:00 |       124 |
|       416 | TATIANA    | HOWE       | 1650 ALL STAR CIRCLE, Sunny South, AL, 36769        | 2013-09-07 00:00:00 |        76 |
|       458 | ELOISA     | VALLE      | 182 BRANDON STREET, Abel, AL, 36258                 | 2013-12-21 00:00:00 |       225 |
|       487 | ROSENDO    | REYNA      | 1009 HAMPSHIRE BOULEVARD, Gunter Afs-Eci, AL, 36118 | 2013-12-03 00:00:00 |        41 |
|       538 | CHIQUITA   | CALDWELL   | 1501 BRIGGS COURT, Normal, AL, 35762                | 2013-05-28 00:00:00 |       144 |
|       585 | LORRAINE   | HANNAH     | 173 LAUREL STREET, Orange Beach, AL, 36561          | 2014-01-02 00:00:00 |       114 |
|       643 | NINA       | ALLEN      | 680 RED TALON DRIVE, Robertsdale, AL, 36574         | 2013-06-23 00:00:00 |        12 |
|       696 | ALISHA     | TOMLINSON  | 1985 EAST 52ND AVENUE, Catherine, AL, 36728         | 2013-07-09 00:00:00 |        38 |
|       738 | ALIDA      | HANSEN     | 792 FERGY CIRCLE, Furman, AL, 36741                 | 2013-08-04 00:00:00 |       111 |
|       753 | CECIL      | ESPARZA    | 1928 VALLEY VISTA CIRCLE, Mulga, AL, 35118          | 2013-07-21 00:00:00 |       180 |
|       780 | LARISSA    | POOL       | 574 ADAK CIRCLE, Decatur, AL, 35609                 | 2014-01-08 00:00:00 |       243 |
|       798 | LETICIA    | HEBERT     | 1244 DEER PARK DRIVE, Shorterville, AL, 36373       | 2013-07-31 00:00:00 |        41 |
|       820 | MARCELA    | DUGAN      | 1785 DORIS PLACE, Sylacauga, AL, 35150              | 2013-08-06 00:00:00 |       195 |
|       853 | GAYLORD    | BOLTON     | 1069 LUGENE LANE, Montgomery, AL, 36131             | 2013-11-27 00:00:00 |       373 |
|       855 | AUBREY     | GLOVER     | 907 GOLD CLAIM DRIVE, Honoraville, AL, 36042        | 2013-08-17 00:00:00 |       300 |
|       881 | LIZ        | MCQUEEN    | 1203 VALLEY STREET, Brewton, AL, 36426              | 2013-08-28 00:00:00 |       318 |
|       886 | ROSARIO    | STOKES     | 959 SUNRISE DRIVE, Hightower, AL, 36263             | 2013-08-25 00:00:00 |       120 |
|       903 | ROBIN      | ADDISON    | 323 LORETTA PLACE, Mobile, AL, 36693                | 2013-10-26 00:00:00 |       129 |
|       915 | CARLOTTA   | KNIGHT     | 335 HESTERBERG ROAD, Winslow, AL, 36003             | 2013-11-16 00:00:00 |       213 |
|       925 | ALANA      | BOOKER     | 1874 I STREET, Mccullough, AL, 36502                | 2013-12-14 00:00:00 |       209 |
|       931 | PANSY      | MAYBERRY   | 728 LYNKERRY CIRCLE, Enterprise, AL, 36330          | 2013-09-12 00:00:00 |        79 |
|       979 | IMOGENE    | MAYES      | 1017 HARCA STREET, Sylacauga, AL, 35150             | 2013-09-17 00:00:00 |       246 |
|      1029 | JOHNETTA   | ROY        | 1163 GIROUX CIRCLE, Dauphin Island, AL, 36528       | 2013-09-30 00:00:00 |       225 |
|      1068 | ELIZA      | CURRIE     | 778 LOUDERMILK CIRCLE, Panola, AL, 35477            | 2013-10-22 00:00:00 |       250 |
|      1100 | ELEANORE   | SAUNDERS   | 820 QUARTZ AVENUE, Silver Cross, AL, 36538          | 2013-10-20 00:00:00 |       326 |
|      1131 | CARMA      | CORNETT    | 767 CHISANA WAY, Killen, AL, 35645                  | 2014-01-16 00:00:00 |       238 |
|      1172 | ADELE      | PERKINS    | 1192 RICHARDSON VISTA ROAD, Sylacauga, AL, 35150    | 2013-12-21 00:00:00 |       194 |
|      1233 | NATHALIE   | CHURCH     | 1802 SNOWY OWL CIRCLE, Napier Field, AL, 36303      | 2013-11-26 00:00:00 |       161 |
|      1248 | LISA       | BRADY      | 491 LOWLAND AVENUE, Daphne, AL, 36577               | 2014-01-10 00:00:00 |       372 |
|      1253 | TRENTON    | PRESLEY    | 1905 ANCHOR PARK CIRCLE, Montgomery, AL, 36131      | 2013-11-30 00:00:00 |       349 |
|      1264 | MICHAELA   | RICHARD    | 44 RASMUSSON STREET, Georgetown, AL, 36521          | 2013-12-04 00:00:00 |       144 |
|      1275 | ELEANORE   | NEFF       | 556 CACHE DRIVE, Saint Stephens, AL, 36569          | 2013-12-08 00:00:00 |       429 |
|      1350 | LATONYA    | KAY        | 61 LOUSSAC DRIVE, Seaboard, AL, 36529               | 2013-12-26 00:00:00 |       275 |
|      1407 | FELICIA    | CRUZ       | 643 TURNAGAIN PARKWAY, Coalburg, AL, 35068          | 2014-01-08 00:00:00 |       388 |
|      1412 | EVALYN     | HEWITT     | 293 TIMOTHY CIRCLE, Tallassee, AL, 36078            | 2014-01-09 00:00:00 |       201 |
|      1439 | SHANNA     | PRITCHARD  | 1448 ARLENE DRIVE, Lavaca, AL, 36911                | 2014-01-16 00:00:00 |        72 |
|      1440 | LONNIE     | MANNING    | 644 PARKER PLACE, Big Cove, AL, 35763               | 2014-01-17 00:00:00 |       378 |
|      1443 | ALYSON     | SELF       | 772 LUPIN DRIVE, Motley, AL, 36276                  | 2014-01-18 00:00:00 |       115 |
|       393 | FOSTER     | BERNAL     | 1299 EAST 3RD AVENUE, Birmingham, AL, 35280         | NULL                |         0 |
+-----------+------------+------------+-----------------------------------------------------+---------------------+-----------+
50 rows in set (0.01 sec)

--7.  One of the purchasing managers is interested in the impact of product prices on the sale of products of each brand.
--    Write a query to display the brand name, brand type, average price of products of each brand, and total units sold of products of each brand.
--    Even if a product has been sold more than once, its price should only be included once in the calculation of the average price.
--    However, you must be careful because multiple products of the same brand can have the same price, and each of those products must be included in the calculation of the brand’s average price.

select
a.brand_name,
a.brand_type,
x.avg_price,
y.total_sold
from brand as a
left join 
(
    select brand_id,avg(prod_price) as avg_price 
    from product
    group by brand_id
)x
on a.brand_id = x.brand_id
left join
(
    select brand_id,sum(line_qty) as total_sold
    from product as b
    left join line as c
    on b.prod_sku = c.prod_sku
    group by b.brand_id
)y
on a.brand_id = y.brand_id
where y.total_sold is not null;

+-------------------+------------+-----------+------------+
| brand_name        | brand_type | avg_price | total_sold |
+-------------------+------------+-----------+------------+
| FORESTERS BEST    | VALUE      |   21.0000 |        221 |
| STUTTENFURST      | CONTRACTOR |   16.4815 |        385 |
| HOME COMFORT      | CONTRACTOR |   21.8889 |        466 |
| OLDE TYME QUALITY | CONTRACTOR |   18.4074 |        398 |
| BUSTERS           | VALUE      |   22.6800 |        479 |
| LONG HAUL         | CONTRACTOR |   20.2195 |        665 |
| VALU-MATTE        | VALUE      |   16.8333 |        312 |
| BINDER PRIME      | PREMIUM    |   16.1481 |        377 |
| LE MODE           | PREMIUM    |   19.2500 |        561 |
+-------------------+------------+-----------+------------+
3 rows in set (0.00 sec)

--8.  The purchasing manager is still concerned about the impact of price on sales. 
--    Write a query to display the brand name, brand type, product SKU, product description, 
--    and price of any products that are not a premium brand, but that cost more than the most expensive premium brand products.

select
a.brand_name,
a.brand_type,
b.prod_sku,
b.prod_descript,
b.prod_price
from brand as a
left join product as b
on a.brand_id = b.brand_id
where a.brand_type <> 'PREMIUM'
and b.prod_price > 
(
    select max(d.prod_price)
    from brand as c
    left join product as d
    on c.brand_id = d.brand_id
    where c.brand_type = 'PREMIUM'
);

+------------+------------+----------+--------------------------------------------+------------+
| brand_name | brand_type | prod_sku | prod_descript                              | prod_price |
+------------+------------+----------+--------------------------------------------+------------+
| LONG HAUL  | CONTRACTOR | 1964-OUT | Fire Resistant Top Coat, for Interior Wood |         78 |
+------------+------------+----------+--------------------------------------------+------------+
1 row in set (0.00 sec)

--9.  Using SQL descriptive statistics functions calculate the value of the following items: 
--    a. What are the products that have a price greater than $50?

select * from product where prod_price > 50;

+----------+----------------------------------------------------------------+-----------+-----------+---------------+------------+----------+----------+----------+
| prod_sku | prod_descript                                                  | prod_type | prod_base | prod_category | prod_price | prod_qoh | prod_min | brand_id |
+----------+----------------------------------------------------------------+-----------+-----------+---------------+------------+----------+----------+----------+
| 1021-MTI | Elastomeric, Exterior, Industrial Grade, Water Based           | Exterior  | Water     | Top Coat      |         63 |       22 |       25 |       35 |
| 1964-OUT | Fire Resistant Top Coat, for Interior Wood                     | Interior  | Solvent   | Top Coat      |         78 |      120 |       10 |       30 |
| 3694-XFJ | Epoxy-Modified Latex, Interior, Semi-Gloss (MPI Gloss Level 5) | Interior  | Water     | Top Coat      |         55 |       39 |       25 |       27 |
+----------+----------------------------------------------------------------+-----------+-----------+---------------+------------+----------+----------+----------+
3 rows in set (0.00 sec)

--    b. What is total value of our entire inventory on hand?

select sum(prod_qoh*prod_price) as total_inventory_cost from product;

+----------------------+
| total_inventory_cost |
+----------------------+
|               359198 |
+----------------------+
1 row in set (0.00 sec)

--    c. How many customers do we presently have and what is the total of all customer balances?

select count(*) as no_of_customers from customer;
+-----------------+
| no_of_customers |
+-----------------+
|            1362 |
+-----------------+
1 row in set (0.00 sec)

select sum(cust_balance) as total_customer_balance from customer;
+------------------------+
| total_customer_balance |
+------------------------+
|                 787211 |
+------------------------+
1 row in set (0.00 sec)

--    d. What are to top three states that buy the most product in dollars from the company?

select
b.cust_state,
sum(a.inv_total)
from invoice as a
left join customer as b
on a.cust_code = b.cust_code
group by b.cust_state
order by sum(a.inv_total) desc
limit 3;

+------------+------------------+
| cust_state | sum(a.inv_total) |
+------------+------------------+
| PA         |            37893 |
| NY         |            31979 |
| NC         |            19305 |
+------------+------------------+
3 rows in set (0.04 sec)






mysql> select prod_sku from line where prod_sku not in (select prod_sku from product);
+----------+
| prod_sku |
+----------+
| 5379-BLX |
| 3393-AZQ |
| 6358-UST |
| 6358-UST |
| 3393-AZQ |
| 2233-GJH |
| 5379-BLX |
| 5379-BLX |
| 3393-AZQ |
| 2233-GJH |
| 2233-GJH |
| 5379-BLX |
| 2233-GJH |
| 5379-BLX |
| 2233-GJH |
| 2233-GJH |
| 5379-BLX |
| 6358-UST |
| 6358-UST |
| 6358-UST |
| 3393-AZQ |
| 5379-BLX |
| 6358-UST |
| 3393-AZQ |
| 5379-BLX |
+----------+
25 rows in set (0.00 sec)