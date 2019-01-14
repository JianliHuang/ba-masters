data a1;infile 'C:\Users\murthi\Documents\Teaching\MAS6V05-SAS\dat files\mfund.dat' firstobs=3;
input share return ratings beta expense time size;run;

proc reg;model share=return ratings beta expense time size/vif stb;run;

proc model data=a1;
parms b0 b1 b2 b3 b4 b5 b6;
share=b0+b1*return+ b2*ratings+ b3*beta+ b4*expense+ b5*time+ b6*size;
fit share / white out=resid1 outresid;run;

proc model data=a1;
parms b0 b1 b2 b3 b4 b5 b6;
size_inv=1/size;
share=b0+b1*return+ b2*ratings+ b3*beta+ b4*expense+ b5*time+ b6*size;
fit share / white ;
weight size_inv;
run;

data a2;set a1;
ksh=share/size;
kret=return/size;
krat=ratings/size;
kexp=expense/size;
ktime=time/size;
kbeta=beta/size;
ksi=1/size;
run;
proc model data=a2;
parms b0 b1 b2 b3 b4 b5 b6;
ksh=b6+b1*kret+ b2*krat+ b3*kbeta+ b4*kexp+ b5*ktime+ b0*ksi;
fit ksh / white ;run;

proc model data=a1;
parms b0 b1 b2 b3 b4 b5 b6;
size_inv=1/(size*size);
share=b0+b1*return+ b2*ratings+ b3*beta+ b4*expense+ b5*time+ b6*size;
fit share / white ;
weight size_inv;
run;



