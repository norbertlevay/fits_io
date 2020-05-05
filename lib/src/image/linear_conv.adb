


package body Linear_Conv is


function FF(A,B:Tc; Vin : Tin) return Tout is begin return Tout(A+B*Tc(Vin)); end FF;
function FI(A,B:Tc; Vin : Tin) return Tout is begin return Tout(A+B*Tc(Vin)); end FI;




end Linear_Conv;
