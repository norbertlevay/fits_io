

generic
type Tm is private;
type Tf is private;
with function Linear(Vin : in Tf; A,B:Tm) return Tm is <>; --forRead,forWrite swap Tf<->Tm
package Physical is

procedure Read_Array(dummy : Integer);

end Physical;
