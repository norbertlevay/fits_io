
with Endian;
with File.Misc; -- Write_Padding needed
with Scaling;

package body Numeric_Type.Stream is



procedure Read(S : SIO.Stream_Access; A : out Float_Arr)
is
    Abuf : Numeric_Arr(A'Range);
    procedure CheckAndRevert is new Endian.Check_And_Revert(Numeric,Numeric_Arr);
begin
    Numeric_Arr'Read(S, Abuf);
    CheckAndRevert(Abuf);
    A := To_Float(Abuf);
end Read;



procedure Write(S : SIO.Stream_Access; A : in Float_Arr)
is
    Abuf : Numeric_Arr(A'Range);
    procedure CheckAndRevert is new Endian.Check_And_Revert(Numeric,Numeric_Arr);
begin
    Abuf := To_Numeric(A);
    CheckAndRevert(Abuf);
    Numeric_Arr'Write(S, Abuf);
end Write;



-- Data Unit access



function DU_Data_Count(NAXISn : in NAXIS_Arr) return Positive_Count
is
  Acc : Positive_Count := 1;
begin
  for I in NAXISn'Range
  loop
    Acc := Acc * NAXISn(I);
  end loop;
  return Acc;
end DU_Data_Count;



procedure Read_Data_Unit
  (F : SIO.File_Type;
  NAXISn : in NAXIS_Arr)
is
    DULength : Positive_Count := DU_Data_Count(NAXISn);
    E : Numeric;
begin
  for I in 1 .. DULength
  loop
    Numeric'Read(SIO.Stream(F), E);
    Elem(To_Float(E)); -- NOTE or Elem(E) <-- Raw-data access
  end loop;
end Read_Data_Unit;



procedure Write_Data_Unit
  (F : SIO.File_Type;
  NAXISn : in NAXIS_Arr)
is
    DULength : Positive_Count := DU_Data_Count(NAXISn);
    E : Float;
begin
  for I in 1 .. DULength
  loop
    Elem(E);
    Numeric'Write(SIO.Stream(F), To_Numeric(E));
  end loop;

  File.Misc.Write_Padding(F,SIO.Index(F),File.Misc.DataPadValue);

end Write_Data_Unit;


end Numeric_Type.Stream;

