
with Endian;
with File.Misc; -- Write_Padding needed


-- FIXME actually: plug-in here the Raw-module: deals only with one type


package body Numeric_Type.Data_IO is



procedure Read(F : SIO.File_Type; A : out Float_Arr)
is
    Abuf : Numeric_Arr(A'Range);
    procedure CheckAndRevert is new Endian.Check_And_Revert(Numeric,Numeric_Arr);
begin
    Numeric_Arr'Read(SIO.Stream(F), Abuf);
    CheckAndRevert(Abuf);
    A := To_Float(Abuf);
end Read;



procedure Write(F : SIO.File_Type; A : in Float_Arr)
is
    Abuf : Numeric_Arr(A'Range);
    procedure CheckAndRevert is new Endian.Check_And_Revert(Numeric,Numeric_Arr);
begin
    Abuf := To_Numeric(A);
    CheckAndRevert(Abuf);
    Numeric_Arr'Write(SIO.Stream(F), Abuf);
end Write;



-- Data Unit access



function DU_Data_Count(NAXISn : in NAXIS_Array) return Positive_Count
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
  NAXISn : in NAXIS_Array)
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
  NAXISn : in NAXIS_Array)
is
    DULength : Positive_Count := DU_Data_Count(NAXISn);
    E : Float;
begin
  for I in 1 .. DULength
  loop
    Elem(E);
    Numeric'Write(SIO.Stream(F), To_Numeric(E));
  end loop;

  --File.Misc.Write_Padding(F,SIO.Index(F),File.Misc.DataPadValue);

end Write_Data_Unit;


end Numeric_Type.Data_IO;

