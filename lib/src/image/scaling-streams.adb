
with Endian;
with File.Misc; -- Write_Padding needed

package body Scaling.Streams is



-- Read from file
procedure Linear(Ssrc : SIO.Stream_Access; Aout : out Tdst.Numeric_Arr)
is
    Ain : Tsrc.Numeric_Arr(Aout'Range);
    procedure CheckAndRevert is new Endian.Check_And_Revert(Tsrc.Numeric,Tsrc.Numeric_Arr);
begin
    Tsrc.Numeric_Arr'Read(Ssrc, Ain);
    CheckAndRevert(Ain);
    Linear(Ain,Aout);
end Linear;



-- Write to file
procedure Linear(Ain : in Tsrc.Numeric_Arr; Sdst : SIO.Stream_Access)
is
    Aout : Tdst.Numeric_Arr(Ain'Range);
    procedure CheckAndRevert is new Endian.Check_And_Revert(Tdst.Numeric,Tdst.Numeric_Arr);
begin
    Linear(Ain,Aout);
    CheckAndRevert(Aout);
    Tdst.Numeric_Arr'Write(Sdst, Aout);
end Linear;



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


-- FIXME both miss Undefed handling and scaling (set A,B)

-- src : file
-- dst : array in memory
procedure Read_Data_Unit
  (F : SIO.File_Type;
  NAXISn : in NAXIS_Arr)
is
    DULength : Positive_Count := DU_Data_Count(NAXISn);
    E : Tsrc.Numeric;
begin
  for I in 1 .. DULength
  loop
    Tsrc.Numeric'Read(SIO.Stream(F), E);
    Elem(Linear(E));
  end loop;
end Read_Data_Unit;



-- src : array in memory
-- dst : file
procedure Write_Data_Unit
  (F : SIO.File_Type;
  NAXISn : in NAXIS_Arr)
is
    DULength : Positive_Count := DU_Data_Count(NAXISn);
    E : Tsrc.Numeric;
begin
  for I in 1 .. DULength
  loop
    Elem(E);
    Tdst.Numeric'Write(SIO.Stream(F), Linear(E));
  end loop;

  File.Misc.Write_Padding(F,SIO.Index(F),File.Misc.DataPadValue);

end Write_Data_Unit;








end Scaling.Streams;

