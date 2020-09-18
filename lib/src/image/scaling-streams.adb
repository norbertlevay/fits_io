
with Endian;


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



end Scaling.Streams;

