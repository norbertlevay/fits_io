
with Endian;


package body Scaling.Streams is



-- Read from file
procedure Linear(Ssrc : SIO.Stream_Access; Aout : out Tdst_Numeric_Arr)
is
    Ain : Tsrc_Numeric_Arr(Aout'Range);
    procedure CheckAndRevert is new Endian.Check_And_Revert(Tsrc.Numeric,Tsrc_Numeric_Arr);
begin
    Tsrc_Numeric_Arr'Read(Ssrc, Ain);
    CheckAndRevert(Ain);
    Linear(Ain,Aout);
end Linear;



-- Write to file
procedure Linear(Ain : in Tsrc_Numeric_Arr; Sdst : SIO.Stream_Access)
is
    Aout : Tdst_Numeric_Arr(Ain'Range);
    procedure CheckAndRevert is new Endian.Check_And_Revert(Tdst.Numeric,Tdst_Numeric_Arr);
begin
    Linear(Ain,Aout);
    CheckAndRevert(Aout);
    Tdst_Numeric_Arr'Write(Sdst, Aout);
end Linear;



end Scaling.Streams;

