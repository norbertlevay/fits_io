

with Generic_Data_Unit;
with NCube_Funcs; use NCube_Funcs;
with Data_Funcs; use Data_Funcs;

with Keyword_Record; use Keyword_Record; -- FPositive needed

with Generic_Data_Unit;

package body NCube is

 procedure Read_Valid_Scaled_Line
                (F : SIO.File_Type; 
                BZERO  : in Tout;
                BSCALE : in Tout;
                Undef_Val : in Tout;
		DUStart   : in Positive;   
		MaxCoords : in Coord_Type; -- NAXIS1, NAXIS2... NAXISn 
                First  : in Coord_Type;
                Length : in Positive; -- may be at most NAXIS1
		Values : out Tout_Arr)
 is
  Offset    : Positive := Positive(To_Offset(First, MaxCoords)); 
  DUBlockIx : Positive := DU_Block_Index(Offset, Tout'Size/8);
  OffsetInBlock : Positive := Offset_In_Block(Offset, 2880/(Tout'Size/8));

  -- instantiate sequential reader
  Ix : Positive := 1;

  procedure cbValue(V : in Tout) 
  is
  begin
   Values(Ix) := V;
   Ix := Ix + 1;
  end cbValue;

  procedure cbInvalid
  is
  begin
   Values(Ix) := Undef_Val;
   Ix := Ix + 1;
  end cbInvalid;

  package Data_Unit is new Generic_Data_Unit(T);
  package Phys      is new Data_Unit.Physical(Tout);
  use Phys;
  procedure Read_Valid_Scaled_Vals
	is new Phys.Read_Valid_Scaled_Values(cbValue, Is_Valid , cbInvalid);

  --UndefVal : Tout;
 begin

  Set_File_Block_Index(F, DUStart + DUBlockIx);

  Read_Valid_Scaled_Vals(F, Length, BZERO, BSCALE, Undef_Val, OffsetInBlock);  

 end Read_Valid_Scaled_Line;


end NCube;
