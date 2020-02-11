

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





 -- read Volume of N-dimensions



 function DU_Length_elems(NAXISn : Coord_Type) return Natural
 is
  Len : Positive := 1;
 begin

  for I in NAXISn'Range
  loop
    Len := Len * Positive(NAXISn(I));
  end loop;
  return Len;
 end DU_Length_elems;
 -- DU length in count of elements



 procedure Read_Valid_Scaled_Volume
                (File : SIO.File_Type; 
                BZERO  : in Tout;
                BSCALE : in Tout;
                Undef_Val : in Tout; 
                DUStart   : in Positive;
                MaxCoords : in Coord_Type;-- NAXISn
                First  : in Coord_Type;
                Last   : in Coord_Type;
                Volume : out Tout_Arr)
 is
   procedure Read_One_Line
	is new Read_Valid_Scaled_Line(T,Tout,Tout_Arr, Is_Valid, "+","*","+");

   LineLength : Positive := 1 + Integer(Last(1) - First(1)); 
   Line: Tout_Arr(1 .. LineLength);

--   VolumeLength : Positive := DU_Length_elems(MaxCoords);
--   Volume : Tout_Arr(1 .. VolumeLength);

   -- generate coords vars
   Winit : FInteger := 2;
   W : FInteger;
--   F :  Coord_Arr := (3,1,7);
--   L :  Coord_Arr := (5,2,9);
   C : Coord_Type := First;  -- Current coords
   Vf, Vl : Positive;
   Unity : constant Coord_Type(First'Range) := (others => 1);
   VolMaxCoords : Coord_Type(First'Range);
 begin

 for I in First'Range loop
   VolMaxCoords(I) := Unity(I) + Last(I) - First(I);
 end loop;


  C := First;
  W := Winit;

  --print_coord(C)  
  Read_One_Line(File,BZERO,BSCALE, Undef_Val,DUStart,   
		MaxCoords, C, LineLength, Line);

  Vf := Integer(To_Offset(C,VolMaxCoords));
  Vl := Vf; 
  Vl := Vf + LineLength - 1;
  Volume(Vf .. Vl) := Line;
  -- store read line
 
  Outer_Loop:
  loop

    loop

        if( C(W) = Last(W) )
        then
         C(W) := First(W);
         W := W + 1;
         exit Outer_Loop when ( W > Last'Last );
        else
         C(W) := C(W) + 1;
         W := Winit;
         exit;
        end if;

    end loop;

   -- print_coord(C);
   Read_One_Line(File,BZERO,BSCALE, Undef_Val,DUStart,   
 		MaxCoords, C, LineLength, Line);
 
   Vf := Integer(To_Offset(C,VolMaxCoords));
   Vl := Vf; 
   Vl := Vf + LineLength - 1;
   Volume(Vf .. Vl) := Line;
  -- store read line

  end loop Outer_Loop;

 end Read_Valid_Scaled_Volume;


end NCube;

