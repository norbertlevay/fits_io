
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with Data_Unit;
with Data_Funcs;    use Data_Funcs;
with Mandatory;     use Mandatory; -- Positive_Arr needed
with Keyword_Record; use Keyword_Record; -- FPositive needed in Positive_Arr for operators visibilití

with NCube_Funcs; use NCube_Funcs;

package body NCube is

use SIO;

 procedure Read_Valid_Scaled_Line
   (F : SIO.File_Type; 
    BZERO  : in Tout;
    BSCALE : in Tout;
    Undef_Val : in Tout;
    DUStart   : in Positive_Count;
    MaxCoords : in Positive_Arr; -- NAXIS1, NAXIS2... NAXISn 
    First  : in Positive_Arr;
    Length : in Positive_Count; -- may be at most NAXIS1
    Values : out Tout_Arr)
 is
  Offset    : Positive_Count := Positive_Count(To_Offset(First, MaxCoords));-- FIXME 
  DUBlockIx : Positive_Count := DU_Block_Index(Offset, Tout'Size/8);
  OffsetInBlock : Positive := Offset_In_Block(Offset, 2880/(Tout'Size/8));

  -- instantiate sequential reader
  Ix : Positive_Count := 1;

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

  package DU   is new Data_Unit(T);
  package Phys is new DU.Physical(Tout);
  use Phys;
  procedure Read_Valid_Scaled_Vals
    is new Phys.Read_Valid_Scaled_Values(cbValue, Is_Valid , cbInvalid);

 begin

  Set_File_Block_Index(F, DUStart + DUBlockIx - 1);

  Read_Valid_Scaled_Vals(F, Length, BZERO, BSCALE, Undef_Val, OffsetInBlock);  

 end Read_Valid_Scaled_Line;





 -- read Volume of N-dimensions



 function DU_Length_elems(NAXISn : Positive_Arr) return Natural
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
                DUStart   : in Positive_Count;
                MaxCoords : in Positive_Arr;-- NAXISn
                First  : in Positive_Arr;
                Last   : in Positive_Arr;
                Volume : out Tout_Arr)
 is
   procedure Read_One_Line
    is new Read_Valid_Scaled_Line(T,Tout,Tout_Arr, Is_Valid, "+","*","+");

   LineLength : Positive_Count := 1 + Positive_Count(Last(1) - First(1)); -- FIXME FInteger
   Line: Tout_Arr(1 .. LineLength);

   -- generate coords vars
   Winit : FIndex := 2;
   W : FIndex;
   C  : Positive_Arr := First;  -- Current coords in source Data Unit
   CV : Positive_Arr := First;  -- Current coords in target Volume
   Vf, Vl : Positive_Count;
   Unity : constant Positive_Arr(First'Range) := (others => 1);
   VolMaxCoords : Positive_Arr(First'Range);
 begin

 for I in First'Range loop
   VolMaxCoords(I) := Unity(I) + Last(I) - First(I);
 end loop;

  W := Winit;
  C := First;
  for I in First'Range loop
   CV(I) := Unity(I) + C(I) - First(I);
  end loop;


  --print_coord(C)  
  Read_One_Line(File,BZERO,BSCALE, Undef_Val,DUStart,
        MaxCoords, C, LineLength, Line);

  Vf := Positive_Count(To_Offset(CV,VolMaxCoords)); -- FIXME FInteger
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

   for I in First'Range loop
    CV(I) := Unity(I) + C(I) - First(I);
   end loop;

   Vf := Positive_Count(To_Offset(CV,VolMaxCoords));-- FIXME FInteger
   Vl := Vf + LineLength - 1;
   Volume(Vf .. Vl) := Line;
  -- store read line

  end loop Outer_Loop;

 end Read_Valid_Scaled_Volume;


end NCube;

