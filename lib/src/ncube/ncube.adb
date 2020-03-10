
with Ada.Text_IO;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;
with Interfaces;

with Data_Unit;
with Unit;
with Data_Funcs;    use Data_Funcs;
with Mandatory;     use Mandatory; -- NAXIS_Arr needed
with Keyword_Record; use Keyword_Record; -- FIndex needed in NAXIS_Arr

with NCube_Funcs; use NCube_Funcs;

package body NCube is

use SIO;

 package TIO renames Ada.Text_IO;

-- sequential access

-- if T_Arr'Length = NAXISi*NAXISi-1*...*NAXIS1
-- then repeating Read (NAXISn*NAXISn-1*...NAXISi+1)-times 
-- reads all DU sequentially
-- NOTE position to DUStart before 1st call
 procedure Read_Raw_Plane
   (F : SIO.File_Type;
    Plane  : out T_Arr)
 is
  --procedure ReadPlane is new Unit.Read_Array_From_Current_Block(T, T_Arr);
 begin
  --ReadPlane(F, Plane, 1);-- First=1: always read from beginng of block
    T_Arr'Read(SIO.Stream(F),Plane);
 end Read_Raw_Plane;


 -- endianness

   generic
        type T is private;
   procedure Revert_Bytes( Data : in out T );
   procedure Revert_Bytes( Data : in out T )
   is
     Size_Bytes : Positive := T'Size / Interfaces.Unsigned_8'Size;
     type Arr4xU8 is array (1..Size_Bytes) of Interfaces.Unsigned_8;

     function Data_To_Arr is
       new Ada.Unchecked_Conversion(Source => T, Target => Arr4xU8);
     function Arr_To_Data is
       new Ada.Unchecked_Conversion(Source => Arr4xU8, Target => T);

     Arr  : Arr4xU8 := Data_To_Arr(Data);
     ArrO : Arr4xU8;
   begin

     for I in Arr'Range
     loop
       ArrO(I) := Arr(1 + Size_Bytes - I); 
     end loop;

     Data := Arr_To_Data(ArrO);

   end Revert_Bytes;




procedure Read_Plane
   (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
    Length : in Positive_Count;
    Plane  : out Tm_Arr)
is
    type Tf_Arr is array (Positive_Count range <>) of Tf;
    procedure ReadRawPlane is new Read_Raw_Plane(Tf,Tf_Arr);
    function LinScale is new Unit.Scale(Tf,Tm,Tc, BZERO, BSCALE,"+","+");
    RawPlane : Tf_Arr(1..Length);
    procedure RevertBytes is new Revert_Bytes(Tf);
begin

    ReadRawPlane(F, RawPlane);

    for I in RawPlane'Range
    loop
        RevertBytes(RawPlane(I));
        Plane(I) := LinScale(RawPlane(I));
    end loop;


end Read_Plane;





 procedure Read_Float_Plane
   (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
    Length : in Positive_Count;
    Plane  : out Tm_Arr)
is
    type Tf_Arr is array (Positive_Count range <>) of Tf;
    procedure ReadRawPlane is new Read_Raw_Plane(Tf,Tf_Arr);
    function LinFloatScale is new Unit.Scale_Float(Tf,Tm,Tc, BZERO, BSCALE, Undef_Val, "+","+");
    RawPlane : Tf_Arr(1..Length);
    procedure RevertBytes is new Revert_Bytes(Tf);
begin

    ReadRawPlane(F, RawPlane);

    for I in RawPlane'Range
    loop
        RevertBytes(RawPlane(I));
        Plane(I) := LinFloatScale(RawPlane(I));
    end loop;

    --TIO.New_Line;

end Read_Float_Plane;




-- random access



 generic
  type T is private;
  type T_Arr is array (Positive_Count range <>) of T;
 procedure Read_Raw_Line
   (F : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Values  : out T_Arr);

 procedure Read_Raw_Line
   (F : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Values  : out T_Arr)
 is
  Offset    : Positive_Count := To_Offset(First, NAXISn);
  procedure ReadArr is new Unit.Read_Array(T, T_Arr);
 begin
  ReadArr(F, DUStart, Offset, Values);
  -- FIXME Read_Array reads by blocks, but may read all in one:
  -- T_Arr'Read(Stream(F), Values) <- must have endianness
 end Read_Raw_Line;



 procedure Read_Raw_Volume
   (File : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Last    : in NAXIS_Arr;
    Volume  : out T_Arr) -- FIXME  later make T_Arr private
 is
   procedure Read_One_Line
    is new Read_Raw_Line(T,T_Arr);

   LineLength : Positive_Count := 1 + Positive_Count(Last(1) - First(1)); -- FIXME FInteger
   Line: T_Arr(1 .. LineLength);

   -- generate coords vars
   Winit : FIndex := 2;
   W : FIndex;
   C  : NAXIS_Arr := First;  -- Current coords in source Data Unit
   CV : NAXIS_Arr := First;  -- Current coords in target Volume
   Vf, Vl : Positive_Count;
   Unity : constant NAXIS_Arr(First'Range) := (others => 1);
   VolNAXISn : NAXIS_Arr(First'Range);
 begin

 for I in First'Range loop
   VolNAXISn(I) := Unity(I) + Last(I) - First(I);
 end loop;

  W := Winit;
  C := First;
  for I in First'Range loop
   CV(I) := Unity(I) + C(I) - First(I);
  end loop;


  --print_coord(C)
  Read_One_Line(File, DUStart, NAXISn, C, Line);

  Vf := To_Offset(CV,VolNAXISn);
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
   Read_One_Line(File, DUStart, NAXISn, C, Line);

   for I in First'Range loop
    CV(I) := Unity(I) + C(I) - First(I);
   end loop;

   Vf := To_Offset(CV,VolNAXISn);
   Vl := Vf + LineLength - 1;
   Volume(Vf .. Vl) := Line;
  -- store read line

  end loop Outer_Loop;

 end Read_Raw_Volume;















 procedure Read_Valid_Scaled_Line
   (F : SIO.File_Type;
    BZERO  : in Tout;
    BSCALE : in Tout;
    Undef_Val : in Tout;
    DUStart   : in Positive_Count;
    NAXISn : in NAXIS_Arr;
    First  : in NAXIS_Arr;
    Length : in Positive_Count; -- may be at most NAXIS1
    Values : out Tout_Arr)
 is
  Offset    : Positive_Count := To_Offset(First, NAXISn);
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





 procedure Read_Valid_Scaled_Volume
                (File : SIO.File_Type; 
                BZERO  : in Tout;
                BSCALE : in Tout;
                Undef_Val : in Tout; 
                DUStart   : in Positive_Count;
                NAXISn : in NAXIS_Arr;-- NAXISn
                First  : in NAXIS_Arr;
                Last   : in NAXIS_Arr;
                Volume : out Tout_Arr)
 is
   procedure Read_One_Line
    is new Read_Valid_Scaled_Line(T,Tout,Tout_Arr, Is_Valid, "+","*","+");

   LineLength : Positive_Count := 1 + Positive_Count(Last(1) - First(1)); -- FIXME FInteger
   Line: Tout_Arr(1 .. LineLength);

   -- generate coords vars
   Winit : FIndex := 2;
   W : FIndex;
   C  : NAXIS_Arr := First;  -- Current coords in source Data Unit
   CV : NAXIS_Arr := First;  -- Current coords in target Volume
   Vf, Vl : Positive_Count;
   Unity : constant NAXIS_Arr(First'Range) := (others => 1);
   VolNAXISn : NAXIS_Arr(First'Range);
 begin

 for I in First'Range loop
   VolNAXISn(I) := Unity(I) + Last(I) - First(I);
 end loop;

  W := Winit;
  C := First;
  for I in First'Range loop
   CV(I) := Unity(I) + C(I) - First(I);
  end loop;


  --print_coord(C)  
  Read_One_Line(File,BZERO,BSCALE, Undef_Val,DUStart,
        NAXISn, C, LineLength, Line);

  Vf := To_Offset(CV,VolNAXISn);
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
        NAXISn, C, LineLength, Line);

   for I in First'Range loop
    CV(I) := Unity(I) + C(I) - First(I);
   end loop;

   Vf := To_Offset(CV,VolNAXISn);
   Vl := Vf + LineLength - 1;
   Volume(Vf .. Vl) := Line;
  -- store read line

  end loop Outer_Loop;

 end Read_Valid_Scaled_Volume;


end NCube;

