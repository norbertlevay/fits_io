
-- Sequential Access:
-- if T_Arr'Length = NAXISi*NAXISi-1*...*NAXIS1
-- then repeating Read (NAXISn*NAXISn-1*...NAXISi+1)-times 
-- reads all DU sequentially
-- NOTE position to DUStart before 1st call in Read/Write_x_Plane

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

package body Raw is

  use SIO;

  package TIO renames Ada.Text_IO;

  -- Endianness

  generic
  type T is private;
  procedure Revert_Bytes( Data : in out T );
  procedure Revert_Bytes( Data : in out T )
  is
    Size_Bytes : Positive := T'Size / Interfaces.Unsigned_8'Size;
    type Arr4xU8 is array (1..Size_Bytes) of Interfaces.Unsigned_8;

    function Data_To_Arr is new Ada.Unchecked_Conversion(Source => T, Target => Arr4xU8);
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



  -- Sequential access


  procedure Read_Raw_Plane
    (F : SIO.File_Type;
    Plane  : out T_Arr)
  is
  begin
    T_Arr'Read(SIO.Stream(F),Plane);
  end Read_Raw_Plane;

  procedure Write_Raw_Plane
    (F : SIO.File_Type;
    Plane  : in T_Arr)
  is
  begin
    T_Arr'Write(SIO.Stream(F),Plane);
  end Write_Raw_Plane;



  -- Random access


  generic
  type T is private;
  type T_Arr is array (Positive_Count range <>) of T;
  procedure Read_Raw_Line
    (F : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Length  : in Positive_Count;
    AValues  : in out T_Arr);
    -- FIXME AValues put to Heap, might be too big for Stack

  procedure Read_Raw_Line
    (F : SIO.File_Type;
    DUStart : in Positive_Count; -- block count
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Length  : in Positive_Count;
    AValues  : in out T_Arr)
  is
    -- StreamElem count (File_Index) from File begining:
    DUStart_SE : SIO.Positive_Count := 1 + (DUStart-1) * 2880;
    DUIndex : Positive_Count := NCube_Funcs.To_Offset(First, NAXISn);
    procedure RevertBytes is new Revert_Bytes(T);
 begin

    SIO.Set_Index(F, DUStart_SE + (DUIndex-1)*T'Size/8);-- FIXME use Stream_Elemen'Size

    T_Arr'Read(SIO.Stream(F), AValues);

    for I in AValues'Range
    loop
      RevertBytes(AValues(I));
    end loop;

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
    Read_One_Line(File, DUStart, NAXISn, C, LineLength, Line);

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
      Read_One_Line(File, DUStart, NAXISn, C, LineLength, Line);

      for I in First'Range loop
        CV(I) := Unity(I) + C(I) - First(I);
      end loop;

      Vf := To_Offset(CV,VolNAXISn);
      Vl := Vf + LineLength - 1;
      Volume(Vf .. Vl) := Line;
      -- store read line

    end loop Outer_Loop;

  end Read_Raw_Volume;



 procedure Write_Raw_Volume
   (File : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Last    : in NAXIS_Arr;
    Volume  : in T_Arr)
 is
 begin
   -- FIXME not implemented
   null;
 end Write_Raw_Volume;







end Raw;
