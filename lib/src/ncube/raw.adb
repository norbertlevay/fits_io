
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

  -- Plane and Volume Lnegths

  function Plane_Length
    (Plane : in NAXIS_Arr) return Positive_Count
  is
    PlaneLen : Positive_Count := 1;
  begin
    for I in Plane'Range
    loop
      PlaneLen := PlaneLen * Plane(I);
    end loop;
    return PlaneLen;
  end Plane_Length;
 
  function Volume_Length
    (First : in NAXIS_Arr;
    Last   : in NAXIS_Arr) return Positive_Count
  is
    L : Positive_Count := 1;
  begin

    -- FIXME sanity check that First & Last are equal length

    for I in First'Range
    loop
      L := L * (1 + Positive_Count(Last(I) - First(I)));
    end loop;
    return L;
  end Volume_Length;



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


  generic
    type T is private;
    type T_Arr is array (Positive_Count range <>) of T;
  procedure Check_And_Revert(Arr : in out T_Arr);
  procedure Check_And_Revert(Arr : in out T_Arr)
  is
    procedure RevertBytes is new Revert_Bytes(T);
  begin
    -- FIXME add here check endianness: revert bytes only of needed
    -- FIXME build-time or run-time issue ?
    for I in Arr'Range
    loop
      RevertBytes(Arr(I));
    end loop;
  end Check_And_Revert;


  -- Sequential access


  procedure Read_Plane
    (F : SIO.File_Type;
    Plane  : out T_Arr)
  is
    procedure CheckAndRevert is new Check_And_Revert(T,T_Arr);
  begin
    T_Arr'Read(SIO.Stream(F),Plane);
    CheckAndRevert(Plane);
  end Read_Plane;

  procedure Write_Plane
    (F : SIO.File_Type;
    Plane  : in T_Arr)
  is
    LocalPlane : T_Arr := Plane;
    procedure CheckAndRevert is new Check_And_Revert(T,T_Arr);
  begin
    CheckAndRevert(LocalPlane);
    T_Arr'Write(SIO.Stream(F),LocalPlane);
  end Write_Plane;



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
    DUIndex : Positive_Count := NCube_Funcs.To_DU_Index(First, NAXISn);
    procedure CheckAndRevert is new Check_And_Revert(T,T_Arr);
 begin
    SIO.Set_Index(F, DUStart_SE + (DUIndex-1)*T'Size/8);-- FIXME use Stream_Elemen'Size
    T_Arr'Read(SIO.Stream(F), AValues);
    CheckAndRevert(AValues);
  end Read_Raw_Line;








  procedure Read_Volume
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

    Vf := To_DU_Index(CV,VolNAXISn);
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

      Vf := To_DU_Index(CV,VolNAXISn);
      Vl := Vf + LineLength - 1;
      Volume(Vf .. Vl) := Line;
      -- store read line

    end loop Outer_Loop;

  end Read_Volume;



 procedure Write_Volume
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
 end Write_Volume;

end Raw;

