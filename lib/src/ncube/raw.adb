
-- Sequential Access:
-- if T_Arr'Length = NAXISi*NAXISi-1*...*NAXIS1
-- then repeating Read (NAXISn*NAXISn-1*...NAXISi+1)-times 
-- reads all DU sequentially
-- NOTE position to DUStart before 1st call in Read/Write_x_Data

-- FIXME Read/Write_Array should be 'separate'-file and have two variants:
-- with and without Revert_Bytes depending on build-target being
-- Little- or BigEndian code

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

with File_Funcs;


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


  procedure Read_Array
    (F : SIO.File_Type;
    Data  : out T_Arr)
  is
    procedure CheckAndRevert is new Check_And_Revert(T,T_Arr);
  begin
    T_Arr'Read(SIO.Stream(F),Data);
    CheckAndRevert(Data);
  end Read_Array;

  procedure Write_Array
    (F : SIO.File_Type;
    Data  : in T_Arr)
  is
    LocalData : T_Arr := Data;
    procedure CheckAndRevert is new Check_And_Revert(T,T_Arr);
  begin
    CheckAndRevert(LocalData);
    T_Arr'Write(SIO.Stream(F),LocalData);
  end Write_Array;



  -- Random access


  generic
  type T is private;
  type T_Arr is array (Positive_Count range <>) of T;
  procedure Read_Raw_Line
    (F : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    AValues : in out T_Arr);
    -- FIXME AValues put to Heap, might be too big for Stack

  procedure Read_Raw_Line
    (F : SIO.File_Type;
    DUStart : in Positive_Count; -- block count
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    AValues : in out T_Arr)
  is
    -- StreamElem count (File_Index) from File begining:
    DUStart_SE : SIO.Positive_Count := 1 + (DUStart-1) * 2880;
    DUIndex : Positive_Count := NCube_Funcs.To_DU_Index(First, NAXISn);
    procedure ReadArray is new Read_Array(T,T_Arr);
 begin
    SIO.Set_Index(F, DUStart_SE + (DUIndex-1)*T'Size/8);-- FIXME use Stream_Elemen'Size
    ReadArray(F, AValues);
  end Read_Raw_Line;


-- whole DU access



function DU_Length_blks(BITPIX : in Natural; NAXISn : in NAXIS_Arr) return Positive_Count
is
  Size_bits : SIO.Count;
  BitsPerBlock : constant Positive_Count := (2880*8);
begin
   Size_bits := File_Funcs.PrimaryImage_DataSize_Bits(BITPIX, NAXISn);
   return (1 + (Size_bits - 1) / BitsPerBlock);
end DU_Length_blks;



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




procedure Write
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr)
is
  DULength : Positive_Count := DU_Data_Count(NAXISn);
  DULength_blks : Positive_Count := DU_Length_blks(T'Size, NAXISn);
  PaddingFirst, PaddingLast : Positive_Count;
  -- FIXME T'Size instead of BITPIX ok?
  First : NAXIS_Arr := NAXISn; -- FIXME we don't need values only size
  Block : T_Arr(1 .. 2880/(T'Size/8));
  -- FIXME explicit ranges instead of 'First .. 'First + 2880 - 1
  procedure Write_Block is new Write_Array(T, T_Arr);
begin

  TIO.Put_Line("T'Size        : " & Positive_Count'Image(T'Size));
  TIO.Put_Line("DULength      : " & Positive_Count'Image(DULength));
  TIO.Put_Line("DULength_blks : " & Positive_Count'Image(DULength_blks));

  for I in 1 .. (DULength_blks - 1)
  loop
    Data(Block);
    Write_Block(File, Block);
  end loop;

  -- last block handle separately because needs padding
  Data(Block);
  PaddingFirst := DULength rem (2880/(T'Size/8));
  PaddingLast  := 2880/(T'Size/8);-- Block end

  TIO.Put_Line("PaddingFirst  : " & Positive_Count'Image(PaddingFirst));
  TIO.Put_Line("PaddingLast   : " & Positive_Count'Image(PaddingLast));

  Block(PaddingFirst .. PaddingLast) := (others => T_DataPadding);
  Write_Block(File, Block);

end Write;




procedure Read
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr)
is
  DULength_blks : Positive_Count := DU_Length_blks(T'Size,NAXISn);
  -- FIXME crosscheck use of T'Size instead of BITPIX, ok?
  First : NAXIS_Arr := NAXISn;
  Block : T_Arr(1 .. 2880); -- FIXME explicit ranges instead of 'First .. 'First + 2880 - 1
  procedure Read_Block is new Read_Array(T, T_Arr);
begin

  for I in 1 .. DULength_blks
  loop
    Read_Block(File, Block);
    Data(Block);
  end loop;

end Read;








-- random access


  procedure Read_Volume
    (File : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Last    : in NAXIS_Arr;
    Volume  : out T_Arr) -- FIXME  later make T_Arr private
  is
    procedure Read_One_Line is new Read_Raw_Line(T,T_Arr);

    LineLength : Positive_Count := 1 + (Last(1) - First(1));
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
      Read_One_Line(File, DUStart, NAXISn, C, Line);

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
    DUStart : in Positive_Count; -- count in Blocks
    NAXISn  : in NAXIS_Arr;    -- organization of data in DU
    First   : in NAXIS_Arr;
    VolumeSize : in NAXIS_Arr; -- organization of data in Volume
    Volume     : in T_Arr)
 is
   LineLength : Positive_Count := VolumeSize(1);
   Line       : T_Arr(1 .. LineLength);
   LineFirst  : Positive_Count;
   LineLast   : Positive_Count := LineLength;
   C     : NAXIS_Arr(First'First .. First'Last);
   DestC : NAXIS_Arr(First'First .. First'Last);
   DestDUIndex : Positive_Count;
   SIOFileIndex : SIO.Positive_Count;
   procedure Write_One_Line is new Write_Array(T, T_Arr);
 begin

   for I in 2 .. Volume'Length 
   loop

      for L in 1 .. VolumeSize(I)
      loop
        C(I) := L;
        -- get Line from Source
        LineFirst := LineLast + 1;
        LineLast  := LineFirst + LineLength - 1;
        Line := Volume(LineFirst .. LineLast);

        -- put Line into Dest
        for K in 1 .. C'Length
        loop
          DestC(K) := First(K) + C(K) - 1;
        end loop;

        DestDUIndex  := To_DU_Index(DestC, NAXISn);
        SIOFileIndex := DestDUIndex*(T'Size/8) + (DUStart - 1) * 2880;
        SIO.Set_Index(File, SIOFileIndex);
        Write_One_Line(File, Line);
      end loop;

      C(I) := 1;

   end loop;

 end Write_Volume;

end Raw;

