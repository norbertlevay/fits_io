
-- Sequential Access:
-- if T_Arr'Length = NAXISi*NAXISi-1*...*NAXIS1
-- then repeating Read (NAXISn*NAXISn-1*...NAXISi+1)-times 
-- reads all DU sequentially
-- NOTE position to DUStart before 1st call in Read/Write_x_Data

-- FIXME Read/Write_Array should be 'separate'-file and have two variants:
-- with and without Revert_Bytes depending on build-target being
-- Little- or BigEndian code

with Ada.Text_IO;

with Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;
with Interfaces;

with Raw_Funcs;
with File_Funcs;


package body Raw is

  --use SIO;

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


  procedure Read_Raw_Line
    (F : SIO.File_Type;
    DUStart : in Positive_Count; -- block count
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    AValues : in out T_Arr)
  is
    -- StreamElem count (File_Index) from File begining:
    DUStart_SE  : Positive_Count := 1 + (DUStart-1) * 2880;
    DUIndex     : Positive_Count := Raw_Funcs.To_DU_Index(First, NAXISn);
    --procedure ReadArray is new Read_Array(T,T_Arr);
 begin
    SIO.Set_Index(F, SIO.Count(DUStart_SE + (DUIndex-1)*T'Size/8));
    -- FIXME use Stream_Elemen'Size AND cast to SIO.Count !!
    Read_Array(F, AValues);
  end Read_Raw_Line;



  procedure Read_Volume
    (File : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    VolumeSize : in NAXIS_Arr; -- organization of data in Volume
    --Last    : in NAXIS_Arr;
    Volume  : out T_Arr) -- FIXME  later make T_Arr private
  is
--    procedure Read_One_Line is new Read_Raw_Line(T,T_Arr);

      LineLength : Positive_Count := VolumeSize(1);
      Line       : T_Arr(1 .. LineLength);
    --LineLength : Positive_Count := 1 + (Last(1) - First(1));
    --Line: T_Arr(1 .. LineLength);

      Last : NAXIS_Arr := VolumeSize;

    -- generate coords vars
    Winit : FIndex := 2;
    W : FIndex;
    C  : NAXIS_Arr := First;  -- Current coords in source Data Unit
    CV : NAXIS_Arr := First;  -- Current coords in target Volume
    Vf, Vl : Positive_Count;
    Unity : constant NAXIS_Arr(First'Range) := (others => 1);
    VolNAXISn : NAXIS_Arr(First'Range);
  begin

      -- convert VoilumeSize -> Last

    for I in First'Range loop
        Last(I) := First(I) + VolumeSize(I) - 1;
    end loop;

    for I in First'Range loop
      VolNAXISn(I) := Unity(I) + Last(I) - First(I);
    end loop;

    W := Winit;
    C := First;
    for I in First'Range loop
      CV(I) := Unity(I) + C(I) - First(I);
    end loop;

    --print_coord(C)
    Read_Raw_Line(File, DUStart, NAXISn, C, Line);

    Vf := Raw_Funcs.To_DU_Index(CV,VolNAXISn);
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
      Read_Raw_Line(File, DUStart, NAXISn, C, Line);

      for I in First'Range loop
        CV(I) := Unity(I) + C(I) - First(I);
      end loop;

      Vf := Raw_Funcs.To_DU_Index(CV,VolNAXISn);
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
--   procedure Write_One_Line is new Write_Array(T, T_Arr);
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

        DestDUIndex  := Raw_Funcs.To_DU_Index(DestC, NAXISn);
        SIOFileIndex := SIO.Count(DestDUIndex*(T'Size/8) + (DUStart - 1) * 2880);
        -- FIXME cast SIO.Count !!
        SIO.Set_Index(File, SIOFileIndex);
        Write_Array(File, Line);
      end loop;

      C(I) := 1;

   end loop;

 end Write_Volume;


end Raw;

