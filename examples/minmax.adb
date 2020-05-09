--
-- Example convert FLoat32 -> Int16
--
-- FIXME: assume Primary HDU. What if other HDU is IMAGE type too?
--
-- demonstrate usage if data unit is "big":
-- all data will not fit into memory, needs to be processed
-- in chunks.


with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;
with Ada.Unchecked_Conversion;
with Ada.Streams.Stream_IO;
with Interfaces;

with Ada.Unchecked_Conversion;

with File;   use File;
with File_Funcs;  use File_Funcs;
with File.Misc;   use File.Misc;
with Keyword_Record; use Keyword_Record;
with Header; use Header;
with Mandatory; use Mandatory; -- NAXIS_Arr needed
with Optional;
with Optional.Reserved; use Optional.Reserved;


-- new Data interface
with V3_Types; use V3_Types;

with Image_Data;
--with Physical;
with Linear_Conv;


procedure minmax
is

  package TIO renames Ada.Text_IO;
  package SIO renames Ada.Streams.Stream_IO;
  use SIO;

  function Get_Int_16
    (Key : in String; 
    Cards : in Optional.Card_Arr) return Integer_16
  is
  begin
    for I in Cards'Range
    loop
      if(Cards(I)(1..Key'Length) = Key)
      then
        return Integer_16'Value(Cards(I)(11..30));
      end if;
    end loop;
    return 0;-- FIXME raise exception "Not_Found"
  end Get_Int_16;


  generic
  type T is digits <>;
  function Get_Float
    (Key : in String; 
    Cards : in Optional.Card_Arr;
    Default_Value : in T) return T;

    function Get_Float
      (Key : in String;
      Cards : in Optional.Card_Arr;
      Default_Value : in T) return T
    is
    begin
      for I in Cards'Range
      loop
        if(Cards(I)(1..Key'Length) = Key)
        then
          TIO.Put_Line(Cards(I)(11..30));
          return T'Value(Cards(I)(11..30));
        end if;
      end loop;
      return Default_Value;
    end Get_Float;

    function F64_Get_Float is new Get_Float(Float_64);
    function F32_Get_Float is new Get_Float(Float_32);



    InFile   : SIO.File_Type;
    HDUStart : SIO.Positive_Count := 1; -- Primary HDU only

    BITPIX  : Integer;
    NAXISn  : NAXIS_Arr(1..2);
    --NAXISn  : NAXIS_Arr(1..3);
    DUSize  : SIO.Positive_Count;
    DUStart : SIO.Positive_Count;

    PlaneLength :  SIO.Positive_Count;
    NPlanes     :  SIO.Positive_Count;

  begin 

    if(Argument_Count /= 1 )
    then 
      Put_Line("Usage  " & Command_Name & " <file name>");
      return;
    else
      SIO.Open(InFile, SIO.In_File, (Argument(1)));
    end if;

    File.Set_File_Block_Index(InFile,HDUStart);

    declare
      HDUInfo : HDU_Info_Type := Read_Header(InFile);
    begin
      DUStart := File_Block_Index(InFile);
      DUSize  := SIO.Positive_Count(Data_Unit_Size_elems(HDUInfo.NAXISn));
      BITPIX := HDUInfo.BITPIX;
      NAXISn := HDUInfo.NAXISn;
    end;

    Put_Line("DU Start [blocks] :" & SIO.Positive_Count'Image(DUStart)); 
    Put_Line("DU Size  [element count]:" & SIO.Positive_Count'Image(DUSize)); 

    File.Set_File_Block_Index(InFile,HDUStart);

    for I in NAXISn'Range
    loop
      Put_Line(Integer'Image(I) & " : " & SIO.Positive_Count'Image(NAXISn(I)));
    end loop;

    --NPlanes     := NAXISn(3)*NAXISn(2);
    NPlanes     := NAXISn(2);
    PlaneLength := NAXISn(1);
    -- Size of Plane must be reasonable
    Put_Line("Plane Size [Bytes]:" & SIO.Positive_Count'Image(PlaneLength * SIO.Positive_Count( abs BITPIX/8 ))); 
    Put_Line("NPlanes           :" & SIO.Positive_Count'Image(NPlanes)); 

    declare
      Cards : Optional.Card_Arr := Read_Optional(InFile, Optional.Reserved.Array_Keys);
      F64_BZERO  : Float_64   := F64_Get_Float("BZERO",  Cards, 0.0);
      F64_BSCALE : Float_64   := F64_Get_Float("BSCALE", Cards, 1.0);
      F32_BZERO  : Float_32   := F32_Get_Float("BZERO",  Cards, 0.0);
      F32_BSCALE : Float_32   := F32_Get_Float("BSCALE", Cards, 1.0);
      I16_BLANK  : Integer_16 := Get_Int_16("BLANK", Cards);

      function I16F32_Scale is
        new Linear_Conv.FI4R(Integer_16, Float_32, Float_32,F32_BZERO,F32_BSCALE);
      NewBLANK : Float_32 := I16F32_Scale(I16_BLANK);


      -- new BEGIN
      type F64_Arr is array(SIO.Positive_Count range <>) of Float_64;
      type F32_Arr is array(SIO.Positive_Count range <>) of Float_32;
      package F64F64 is new Image_Data.FF(Float_64, F64_Arr, Float_64, Float_64, F64_BZERO,F64_BSCALE,F64NaN,F64NaN);
      package F32F32 is new Image_Data.FF(Float_32, F32_Arr, Float_32, Float_32, F32_BZERO,F32_BSCALE,F32NaN,F32NaN);
      package F32I16 is new Image_Data.FI(Float_32, F32_Arr, Float_32, Integer_16, F32_BZERO,F32_BSCALE);
      type F64_Plane_Acc is Access F64_Arr;
      type F32_Plane_Acc is Access F32_Arr;
        -- FIXME how to implement algorithms 
      -- * for all V3 file-types but one Tm type (Float_64)
      -- * for all combinations of V3 types Tm x Tf : 10 x 6 = 60 combinations
      --
      -- consider writing generic parametrized by Tm/Tm_Arr
      -- which implements Read_Array in case(BITPIX)... 
      -- for all V3 types: TmF64 TmF32 TmI64 TmI32...
      -- then instantiate for one (or all) 
      -- V3-Tm types: F64 (F32 I64 ... I8 U64 U32 .. U8)
      -- new END

      F64Plane : F64_Plane_Acc := new F64_Arr(1..PlaneLength);
      F32Plane : F32_Plane_Acc := new F32_Arr(1..PlaneLength);
      Max : Float_32 := Float_32'First;
      Invalid_Count : Natural := 0;

    begin
      TIO.Put_Line("BZERO    : " & Float_32'Image(F32_BZERO));
      TIO.Put_Line("BSCALE   : " & Float_32'Image(F32_BSCALE));
      TIO.Put_Line("   BLANK : " & Integer_16'Image(I16_BLANK));
      TIO.Put_Line("NewBLANK : " & Float_32'Image(NewBLANK));

      for I in 1..NPlanes
      loop

        case(BITPIX) is
          when -64 =>
              F64F64.Physical_In.Read_Array(InFile,F64Plane.All);
          when -32 =>
              F32F32.Physical_In.Read_Array(InFile,F32Plane.All);
          when  64 => null;
          when  32 => null;
          when  16 =>
              F32I16.Physical_In.Read_Array(InFile,F32Plane.All);
          when   8 => null;
          when others => TIO.Put_Line("BITPIX " & Integer'Image(BITPIX) & " not implemented.");
        end case;

        -- FIXME implement generic (F64Plane ...)

        for I in F32Plane'Range
        loop
          if(F32Plane(I) /= F32NaN)
          then
            if(F32Plane(I)>Max) then Max := F32Plane(I); end if;
          else
            Invalid_Count := Invalid_Count + 1;
          end if;
        end loop;

      end loop;
      TIO.New_Line;
      TIO.Put_Line("Max Value :" & Float_32'Image(Max));
      TIO.Put_Line("Invalid Count: " & Natural'Image(Invalid_Count));
      TIO.Put_Line("Invalid Count [%]: " & Float_32'Image(100.0*Float_32(Invalid_Count)/Float_32(DUSize)));
    end;

    SIO.Close(InFile);

    Put_Line("done.");


  exception

    when Except_ID : others =>
      declare
        Error : Ada.Text_IO.File_Type := Standard_Error;
      begin
        New_Line(Error);
        Put_Line(Error, "Exception_Information: ");
        Put_Line(Error, Exception_Information( Except_ID ) );
        New_Line(Error);
        Put_Line(Error, "Call stack traceback symbols: addr2line -e ./fits addr1 addr2 ...");
        Put_Line(" > Trace-back of call stack: " );
        Put_Line( GNAT.Traceback.Symbolic.Symbolic_Traceback(Except_ID) );
        -- See more at: http://compgroups.net/comp.lang.ada/gnat-symbolic-traceback-on-exceptions/1409155#sthash.lNdkTjq6.dpuf
        -- Do the same manually, use:
        -- addr2line -e ./fits addr1 addr2 ...
      end;
  end minmax;

