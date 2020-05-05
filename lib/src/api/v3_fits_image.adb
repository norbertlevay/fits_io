-- NOTE from Read_Volume_As_Float:
--  if(BITPIX > 0)
--  then
    -- call ReadIntPlane()
--    null;
--  else
    -- call ReadFloatPlane()
--    null;
--  end if;

--DUSize  := Data_Unit_Size_elems(HDUInfo.NAXISn);

    -- Set Undef val in Read_Volume_As_Int/UInt()
--    case(BITPIX) is
--      when   8 => Undef_Value := ScaleUI8(BLANK_Str); -- UndefVal = A + B * BLANK_Str;
--      when  16 => Undef_Value := ScaleI16(BLANK_Str);
--      when -32 => Undef_Value := Tm(F64NaN);-- FIXME weird
--      when -64 => Undef_Value := Tm(F64NaN);-- FIXME weird
--      when others => null; -- FIXME Error
--    end case;

-- decode BLANK as string:
-- BLANK_Str : String(1..20);
-- ignore in Read_* _As_Float:      elsif(Key = "BLANK   ") then BLANK_Str := Cards(I)(11..30);


with Ada.Text_IO; --use Ada.Text_IO;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed
with Mandatory; use Mandatory;-- NAXIS_Arr needed
with Optional;
with Optional.Reserved;
with Header; use Header;
with File;
with V3_Types; use V3_Types;-- types needed

with FF;
with FI;
with Physical;

package body V3_FITS_Image is







-- NOTE taken from original place data/data_funcs.ad? when data/ deprecated
-- indexing relative to file start
BlockSize_bytes    : constant Positive_Count :=  2880;
BlockSize_bits     : constant Positive_Count := 23040;
BlockSize_sioelems : constant Positive_Count
    := BlockSize_bits / Ada.Streams.Stream_Element'Size;
-- size counted in SIO file-index elements


function File_Block_Index(File : File_Type) return Positive_Count
is
    SIO_Index : Positive_Count := Index(File);
begin
    return (1 + (SIO_Index - 1) / BlockSize_sioelems);
end File_Block_Index;



procedure Set_File_Block_Index
        (File        : File_Type; 
         Block_Index : in Positive_Count)
is
    SIO_Index : Positive_Count :=  
        1 + (Block_Index - 1) * BlockSize_sioelems;
begin
    Set_Index(File, SIO_Index);
end Set_File_Block_Index;




-- Read data

-- FIXME how to give "none" no BLANK provided (for raw Integers data)
procedure Read_Plane_As_Float
  (F : in SIO.File_Type;
  NAXISi : in NAXIS_Arr; -- Tm_Arr has size NAXIS1 .. NAXISi, where i<=NAXISn'Length
  Undef_Value : out Tm) -- FIXME how to return "none"=no BLANK (for raw Integers data)
is
  BITPIX : Integer;
  NAXISn : NAXIS_Arr(1..1);-- FIXME
  Length : Positive_Count := 1; -- calc from NAXISn

  Plane : Tm_Arr(1 .. Length);

  -- NOTE Scaling always calced in Float_64 (=Tc)
  -- For starters: always Float_64: Card-Values string has 20 chars 
  -- -> Float_64 has 15 digits validity -> use for card-value floating point always Float 64
  subtype Tcalc is Float_64;
  F64BZERO, F64BSCALE : Tcalc;
-- From Tc -> Tm
--  function "+"(R : in Tcalc) return Tm is begin return Tm(R); end "+";

  -- From Tf -> Tc
--  function "+"(R : in Unsigned_8) return Tcalc is begin return Tcalc(R); end "+";
--  function "+"(R : in Integer_16) return Tcalc is begin return Tcalc(R); end "+";
--  function "+"(R : in Float_32)   return Tcalc is begin return Tcalc(R); end "+";
--  function "+"(R : in Tcalc)   return Float_32 is begin return Float_32(R); end "+";

  --                                                     Tf                   Tc
--  package U8_Physical  is new Ints_Physical(Unsigned_8,Tm,Tm_Arr,Tcalc,"+","+");
--  package I16_Physical is new Ints_Physical(Integer_16,Tm,Tm_Arr,Tcalc,"+","+");
--  package F32_Physical is new Floats_Physical(Float_32,Tm,Tm_Arr,Tcalc,"+");
--  package F64_Physical is new Floats_Physical(Float_64,Tm,Tm_Arr,Tcalc,"+");

begin
  -- read Header keys
  -- FIXME read BZERO BSCALE (BLANK)

  -- FIXME revert: put loop inside case() to avoid check BITPIX at each turn
  for I in 1 .. (1 + NAXISn'Length - Length)
  loop
    case(BITPIX) is
--      when   8 =>  U8_Physical.Read_Array(F, F64BZERO, F64BSCALE, Plane);
--      when  16 => I16_Physical.Read_Array(F, F64BZERO, F64BSCALE, Plane);
--      when -32 => F32_Physical.Read_Array(F, F64BZERO, F64BSCALE, Undef_Value, Plane);
--      when -64 => F64_Physical.Read_Array(F, F64BZERO, F64BSCALE, Undef_Value, Plane);
      when others => null; -- FIXME Error
    end case;
    Plane_Data(Plane, I);
  end loop;

end Read_Plane_As_Float;


procedure Write_Plane_As_Float
    (F : in SIO.File_Type;
    NAXISi : in NAXIS_Arr; -- Tm_Arr has size NAXIS1 .. NAXISi, where i<=NAXISn'Length
    Undef_Value : in Tm) -- FIXME how to return "none"=no BLANK (for raw Integers data)
is
begin
  null;
end Write_Plane_As_Float;








procedure Read_Volume_As_Float
  (F : in SIO.File_Type;         -- File F
  HDUStart : in Positive_Count;  -- at offset FDUStart (referenced by HDUNum)
  First, Last : in NAXIS_Arr;    -- F and L point limiting the subcube of NAXISn
  Undef_Value : out Tm;
  Volume : out Tm_Arr)
is
  DUStart : Positive_Count;

  subtype Tcalc is Float_64;
  F64BZERO  : Tcalc := 0.0;
  F64BSCALE : Tcalc := 1.0;

-- test FF/FI packages:
  package F64F64 is new FF(Float_64,Float_64);
  package F32F32 is new FF(Float_32,Float_32);
  package F32I16 is new FI(Float_32,Integer_16);

begin

  Ada.Text_IO.Put_Line("DBG: Read_Volume_As_Float");


  Set_File_Block_Index(F, HDUStart);

  declare
    Cards : Optional.Card_Arr := Read_Optional(F, Optional.Reserved.Reserved_Keys);
    Key : String(1..8);
  begin
    for I in Cards'Range
    loop
      Key := Cards(I)(1..8);
      if   (Key = "BZERO   ") then F64BZERO  := Float_64'Value(Cards(I)(11..30));
      elsif(Key = "BSCALE  ") then F64BSCALE := Float_64'Value(Cards(I)(11..30));
      end if;
    end loop;
  end;

  Set_File_Block_Index(F, HDUStart);

  declare
    HDUInfo : File.HDU_Info_Type := File.Read_Header(F);
    NAXISn  : NAXIS_Arr := HDUInfo.NAXISn;
    BITPIX  : Integer   := HDUInfo.BITPIX;

    F64F64Vol : F64F64.Phys.Tm_Arr(Volume'Range);
    F32F32Vol : F32F32.Phys.Tm_Arr(Volume'Range);
    F32I16Vol : F32I16.Phys.Tm_Arr(Volume'Range);

  begin
    DUStart := File_Block_Index(F);

    -- FIXME this case is weird - known at instantiation
    case(Tm'Size) is
      when 32 => Undef_Value := Tm(F32NaN);
      when 64 => Undef_Value := Tm(F32NaN);
      when others => null; -- Error
    end case;

    case(BITPIX) is
      when  16 =>
          F32I16.Phys.Read_Volume(F,DUStart,NAXISn, First,Last, Float_32(0.0),Float_32(1.0), F32I16Vol);
          for I in Volume'Range loop Volume(I) := Tm(F32I16Vol(I)); end loop;

      when -32 => 
          F32F32.Phys.Read_Volume(F,DUStart,NAXISn, First,Last, Float_32(0.0),Float_32(1.0), F32F32Vol);
          for I in Volume'Range loop Volume(I) := Tm(F32F32Vol(I)); end loop;

      when -64 =>
          F64F64.Phys.Read_Volume(F,DUStart,NAXISn, First,Last, Float_64(0.0),Float_64(1.0), F64F64Vol);
          for I in Volume'Range loop Volume(I) := Tm(F64F64Vol(I)); end loop;

      when others => null; -- FIXME Error
    end case;


  end;

end Read_Volume_As_Float;







procedure Write_Volume
  (F : in SIO.File_Type;
  HDUStart : in Positive_Count;
  NAXISn : in NAXIS_Arr;
  First, Last : in NAXIS_Arr;
  Volume : in Tm_Arr)
is
begin
  null;
end Write_Volume;


end V3_FITS_Image;

