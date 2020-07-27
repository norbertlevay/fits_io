
with Ada.Text_IO; --use Ada.Text_IO;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed
with Mandatory; use Mandatory;-- NAXIS_Arr needed
with Optional;
with Optional.Reserved;
with Header; use Header;
with File;
with V3_Types; use V3_Types;-- types needed

with Physical_Read;
with Linear_Impl; use Linear_Impl;-- needed From_Header() for Header_Info instance
with Pool_String_To_V3Types; use Pool_String_To_V3Types; -- needed by Header_Info instance
with Pool_V3Type_Convs; use  Pool_V3Type_Convs;

package body V3_Image_Read is

package TIO renames Ada.Text_IO;

  Package F64_Physical is new Physical_Read(Tm, Tm_Arr, Tc, Float_64);
  Package F32_Physical is new Physical_Read(Tm, Tm_Arr, Tc, Float_32);

  Package I64_Physical is new Physical_Read(Tm, Tm_Arr, Tc, Integer_64);
  Package I32_Physical is new Physical_Read(Tm, Tm_Arr, Tc, Integer_32);
  Package I16_Physical is new Physical_Read(Tm, Tm_Arr, Tc, Integer_16);
  Package U8_Physical  is new Physical_Read(Tm, Tm_Arr, Tc, Unsigned_8);



procedure Read_Volume
  (F : in SIO.File_Type;
  DUStart : in Positive_Count;
  BITPIX : Integer;
  NAXISn : NAXIS_Arr;
  First, Last : in NAXIS_Arr;
  Volume : out Tm_Arr;
  Cards : in Optional.Card_Arr)
is

    package F64_Physical_Read is new Physical_Read(Tm, Tm_Arr, Tc, Float_64);
    package F32_Physical_Read is new Physical_Read(Tm, Tm_Arr, Tc, Float_32);
    package I64_Physical_Read is new Physical_Read(Tm, Tm_Arr, Tc, Integer_64);
    package I32_Physical_Read is new Physical_Read(Tm, Tm_Arr, Tc, Integer_32);
    package I16_Physical_Read is new Physical_Read(Tm, Tm_Arr, Tc, Integer_16);
    package U8_Physical_Read  is new Physical_Read(Tm, Tm_Arr ,Tc, Unsigned_8);

begin
  TIO.Put_Line("DBG: V3_Image_Read::Read_Volume");

  -- ? File.Set_File_Block_Index(F, DUStart);

    case(BITPIX) is
      when   8 => U8_Physical_Read.Read_Volume (F,DUStart,NAXISn, First,Last, Volume, Cards);
      when  16 => I16_Physical_Read.Read_Volume(F,DUStart,NAXISn, First,Last, Volume, Cards);
      when  32 => I32_Physical_Read.Read_Volume(F,DUStart,NAXISn, First,Last, Volume, Cards);
      when  64 => I64_Physical_Read.Read_Volume(F,DUStart,NAXISn, First,Last, Volume, Cards);
      when -32 => F32_Physical_Read.Read_Volume(F,DUStart,NAXISn, First,Last, Volume, Cards);
      when -64 => F64_Physical_Read.Read_Volume(F,DUStart,NAXISn, First,Last, Volume, Cards);
      when others => null; -- FIXME Error
    end case;

end Read_Volume;







procedure Write_Volume
  (F : in SIO.File_Type;
  HDUStart : in Positive_Count;
  First, Last : in NAXIS_Arr;
  NAXISn : in NAXIS_Arr;
  Volume : in Tm_Arr)
is
begin
  null;
end Write_Volume;



-- read DU by planes implements sequential read in chunks
procedure Read_Data_Unit_By_Planes
  (F     : in SIO.File_Type;
  BITPIX : in Integer;
  I      : in Positive;
  NAXISn : in NAXIS_Arr;
  Cards  : in Optional.Card_Arr)
is
    function PlaneLength(NAXISi : NAXIS_Arr) return Positive_Count
    is
        Acc : Positive_Count := 1;
    begin
        for I in NAXISi'Range
        loop
         Acc := Acc * NAXISi(I);
        end loop;
        return Acc;
    end PlaneLength;

  PlLength : Positive_Count := PlaneLength(NAXISn(1..I));
  PlCount  : Positive_Count := PlaneLength(NAXISn(I+1..NAXISn'Last));

  Plane : Tm_Arr(1 .. PlLength);

--  function To_V3Type(S : String) return Tc is
--  begin return Tc'Value(S); end To_V3Type;
  -- FIXME hm....! why this...


begin
    TIO.Put_Line("BITPIX : " & Integer'Image(BITPIX));
    TIO.Put_Line("PlLength : " & Positive_Count'Image(PlLength));
    TIO.Put_Line("PlCount  : " & Positive_Count'Image(PlCount));


  for I in 1 .. PlCount
  loop

      case(BITPIX) is
      when -64 => F64_Physical.Read_Array(F, Plane, Cards);
      when -32 => F32_Physical.Read_Array(F, Plane, Cards);
      when  64 => I64_Physical.Read_Array(F, Plane, Cards);
      when  32 => I32_Physical.Read_Array(F, Plane, Cards);
      when  16 => I16_Physical.Read_Array(F, Plane, Cards);
      when   8 => U8_Physical.Read_Array(F, Plane, Cards);
      when others => null; -- FIXME Error
    end case;

    Plane_Data(Plane, I);

  end loop;
  -- FIXME revert: put loop inside case() to avoid check BITPIX at each turn

end Read_Data_Unit_By_Planes;





procedure Write_Data_Unit_By_Planes
    (F : in SIO.File_Type;
    NAXISi : in NAXIS_Arr) -- Tm_Arr has size NAXIS1 .. NAXISi, where i<=NAXISn'Length
is
begin
  null;
end Write_Data_Unit_By_Planes;






end V3_Image_Read;

