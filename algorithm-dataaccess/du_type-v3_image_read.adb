
with Ada.Text_IO; --use Ada.Text_IO;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed
with Mandatory; use Mandatory;-- NAXIS_Arr needed
with Optional;
with Optional.Reserved;
with Header; use Header;
with File;
with V3_Types; use V3_Types;-- types needed

with DU_Type.Physical;

package body DU_Type.V3_Image_Read is

    package TIO renames Ada.Text_IO;


    package F64_Physical is new Physical(Float_64);
    package F32_Physical is new Physical(Float_32);
    package I64_Physical is new Physical(Integer_64);
    package I32_Physical is new Physical(Integer_32);
    package I16_Physical is new Physical(Integer_16);
    package U8_Physical  is new Physical(Unsigned_8);




procedure Read_Volume
  (F : in SIO.File_Type;
  DUStart : in Positive_Count;
  BITPIX : Integer;
  NAXISn : NAXIS_Arr;
  First, Last : in NAXIS_Arr;
  Volume : out Tm_Arr;
  Cards : in Optional.Card_Arr)
is
    UValid : Boolean := False;
    UValue : Tm;
    A : Tc := 0.0;
    B : Tc := 1.0;
    UIn_Valid : Boolean := False;
begin
  TIO.Put_Line("DBG: V3_Image_Read::Read_Volume");

  -- ? File.Set_File_Block_Index(F, DUStart);

  case(BITPIX) is
      when -64 =>
          declare
              UIn_Value : Float_64;
          begin
            F64_Physical.Header_Info(Cards, A, B, UIn_Valid, UIn_Value);
            F64_Physical.Init_Undef_For_Read(UIn_Valid, UIn_Value, UValid, UValue);
            F64_Physical.Read_Volume(F,DUStart,NAXISn, First,Last, Volume, A,B);
          end;

      when -32 =>
          declare
              UIn_Value : Float_32;
          begin
            F32_Physical.Header_Info(Cards, A, B, UIn_Valid, UIn_Value);
            F32_Physical.Init_Undef_For_Read(UIn_Valid, UIn_Value, UValid, UValue);
            F32_Physical.Read_Volume(F,DUStart,NAXISn, First,Last, Volume, A,B);
          end;

      when  64 =>
          declare
              UIn_Value : Integer_64;
          begin
            I64_Physical.Header_Info(Cards, A, B, UIn_Valid, UIn_Value);
            I64_Physical.Init_Undef_For_Read(UIn_Valid, UIn_Value, UValid, UValue);
            I64_Physical.Read_Volume(F,DUStart,NAXISn, First,Last, Volume, A,B);
          end;

      when  32 =>
          declare
              UIn_Value : Integer_32;
          begin
            I32_Physical.Header_Info(Cards, A, B, UIn_Valid, UIn_Value);
            I32_Physical.Init_Undef_For_Read(UIn_Valid, UIn_Value, UValid, UValue);
            I32_Physical.Read_Volume(F,DUStart,NAXISn, First,Last, Volume, A,B);
          end;

      when  16 =>
          declare
              UIn_Value : Integer_16;
          begin
            I16_Physical.Header_Info(Cards, A, B, UIn_Valid, UIn_Value);
            I16_Physical.Init_Undef_For_Read(UIn_Valid, UIn_Value, UValid, UValue);
            I16_Physical.Read_Volume(F,DUStart,NAXISn, First,Last, Volume, A,B);
          end;

      when   8 =>
          declare
              UIn_Value : Unsigned_8;
          begin
            U8_Physical.Header_Info(Cards, A, B, UIn_Valid, UIn_Value);
            U8_Physical.Init_Undef_For_Read(UIn_Valid, UIn_Value, UValid, UValue);
            U8_Physical.Read_Volume(F,DUStart,NAXISn, First,Last, Volume, A,B);
          end;

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
  Undef_Value : in out Tm;
  Undef_Valid : in out Boolean;
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
  A : Tc := 0.0;
  B : Tc := 1.0;
  UIn_Valid : Boolean := False;
begin
    TIO.Put_Line("BITPIX : " & Integer'Image(BITPIX));
    TIO.Put_Line("PlLength : " & Positive_Count'Image(PlLength));
    TIO.Put_Line("PlCount  : " & Positive_Count'Image(PlCount));


  for I in 1 .. PlCount
  loop

      case(BITPIX) is
      when -64 =>
          declare
              UIn_Value : Float_64;
          begin
            F64_Physical.Header_Info(Cards, A, B, UIn_Valid, UIn_Value);
            F64_Physical.Init_Undef_For_Read(UIn_Valid,UIn_Value, Undef_Valid,Undef_Value);
            F64_Physical.Read_Array(F, Plane, A,B);
          end;

      when -32 =>
          declare
              UIn_Value : Float_32;
          begin
            F32_Physical.Header_Info(Cards, A, B, UIn_Valid, UIn_Value);
            F32_Physical.Init_Undef_For_Read(UIn_Valid,UIn_Value, Undef_Valid,Undef_Value);
            F32_Physical.Read_Array(F, Plane, A,B);
          end;

      when  64 =>
          declare
              UIn_Value : Integer_64;
          begin
            I64_Physical.Header_Info(Cards, A, B, UIn_Valid, UIn_Value);
            I64_Physical.Init_Undef_For_Read(UIn_Valid,UIn_Value, Undef_Valid,Undef_Value);
            I64_Physical.Read_Array(F, Plane, A,B);
          end;

      when  32 =>
          declare
              UIn_Value : Integer_32;
          begin
            I32_Physical.Header_Info(Cards, A, B, UIn_Valid, UIn_Value);
            I32_Physical.Init_Undef_For_Read(UIn_Valid,UIn_Value, Undef_Valid,Undef_Value);
            I32_Physical.Read_Array(F, Plane, A,B);
          end;

      when  16 =>
          declare
              UIn_Value : Integer_16;
          begin
            I16_Physical.Header_Info(Cards, A, B, UIn_Valid, UIn_Value);
            I16_Physical.Init_Undef_For_Read(UIn_Valid,UIn_Value, Undef_Valid,Undef_Value);
            I16_Physical.Read_Array(F, Plane, A,B);
          end;
      when   8 =>
          declare
              UIn_Value : Unsigned_8;
          begin
            U8_Physical.Header_Info(Cards, A, B, UIn_Valid, UIn_Value);
            U8_Physical.Init_Undef_For_Read(UIn_Valid,UIn_Value, Undef_Valid,Undef_Value);
            U8_Physical.Read_Array(F, Plane, A,B);
          end;
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






end DU_Type.V3_Image_Read;

