
-- testing with ../data/NANTEN2/WFPC2ASSNu5780205bx.fits
-- 1      IMAGE     258 (30)      FLOAT32  (  100 x  100 ) 
-- 
-- header blocks: 258/36=7.2 blocks
-- DU size [bytes] : 100 x 100 x 4 = 40000 / 2880 = 13.8 blocks 
-- HDU size: 8 + 14 = 22 blocks = 2880*22 = 63360 bytes
--
-- $ ls -l  ../data/NANTEN2/WFPC2ASSNu5780205bx.fits 
--   63360 Feb 23  2019 ../data/NANTEN2/WFPC2ASSNu5780205bx.fits
--
--
--



with Ada.Text_IO;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Ada.Numerics.Generic_Elementary_Functions; 

with V3_Types; use V3_Types;
with V3_Arrays; use V3_Arrays;
with Raw_Funcs;
with Mandatory; use Mandatory; -- NAXIS_Arr needed
with File; use File;
with File_Funcs; use File_Funcs;

with Optional;
with Optional.Reserved; use Optional.Reserved;
with Header;

with Pool; use Pool;
with Pool_String_To_V3Types; use Pool_String_To_V3Types;
with Pool_V3Type_Convs; use Pool_V3Type_Convs;

with V3_Image_Read;

procedure cutoutdevel
is
 package SIO renames Ada.Streams.Stream_IO;
 package TIO renames Ada.Text_IO;
 
 package Value_Functions is new Ada.Numerics.Generic_Elementary_Functions(Float_32);
 use Value_Functions;


 -- instantiate Read Volume func for Float_32
 package F64_Image is new V3_Image_Read(Float_64, F64_Arr,Float_64);
 package F32_Image is new V3_Image_Read(Float_32, F32_Arr,Float_32);

 
 InFile : SIO.File_Type;
 HDUStart : SIO.Positive_Count := 1;

 -- data related
 
 First : NAXIS_Arr(1..2);-- FIXME use 'First etc...
 Last  : NAXIS_Arr(1..2);-- FIXME instead explicit index def (1..2)

 Nx : SIO.Positive_Count;
 Ny : SIO.Positive_Count;

 BZERO  : Float_32 := 0.0;
 BSCALE : FLoat_32 := 1.0; 
 Undef_Cnt : Natural := 0;

 F32Value : Float_32;
 F32Min : Float_32 := Float_32'Last;
 F32Max : Float_32 := Float_32'First;
 PosMaxI, PosMaxJ : SIO.Positive_Count;
 UI8Value : Unsigned_8;
 OffInVol : SIO.Positive_Count;
 Scaling : Float_32 := 1.0;
begin

 if(Argument_Count < 5 ) 
 then 
  TIO.Put_Line("Usage  " & Command_Name & " <file name> FirstX FirstY LastX LastY [Scaling]");
  TIO.Put_Line(" Conversion: ASCII = Scaling * abs (Value)");
  return;
 else
   SIO.Open(InFile, SIO.In_File, (Argument(1)));
 end if;

 First(1) := SIO.Positive_Count'Value(Argument(2));
 First(2) := SIO.Positive_Count'Value(Argument(3));
 Last(1)  := SIO.Positive_Count'Value(Argument(4));
 Last(2)  := SIO.Positive_Count'Value(Argument(5));

 if (Argument_Count >= 6) then Scaling := Float_32'Value(Argument(6)); end if;

 Nx := 1 + Last(1) - First(1);
 Ny := 1 + Last(2) - First(2);

declare 
 VolF64 : F64_Arr( 1 .. Raw_Funcs.Volume_Length(First,Last));
 Vol : F32_Arr( 1 .. Raw_Funcs.Volume_Length(First,Last));
 HDUInfo : File.HDU_Info_Type := File.Read_Header(InFile);
begin

 Set_File_Block_Index(InFile,HDUStart);

 declare
  Cards   : Optional.Card_Arr := Header.Read_Optional(InFile, Optional.Reserved.Reserved_Keys);
  DUStart : SIO.Positive_Count;
 begin

 DUStart := File_Block_Index(InFile);
-- F64_Image.Read_Volume(InFile, DUStart, HDUInfo.BITPIX, HDUInfo.NAXISn, First, Last, VolF64, Cards);
F32_Image.Read_Volume(InFile, DUStart, HDUInfo.BITPIX, HDUInfo.NAXISn, First, Last, Vol, Cards);
 -- FIXME above proc should raise exception if File::HDU is not BITPIX=-32

 SIO.Close(InFile);



 -- work on data

 for J in 1 .. Ny
 loop
 for I in 1 .. Nx
 loop

   OffInVol := Raw_Funcs.To_DU_Index((I,J),(Nx,Ny));
   F32Value := Vol(OffInVol);

   if(F32Value = F32NaN OR F32Value'Valid /= True)
   then
     Undef_Cnt := Undef_Cnt + 1;
     TIO.Put(' ');
   else
    if(F32Min > F32Value) then F32Min := F32Value; end if;
    if(F32Max < F32Value) then F32Max := F32Value; PosMaxI:=I; PosMaxJ:=J; end if;

    F32Value := Scaling * (abs F32Value);
    if(F32Value > (127.0 - 35.0)) then F32Value := 126.0 - 35.0; end if;

    UI8Value :=  32 + Unsigned_8(F32Value);
    TIO.Put(Character'Val(UI8Value));
   end if;

 end loop;
 TIO.Put_Line("<");
 end loop;

end; -- Card read declare
end; -- F32_Arr declare

 TIO.Put_Line("Undef_Cnt : " & Natural'Image(Undef_Cnt));
 TIO.Put_Line("Min : " & Float_32'Image(F32Min));
 TIO.Put_Line("Max : " & Float_32'Image(F32Max));
 TIO.Put_Line("Max Pos : " & SIO.Positive_Count'Image(PosMaxI) & ", " & SIO.Positive_Count'Image(PosMaxJ));


end cutoutdevel;


-- NOTES:
--
  --Put(item => Integer(I), width => 4);
  --Put(item => Integer(J), width => 4);
  --Put(item => Integer(OffInVol), width => 6);
 
--    case(UI8Value) is
--    when   0 ..  64 => UI8ConvVal := 32; -- space
--    when  65 .. 128 => UI8ConvVal := 46; -- dot
--    when 129 .. 192 => UI8ConvVal := 43; -- plus
--    when 193 .. 255 => UI8ConvVal := 35; -- hash
--    end case;
--   TIO.Put(Character'Val(UI8ConvVal));

