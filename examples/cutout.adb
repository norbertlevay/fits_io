
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
--with Ada.Text_IO.Integer_IO; --use Ada.Text_IO.Integer_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Ada.Numerics.Generic_Elementary_Functions; 

with V3_Types; use V3_Types;
with NCube; use NCube;
with NCube_Funcs; use NCube_Funcs;
with Keyword_Record; use Keyword_Record;-- FPositive needed
with Mandatory; use Mandatory; -- Positive_Arr needed
with File; use File;
with Data_Funcs; use Data_Funcs;

with Optional;
with Optional.Reserved; use Optional.Reserved;




procedure cutout
is
 package SIO renames Ada.Streams.Stream_IO;
 package TIO renames Ada.Text_IO;

 package Value_Functions is new Ada.Numerics.Generic_Elementary_Functions (
     Float_32);
use Value_Functions;
 -- instantiate Read Volume func for Float_32

 type VolData is array (SIO.Positive_Count range <>) of Float_32;

 function F32_Is_Valid (V : in Float_32) return Boolean
    is begin return V'Valid; end F32_Is_Valid;

 procedure F32_Read_Volume is 
    new Read_Valid_Scaled_Volume(Float_32, Float_32, VolData, F32_Is_Valid);


 function DU_Count(NAXISn : Positive_Arr) return FNatural
 is
        Cnt : FNatural := 1;
 begin
        for I in NAXISn'Range
        loop
                Cnt := Cnt * NAXISn(I);
        end loop;
        return Cnt;
 end DU_Count;

 -- vars
 
 -- file related
 File : SIO.File_Type;
 HDUStart : SIO.Positive_Count := 1; -- Primary HDU only
 DUStart : SIO.Positive_Count;
 DUSize  : SIO.Positive_Count;
 BITPIX  : Integer;



 -- data related
 NDim : constant FPositive := 2;
 
-- MaxCoords : Coord_Type(1..NDim);-- NAXISn
 First : Coord_Type := ( 1, 1 );
 Last  : Coord_Type := ( 1, 1 );
-- First : Coord_Type := ( 50,    1);
-- Last  : Coord_Type := (100,   31);
 Nx : FPositive := Last(1) - First(1) + 1;
 Ny : FPositive := Last(2) - First(2) + 1;

 BZERO  : Float_32 := 0.0;
 BSCALE : FLoat_32 := 1.0; 
 Undef_Val : Float_32 := Float_32(16#7F801001#);
 Undef_Cnt : Natural := 0;

 F32Value : Float_32;
 F32Min : Float_32 := Float_32'Last;
 F32Max : Float_32 := Float_32'First;
 PosMaxI, PosMaxJ : FInteger;
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
   SIO.Open(File, SIO.In_File, (Argument(1)));
 end if;

 First(1) := FInteger'Value(Argument(2));
 First(2) := FInteger'Value(Argument(3));
 Last(1)  := FInteger'Value(Argument(4));
 Last(2)  := FInteger'Value(Argument(5));

 if (Argument_Count >= 6) then Scaling := Float_32'Value(Argument(6)); end if;

 Nx := Last(1) - First(1) + 1;
 Ny := Last(2) - First(2) + 1;
declare 
 Vol : VolData( 1 .. SIO.Positive_Count(Nx*Ny));-- FIXME FInteger
 UI8ConvVal : Unsigned_8; 
begin

 Set_File_Block_Index(File,HDUStart);

 -- interpret header: DataUnit length and type needed, also store DUStart

 declare
  HDUInfo : HDU_Info_Type := Read_Header(File);
 begin
  DUStart := File_Block_Index(File);
  DUSize  := SIO.Positive_Count(DU_Count (HDUInfo.NAXISn));--FIXME FInteger 
  BITPIX := HDUInfo.BITPIX;
 
 TIO.Put_Line("DU Start [blocks] :" & SIO.Positive_Count'Image(DUStart)); 
 TIO.Put_Line("DU Size  [element count]:" & SIO.Positive_Count'Image(DUSize)); 

 -- reset to Header start and read it again
-- Set_File_Block_Index(File,HDUStart);

 declare
--   Cards : Optional.Card_Arr := Read_Header(File, Optional.Reserved.Array_Keys);
  dummy : Integer;
 begin

 -- read data
 if(BITPIX = -32)
 then 
    F32_Read_Volume(File, BZERO, BSCALE, Undef_Val, DUStart, Coord_Type(HDUInfo.NAXISn), 
                    First, Last, Vol);
 else
    TIO.Put_Line("Not FLoat_32 data in File.");
 end if;
 end;

 end;-- declare begin....

 SIO.Close(File);



 -- work on data
 
 for J in 1 .. (1 + Last(2) - First(2))
 loop
 for I in 1 .. (1 + Last(1) - First(1))
 loop

   OffInVol := SIO.Positive_Count(To_Offset((I,J),(Nx,Ny))); --FIXME FInteger
 -- Put(item => Integer(OffInVol), width => 6);
   F32Value := Vol(OffInVol);

    if(Undef_Val = F32Value) then Undef_Cnt := Undef_Cnt + 1; end if;

   if(F32Min > F32Value) then F32Min := F32Value; end if;
   if(F32Max < F32Value) then F32Max := F32Value; PosMaxI:=I; PosMaxJ:=J; end if;

    F32Value := Scaling * (abs F32Value);
    if(F32Value > 127.0) then F32Value := 126.0; end if;
    UI8Value := 32 + Unsigned_8(F32Value);

    case(UI8Value) is
    when   0 ..  64 => UI8ConvVal := 32; -- space
    when  65 .. 128 => UI8ConvVal := 46; -- dot
    when 129 .. 192 => UI8ConvVal := 43; -- plus
    when 193 .. 255 => UI8ConvVal := 35; -- hash
    end case;

   TIO.Put(Character'Val(UI8Value));
--   TIO.Put(Character'Val(UI8ConvVal));

 end loop;
 TIO.Put_Line("<");
 end loop;

end; -- VolData declare

 TIO.Put_Line("Undef_Cnt : " & Natural'Image(Undef_Cnt));
 TIO.Put_Line("Min : " & Float_32'Image(F32Min));
 TIO.Put_Line("Max : " & Float_32'Image(F32Max));
 TIO.Put_Line("Pos Max : " & FInteger'Image(PosMaxI) & ", " & FInteger'Image(PosMaxJ));


end cutout;

