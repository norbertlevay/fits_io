
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
with Strict; use Strict; -- Positive_Arr needed
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

 type VolData is array (Positive range <>) of Float_32;

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
 HDUStart : Positive := 1; -- Primary HDU only
 DUStart : Positive;
 DUSize  : Positive;
 BITPIX  : Integer;



 -- data related
 NDim : constant FPositive := 2;
 Nx : constant FPositive := 100;
 Ny : constant FPositive := 100;

-- MaxCoords : Coord_Type(1..NDim);-- NAXISn
 First : Coord_Type := (1,1);
 Last  : Coord_Type := (Nx,Ny);

 BZERO  : Float_32 := 0.0;
 BSCALE : FLoat_32 := 1.0; 
 Undef_Val : Float_32 := Float_32(16#7F801001#);

 Vol : VolData( 1 .. Positive(Nx*Ny)); 

 F32Value : Float_32;
 F32Min : Float_32 := Float_32'Last;
 F32Max : Float_32 := Float_32'First;
 UI8Value : Unsigned_8;
 IValue : Integer;
begin


 if(Argument_Count /= 1 ) 
 then 
  TIO.Put_Line("Usage  " & Command_Name & " <file name>");
  return;
 else
   SIO.Open(File, SIO.In_File, (Argument(1)));
 end if;

 Set_File_Block_Index(File,HDUStart);

 -- interpret header: DataUnit length and type needed, also store DUStart

 declare
  HDUInfo : HDU_Info_Type := Read_Header(File);
 begin
  DUStart := File_Block_Index(File);
  DUSize  := Positive(DU_Count (HDUInfo.NAXISn));
  BITPIX := HDUInfo.BITPIX;
 
 TIO.Put_Line("DU Start [blocks] :" & Positive'Image(DUStart)); 
 TIO.Put_Line("DU Size  [element count]:" & Positive'Image(DUSize)); 

 -- reset to Header start and read it again
-- Set_File_Block_Index(File,HDUStart);

 declare
--   Cards : Optional.Card_Arr := Read_Header(File, Optional.Reserved.Array_Keys);
  dummy : Integer;
 begin

 -- read data
 if(BITPIX = -32)
 then 
	F32_Read_Volume(File, BZERO, BSCALE, Undef_Val, DUStart, Coord_Type(HDUInfo.NAXISn), First, Last, Vol);
 else
	TIO.Put_Line("Not FLoat_32 data in File.");
 end if;
 end;

 end;-- declare begin....

 SIO.Close(File);



 -- work on data
 
 for I in First(1) .. Last(1)
 loop
 for J in First(2) .. Last(2)
 loop

   F32Value := Vol(Integer(To_Offset((I,J),(Nx,Ny))));

   --F32Value := Log(abs F32Value);

   if(F32Min > F32Value) then F32Min := F32Value; end if;
   if(F32Max < F32Value) then F32Max := F32Value; end if;

   --IValue := Integer(1000.0*Log(abs F32Value));
--   UI8Value := Unsigned_8(58.0 + F32Value);
   --if(F32Value > 255.0) then F32Value := 255.0; end if;

    F32Value := 0.1 * (0.0 + abs F32Value);
    if(F32Value > 255.0) then F32Value := 255.0; end if; 
    UI8Value := 32 + Unsigned_8(F32Value);

   TIO.Put(Character'Val(UI8Value));
   --Put(item =>Integer(UI8Value) , width => 3);
--   Put(item =>Integer(IValue) , width => 3);
--   TIO.Put(Float_32'Image(F32Value));

 end loop;
   TIO.Put_Line("<");
 end loop;

 TIO.Put_Line("Min : " & Float_32'Image(F32Min));
 TIO.Put_Line("Max : " & Float_32'Image(F32Max));


end cutout;

