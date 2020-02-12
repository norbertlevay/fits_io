
with Ada.Text_IO;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
--with Ada.Text_IO.Integer_IO; --use Ada.Text_IO.Integer_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line; use Ada.Command_Line;

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
 Nx : constant FPositive := 90;
 Ny : constant FPositive := 90;

-- MaxCoords : Coord_Type(1..NDim);-- NAXISn
 First : Coord_Type := (1,1);
 Last  : Coord_Type := (Nx,Ny);

 BZERO  : Float_32 := 0.0;
 BSCALE : FLoat_32 := 1.0; 
 Undef_Val : Float_32 := Float_32(16#7F801001#);

 Vol : VolData( 1 .. Positive(Nx*Ny)); 

 F32Value : Float_32;
 UI8Value : Unsigned_8;
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

   F32Value := (Vol(Integer(To_Offset((I,J),(Nx,Ny)))));
   --Put ((Integer(To_Offset((J,I),(Ny,Nx)))));
--   UI8Value := Unsigned_8(abs F32Value);
   UI8Value := Unsigned_8(0.2 * (2.0 + F32Value));

   TIO.Put(Character'Val( 0 +  UI8Value  ));
  -- Put(item =>Integer(UI8Value) , width => 3);
--   TIO.Put(Float_32'Image(F32Value));

 end loop;
   TIO.New_Line;
 end loop;

end cutout;

