--
-- Example convert data to Float_64 type
--
-- FIXME: assume Primary HDU. What if othe HDU is IMAGE type too?
-- FIXME: Float_64 no Endianness fix, use Float_32 now
--
-- demonstrate usage if data unit is "big":
-- all data will not fit into memory, needs to be processed
-- in chunks.


with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;

with Ada.Streams.Stream_IO;

with FITS;   use FITS;
with File;   use File;
with Keyword_Record; use Keyword_Record;
with Strict; use Strict; -- Positive_Arr needed

procedure convert
is

 package SIO renames Ada.Streams.Stream_IO;

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


 OutFileName : constant String := Command_Name & ".fits";
 OutFile     : SIO.File_Type;

 InFile      : SIO.File_Type;
 InFileName  : String := Argument(1);
 -- FIXME might raise excpetion before Usage written

 Card   : Card_Type;
 BITPIX : Integer;
 DUSize : FPositive;

 BufferSize : constant FPositive := 4*1024;
 InBuffer   : UInt8_Arr  (1 .. BufferSize);
 OutBuffer  : Float32_Arr(1 .. BufferSize);

 procedure Read_Data  is new gen_Read_Data (Data_Arr => UInt8_Arr);
 procedure Write_Data is new gen_Write_Data(Data_Arr => Float32_Arr);

 BITPIXFloat64Card : Card_Type :=
"BITPIX  =                   -32 / Standard FITS FIle                            ";

 Nb   : FPositive;
 Nrem : FNatural;

 LastWrittenIdx : SIO.Positive_Count;
begin

 Put_Line("Usage  " & Command_Name & " <file name>");
 Put("Writing " & OutFileName & " ... ");

 SIO.Open   (InFile,  SIO.In_File,  InFileName);
 SIO.Create (OutFile, SIO.Out_File, OutFileName);
 -- FIXME check behaviour AdaRM: overwrites if file already exists ?
 -- FIXME if AdaRM says SIO.Create guarantees File Index
 -- to be 1 after Create ? Otherwise call Set_Index(File,1)

 -- FIXME now only Primary HDU, later consider other HDUs

 -- interpret header: DataUnit length and type needed
 declare
  HDUInfo : HDU_Info_Type := Get(InFile);
 begin
  BITPIX := HDUInfo.BITPIX;
  DUSize := DU_Count (HDUInfo.NAXISn);
 end;

 -- write Header

 Set_Index(InFile, 1);
 loop
  -- Card := Read_Card(InFile);
  Card_Type'Read(SIO.Stream(InFile),Card);

  if(Card(1..6) = "BITPIX") then
    -- Write_Card(OutFile, BITPIXFloat64Card);
    Card_Type'Write(SIO.Stream(OutFile),BITPIXFloat64Card);
  else
    -- Write_Card(OutFile, Card);
    Card_Type'Write(SIO.Stream(OutFile),Card);
  end if;
  exit when Card = ENDCard;
 end loop;
 LastWrittenIdx := SIO.Index(OutFile);
 -- Index to StreamElement (after) the last written one

 Write_Padding(OutFile, LastWrittenIdx, HeaderPadValue);

 -- write Data

 -- FIXME this below should be done 6x for every possible
 --       data (BITPIX) in InFile -> use generic?!!

 Nb   := DUSize / BufferSize;
 Nrem := DUSize rem BufferSize;

 for I in 1 .. Nb
 loop
  Read_Data(InFile,InBuffer);
  -- convert
  for I in InBuffer'Range
  loop
   OutBuffer(I) := Float_32(InBuffer(I));
  end loop;
  Write_Data(OutFile,OutBuffer);
 end loop;

 Read_Data(InFile,InBuffer(1..Nrem));
 -- convert
 for I in 1..Nrem
 loop
  OutBuffer(I) := Float_32(InBuffer(I));
 end loop;
 Write_Data(OutFile,OutBuffer(1..Nrem));
 LastWrittenIdx := SIO.Index(OutFile);
 -- Index to StreamElement (after) the last written one

 Write_Padding(OutFile, LastWrittenIdx, DataPadValue);

 SIO.Close(OutFile);
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
end convert;

