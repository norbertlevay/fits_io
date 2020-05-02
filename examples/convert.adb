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

with Ada.Streams.Stream_IO;

with File;   use File;
with File_Funcs;   use File_Funcs;--Data_Unit_Size_elems() needed
with File.Misc;   use File.Misc;
with Keyword_Record; use Keyword_Record;
with Mandatory; use Mandatory; -- NAXIS_Arr needed

-- new Data interface
with V3_Types; use V3_Types;
--with Data_Funcs; use Data_Funcs;
with Image; -- Create_Card needed
with Raw;

procedure convert
is

 package TIO renames Ada.Text_IO;
 package SIO renames Ada.Streams.Stream_IO;
 use SIO;


 OutFileName : constant String := Command_Name & ".fits";
 OutFile     : SIO.File_Type;

 InFile      : SIO.File_Type;
 InFileName  : String := Argument(1);
 -- FIXME might raise excpetion before Usage written

 Card   : String_80;
 BITPIX : Integer;
 DUSize : SIO.Positive_Count;

 BITPIXnewCard : String_80 := Image.Create_Card("BITPIX", "16");
 BSCALECard    : String_80 := Image.Create_Card("BSCALE",  "0.003891051");
 BZEROCard     : String_80 := Image.Create_Card("BZERO" ,"127.501945525");

 -- FIXME find Data Min Max and calc BSCALE BZERO (set BLANK if necessary)

 LastWrittenIdx : SIO.Positive_Count;
begin

 Put_Line("Usage  " & Command_Name & " <file name>");
 Put("Writing " & OutFileName & " ... ");

 SIO.Open   (InFile,  SIO.In_File,  InFileName);
 SIO.Create (OutFile, SIO.Out_File, OutFileName);


 -- Copy Header
 -- but replace BITPIX card and insert BZERO BSCALE
 -- cards after DATAMAX card

 Set_Index(InFile, Positive(1));
 loop
  String_80'Read(SIO.Stream(InFile),Card);

  if(Card(1..6) = "BITPIX") then
    String_80'Write(SIO.Stream(OutFile),BITPIXnewCard);
  else
    String_80'Write(SIO.Stream(OutFile),Card);
  end if;
 
  if(Card(1..7) = "DATAMAX") then
    String_80'Write(SIO.Stream(OutFile),BZEROCard);
    String_80'Write(SIO.Stream(OutFile),BSCALECard);
  end if; 

 exit when Card = ENDCard;
 end loop;

 LastWrittenIdx := SIO.Index(OutFile);
 Write_Padding(OutFile, LastWrittenIdx, HeaderPadValue);
 -- NOTE also make sure InFile Padding is skipped: now 
 -- Get() reads by Blocks



 SIO.Set_Index(InFile,1);
 -- interpret header: DataUnit length and type needed
 declare
  HDUInfo : HDU_Info_Type := Read_Header(InFile);
 begin
  BITPIX := HDUInfo.BITPIX;
  DUSize := Data_Unit_Size_elems (HDUInfo.NAXISn);
-- end;

 -- Read Write sequentially by Blocks

 declare
  type F32_Arr is array (SIO.Positive_Count range <>) of Float_32;
  type I16_Arr is array (SIO.Positive_Count range <>) of Integer_16;
  package F32_Raw is new Raw(Float_32,   F32_Arr);
  package I16_Raw is new Raw(Integer_16, I16_Arr);

  K : SIO.Positive_Count := 1 + 2880/4;
  InBlock  : F32_Arr(1 .. 2880/4);
  --OutBlock : I16_Arr(1 .. 2880/2);
  --FIXME consider: buffer/block size define inside Raw.Write_Data_Unit !?

  BSCALE : constant V3_Types.Float_32 :=   0.003891051;
  BZERO  : constant V3_Types.Float_32 := 127.501945525;


  procedure ConvertData(Block : out I16_Arr)
  is
    F32Value : Float_32;
    I16Value : Integer_16;
  begin
    for I in Block'Range
    loop

      if(K > 2880/4)
      then
        F32_Raw.Read_Array(InFile, InBlock);
        K := 1;
      end if;
      F32Value := InBlock(K);
      K := K + 1;

      -- scale and convert
      if(F32Value > 255.0) then F32Value := 255.0; end if;
--      F32Value  := (F32Value - BZERO) / BSCALE; -- FIXME why this ??
      I16Value := Integer_16(F32Value);

      Block(I) := I16Value;
    end loop;
  end ConvertData;

  procedure I16_Write_DU is new I16_Raw.Write_Data_Unit(0,ConvertData);


 begin
  I16_Write_DU(OutFile, HDUInfo.NAXISn);
 end;--read Raw
 end;--read HDUInfo

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

