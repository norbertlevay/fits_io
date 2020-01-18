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
with File.Misc;   use File.Misc;
with Keyword_Record; use Keyword_Record;
with Strict; use Strict; -- Positive_Arr needed

-- new Data interface
with Data_Types; use Data_Types;
with Data_Funcs; use Data_Funcs;

procedure convert
is

 package SIO renames Ada.Streams.Stream_IO;
 use SIO;

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

 BITPIXnewCard : Card_Type :=
"BITPIX  =                   16 / Standard FITS FIle                             ";
 BZEROCard : Card_Type :=
"BZERO   =                    0 / Standard FITS FIle                             ";
 BSCALECard : Card_Type :=
"BSCALE  =                  1.0 / Standard FITS FIle                             ";
-- FIXME find Data Min Max and calc BSCALE BZERO (set BLANK if necessary)


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

 -- write Header

 Set_Index(InFile, Positive(1));
 loop
  Card_Type'Read(SIO.Stream(InFile),Card);

  if(Card(1..6) = "BITPIX") then
    Card_Type'Write(SIO.Stream(OutFile),BITPIXnewCard);
  else
    Card_Type'Write(SIO.Stream(OutFile),Card);
  end if;
 
  if(Card(1..6) = "NAXIS2") then
    Card_Type'Write(SIO.Stream(OutFile),BZEROCard);
    Card_Type'Write(SIO.Stream(OutFile),BSCALECard);
  end if; 


 exit when Card = ENDCard;
 end loop;
 LastWrittenIdx := SIO.Index(OutFile);
 -- Index to StreamElement (after) the last written one

 Write_Padding(OutFile, LastWrittenIdx, HeaderPadValue);
 -- NOTE also make sure InFile Padding is skipped: now 
 -- Get() reads by Blocks

 SIO.Set_Index(InFile,1);
 -- interpret header: DataUnit length and type needed
 declare
  HDUInfo : HDU_Info_Type := Read_Header(InFile);
 begin
  BITPIX := HDUInfo.BITPIX;
  DUSize := DU_Count (HDUInfo.NAXISn);
 end;

 -- read write sequentially by Blocks

 declare
  InBlock  : Data_Types.F32.Data.Block;
  OutBlock : Data_Types.Int16.Data.Block;
  DUSize_blocks : constant Positive := DU_Block_Index(Positive(DUSize),4);-- 4 ->Float_32 InFile
  -- NOTE DUSize = 0 -> raise excpetion -> correct: dont call this if there is no data
  Last_Data_Element_In_Block : constant Positive := Offset_In_Block(Positive(DUSize), 4);
  F32Value : Data_Types.Float_32;
  I16Value : Data_Types.Integer_16;
  L : Positive := 1;
 begin
	for I in 1 .. (DUSize_Blocks - 1)
	loop
		F32.Data.Block'Read(SIO.Stream(InFile),InBlock);
		for K in InBlock'Range
		loop
			F32Value := InBlock(K);

			-- convert
			I16Value := Data_Types.Integer_16(F32Value);
			-- FIXME not correct: needs Data Min..Max: DATAMIN DATAMAX cards or from DU
			-- calculate BZERO BSCALE (and BLANK if needed)

			-- store converted
			OutBlock(L) := I16Value;
			L := L + 1;

			if L > OutBlock'Last 
			then
				Int16.Data.Block'Write(SIO.Stream(OutFile),OutBlock);
				L := 1;
			end if;

		end loop;
	end loop;

	-- Last Block of InFIle
	
	F32.Data.Block'Read(SIO.Stream(InFile),InBlock);
	for K in 1 .. Last_Data_Element_In_Block
	loop
		F32Value := InBlock(K);
			
		-- convert
		I16Value := Data_Types.Integer_16(F32Value);
		-- FIXME not correct: needs Data Min..Max: DATAMIN DATAMAX cards or from DU
		-- calculate BZERO BSCALE (and BLANK if needed)

		-- store converted
		OutBlock(L) := I16Value;
		L := L + 1;

		if L > OutBlock'Last 
		then
			Int16.Data.Block'Write(SIO.Stream(OutFile),OutBlock);
			L := 1;
		end if;

	end loop;

	-- write padding if needed
	if L /= 1
	then 
		for LL in L .. OutBlock'Last
		loop
			OutBlock(LL) := 0;
		end loop;
		Int16.Data.Block'Write(SIO.Stream(OutFile),OutBlock);
	end if;
 end;

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

