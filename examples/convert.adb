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
with Mandatory; use Mandatory; -- NAXIS_Arr needed

-- new Data interface
with V3_Types; use V3_Types;
with Data_Funcs; use Data_Funcs;

procedure convert
is

 package TIO renames Ada.Text_IO;
 package SIO renames Ada.Streams.Stream_IO;
 use SIO;

 function DU_Count(NAXISn : NAXIS_Arr) return FNatural
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
"BITPIX  =                   16 /                                                ";
 BSCALECard : Card_Type :=
"BSCALE  =          0.003891051 / floating point value                           ";
 BZEROCard : Card_Type :=
"BZERO   =        127.501945525 / floating point value                           ";
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
 
  if(Card(1..7) = "DATAMAX") then
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
  InBlock  : V3_Types.F32.Block;
  OutBlock : V3_Types.Int16.Block;
  DUSize_blocks : constant SIO.Positive_Count
            := DU_Block_Index(SIO.Positive_Count(DUSize),4);--4->Float_32 InFile --FIXME FInteger
  -- NOTE DUSize = 0 -> raise excpetion -> correct: dont call this if there is no data
  Last_Data_Element_In_Block : constant Positive := 
        Offset_In_Block(SIO.Positive_Count(DUSize), SIO.Positive_Count(V3_Types.F32.N));-- FIXME FInteger
  F32Value : V3_Types.Float_32;
  I16Value : V3_Types.Integer_16;
  L : Positive := 1;

  BSCALE : constant V3_Types.Float_32 :=   0.003891051;
  BZERO  : constant V3_Types.Float_32 := 127.501945525;
  F32Temp : V3_Types.Float_32;
 begin
    for I in 1 .. (DUSize_Blocks - 1)
    loop
        F32.Block'Read(SIO.Stream(InFile),InBlock);
        for K in InBlock'Range
        loop
            F32Value := InBlock(K);
            
            -- convert
            
            F32Temp  := (F32Value - BZERO) / BSCALE;
            I16Value := V3_Types.Integer_16( F32Temp );
            -- FIXME not correct: needs Data Min..Max: DATAMIN DATAMAX cards or from DU
            -- calculate BZERO BSCALE (and BLANK if needed)

--          Put_Line(
--              V3_Types.Float_32'Image(F32Value) 
--              &" vs "& V3_Types.Float_32'Image(F32Temp) 
--              &" vs "& V3_Types.Integer_16'Image(I16Value));

            -- store converted
            OutBlock(L) := I16Value;
            L := L + 1;

            if L > OutBlock'Last 
            then
                Int16.Block'Write(SIO.Stream(OutFile),OutBlock);
                L := 1;
            end if;

        end loop;
    end loop;

    -- Last Block of InFIle
    
    F32.Block'Read(SIO.Stream(InFile),InBlock);
    for K in 1 .. Last_Data_Element_In_Block
    loop
        F32Value := InBlock(K);
            
        -- convert
        F32Temp  := (F32Value - BZERO) / BSCALE;
        I16Value := V3_Types.Integer_16(F32Temp);
        -- FIXME not correct: needs Data Min..Max: DATAMIN DATAMAX cards or from DU
        -- calculate BZERO BSCALE (and BLANK if needed)

        -- store converted
        OutBlock(L) := I16Value;
        L := L + 1;

        if L > OutBlock'Last 
        then
            Int16.Block'Write(SIO.Stream(OutFile),OutBlock);
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
        Int16.Block'Write(SIO.Stream(OutFile),OutBlock);
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

