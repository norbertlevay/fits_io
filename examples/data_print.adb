

-- TODO make this run for any FITS file of any data type (BITPIX value)


with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;

with Ada.Streams.Stream_IO;

with File;   use File;
with File.Misc;   use File.Misc;
with Keyword_Record; use Keyword_Record;
with Strict; use Strict; -- Positive_Arr needed

-- for data scaling
with Optional;
with Optional.Reserved;

with Keyword_Record;

with Data_Funcs; use Data_Funcs;
with V3_Types; use V3_Types;

procedure data_print
is
 package KW  renames Keyword_Record;
 package TIO renames Ada.Text_IO;
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


 InFile      : SIO.File_Type;

 BITPIX : Integer;
 DUSize : FPositive;

 Off_In_Block : Positive;
 Off_In_DU : Positive;
 DU_Start  : Positive;

 BlockF32 : F32.Block;

 subtype Key_Type   is String(1..8);
 subtype Value_Type is String(1..20);
 Key   : Key_Type;
 Value : Value_Type;

 type Phys_Value_Scaling is 
	record
	BZERO  : Float;
	BSCALE : Float;
	BUNIT  : Value_Type;
	BLANK  : Integer;
	DATAMIN : Float;
	DATAMAX : Float;	
	end record;


 PhysValScale : Phys_Value_Scaling;

 procedure Put_Phys_Value_Scaling(R : Phys_Value_Scaling)
 is
 begin
	TIO.Put_Line("BZERO   " & Float'Image(R.BZERO));
	TIO.Put_Line("BSCALE  " & Float'Image(R.BSCALE));
	TIO.Put_Line("BUNIT   " & R.BUNIT);
	TIO.Put_Line("BLANK   " & Integer'Image(R.BLANK));
	TIO.Put_Line("DATAMIN " & Float'Image(R.DATAMIN));
	TIO.Put_Line("DATAMAX " & Float'Image(R.DATAMAX));
 end Put_Phys_Value_Scaling;

begin

 if(Argument_Count /= 1) 
 then 
 	Put_Line("Usage  " & Command_Name & " <file name>");
	return;
 end if;

 SIO.Open   (InFile,  SIO.In_File,  Argument(1));

 Put_Line("After Open: " & Positive'Image( File_Block_Index(InFile)));

 -- parse Header
 
 SIO.Set_Index(InFile,1);
 -- interpret header: DataUnit length and type needed
 declare
  HDUInfo : HDU_Info_Type := Read_Header(InFile);
 begin
  BITPIX := HDUInfo.BITPIX;
  DUSize := DU_Count (HDUInfo.NAXISn);
 end;
 
 Put_Line("BITPIX: " & Integer'Image(BITPIX));
 Put_Line("DUSize: " & FInteger'Image(DUSize));

 DU_Start := File_Block_Index(InFile);
 Put_Line("After ReadHeader MandatoryKeys: " & Positive'Image( DU_Start ));

 SIO.Set_Index(InFile,1);
 -- parse data scaling keys (BSCALE BZERO BLANK): needed if BITPIX indicates Integer values
 -- use of scaling keys for FloatingPoint values is not recommended by standard
 -- DATAMIN DATAMAX BUNIT applicable always 
 declare
  Cards : Optional.Card_Arr := 
		Read_Header(InFile, Optional.Reserved.Array_Keys);
 begin
  for I in Cards'Range
  loop
	Key := Cards(I)(1 .. 8);
	Value := Cards(I)(11 .. 30);

	-- FIXME what if some Key not present ?
	if(Key = "BSCALE  ")    then PhysValScale.BSCALE := KW.To_Float(Value);
	elsif(Key = "BZERO   ") then PhysValScale.BZERO  := KW.To_Float(Value);
	elsif(Key = "BUNIT   ") then PhysValScale.BUNIT  := Value; -- FIXME should allow Strings of any length (up to 70 chars)
	elsif(Key = "BLANK   ") then PhysValScale.BLANK  := Integer(KW.To_Integer(Value));
	elsif(Key = "DATAMIN ") then PhysValScale.DATAMIN := KW.To_Float(Value);
	elsif(Key = "DATAMAX ") then PhysValScale.DATAMAX := KW.To_Float(Value);
	end if;
  end loop;
 end;
 Put_Phys_Value_Scaling(PhysValScale);


 DU_Start := File_Block_Index(InFile);
 Put_Line("After ReadHeader ReservedKeys: " & Positive'Image( DU_Start ));

 -- position to and read one Data Block

 Off_In_DU := DU_Block_Index(1000, abs(BITPIX));   -- <---- BITPIX dependent 
 Set_File_Block_Index(InFile, DU_Start + Off_In_DU);

 Put_Line("After SetFileBlockIndex: " & Positive'Image( File_Block_Index(InFile)));

 -- read and use one block data

 F32.Block'Read(SIO.Stream(InFile), BlockF32); -- <----- BITPIX dependent

 Put_Line("After BlockT'Read: " & Positive'Image( File_Block_Index(InFile)));

 -- print all block
 for I in BlockF32'Range
 loop
   null; -- FIXME F32.Physical_Value is generic now needs instance...
--	Put(Positive'Image(I) & ":" 
--		& V3_Types.Float_32'Image(BlockF32(I))
--		& " / " & V3_Types.Float_32'Image( V3_Types.F32.Physical_Value
--					(V3_Types.Float_32(PhysValScale.BZERO), V3_Types.Float_32(PhysValScale.BSCALE), BlockF32(I))   ) ); -- <----- BITPIX dependent
 end loop;
 New_Line;
 
 -- access & print element with Index 1000
 Off_In_Block := Offset_In_Block(1000, F32.N); 
 Put_Line(Positive'Image(Off_In_Block) & ":" & V3_Types.Float_32'Image(BlockF32(Off_in_block))); -- <----- BITPIX dependent


 SIO.Close(InFile);



-- -----------------------------------------------------------------------
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
end data_print;

