
with Ada.Text_IO;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with FITSlib.HDU;
with FITSlib.Header; use FITSlib.Header;

package body FITSlib.File is

  package TIO renames Ada.Text_IO;



  function HDUSIO_File_Next(File : SIO.File_Type) return Card_Block
  is 
   HBlk : Card_Block; 
  begin
   Card_Block'Read(Stream(File), HBlk); 
   return HBlk; 
  end HDUSIO_File_Next; 


  package SIO_HDU is new FITSlib.HDU
	  (Source_Type =>  SIO.File_Type, 
	   Sink_Type   =>  SIO.File_Type, 
	   Next        =>  HDUSIO_File_Next); 


  function Peek (File : in SIO.File_Type) return HDU_Variant
  is
	  OrigIndex : SIO.Positive_Count := SIO.Index(File);
	  Blk : Card_Block  := HDUSIO_File_Next(File);
	  Var : HDU_Variant := Parse(Blk);
  begin
	  -- Peek should not modify File index 
	  Set_Index(File, OrigIndex);
	  -- FIXME explicit cast
	  return Var;
  end Peek;
 



  procedure Read_HDU (FitsFile : in SIO.File_Type)
  is
	 HDU : SIO_HDU.HDU_Type := SIO_HDU.Read_Header(FitsFile);
  begin

	 TIO.Put_Line( "HDU       " & SIO_HDU.HDU_Category'Image(HDU.HDUCat) );
	 TIO.Put_Line( "NAXIS     " & NAXIS_Type'Image(HDU.NAXIS) );
	 TIO.Put_Line( "CardCount " & Positive'Image(HDU.CardCount) );
	 TIO.Put_Line( "BITPIX    " & Integer'Image(HDU.BITPIX) );
	 TIO.Put( "NAXISn    " );
	 for I in HDU.NAXISn'Range
	 loop
		 TIO.Put(Positive'Image(HDU.NAXISn(I)) & " " );
	 end loop;
	 TIO.Put_Line("");
  end Read_HDU;


 function Read_DataSize_bits (FitsFile : in SIO.File_Type) return Natural
 is
 begin
         return SIO_HDU.Read_Data_Size_bits(FitsFile);
 end Read_DataSize_bits;



end FITSlib.File;
