
with Ada.Text_IO;

with FITSlib.Header; use FITSlib.Header;
with FITSlib.HDU;    

package body FITSlib.File is

  package TIO renames Ada.Text_IO;


  -- instantiate HDU for SIO file

  function xxSIO_Next_Buffer_Content(File : SIO.File_Type) return Card_Block
  is
   HBlk : Card_Block;
  begin
   Card_Block'Read(SIO.Stream(File), HBlk);
   return HBlk;
  end xxSIO_Next_Buffer_Content;

  package SIO_HDU is new FITSlib.HDU
	     (Buffered_Source_Type => SIO.File_Type,
	      Buffered_Sink_Type   => SIO.File_Type,
	      Next_Buffer_Conetent => xxSIO_Next_Buffer_Content);

  -- SIO_HDU ready



  procedure Set_HDU
           (File   : SIO.File_Type;
            HDUNum : Positive)
  is
	  Nbits     : Natural;
	  ByCount   : Natural;
	  CurHDUNum : Positive := 1;
	  HeaderStart : SIO.Positive_Count;
  begin
	  SIO.Set_Index(File, 1);
	  -- read Primary Header
	
	  while (CurHDUNum < HDUNum) 
	  loop
		  HeaderStart := SIO.Index(File);

		  Nbits := SIO_HDU.Read_Data_Size_bits(File);
		  -- raises excpetion if not PrimImg or RangGroups
		
		  ByCount := NBits / 8;
		  -- FIXME do properly with SIO Element Size vs FITS Byte...

	    	  SIO.Set_Index(File, HeaderStart + ByCount);
		  
		  -- next HDU
		  CurHDUNum := CurHDUNum + 1;
	  end loop;


  end Set_HDU;






 end FITSlib.File;
