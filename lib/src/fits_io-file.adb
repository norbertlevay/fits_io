
with Ada.Text_IO;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with FITS_IO.Media;

with FITSlib.HDU; -- experimental BEGIN...END
with FITSlib.Header; -- experimental BEGIN...END

package body FITS_IO.File is

 -- ----------------------------------------------------------
 -- BEGIN testing FITSlib.hdu FITSlib.header .FITSlib.key FITSlib.fomrulas

  function HDUSIO_File_Next(File : Ada.Streams.Stream_IO.File_Type) return FITSlib.Header.Card_Block
  is 
   HBlk : FITSlib.Header.Card_Block; 
  begin
   FITSlib.Header.Card_Block'Read(Stream(File), HBlk); 
   return HBlk; 
  end HDUSIO_File_Next; 


  package SIO_HDU is new FITSlib.HDU(Source_Type =>  Ada.Streams.Stream_IO.File_Type, 
        	                         Next =>  HDUSIO_File_Next); 
--  use SIO_HDU;

 package TIO renames Ada.Text_IO;


 -- call IF      
 procedure Read_HDU (FitsFile : in SIO.File_Type)
 is
	 HDU : SIO_HDU.HDU_Type := SIO_HDU.Read_Header(FitsFile);
 begin
	 TIO.Put_Line( "HDU       " & SIO_HDU.HDU_Category'Image(HDU.HDUCat) );
	 TIO.Put_Line( "NAXIS     " & FITSlib.Header.NAXIS_Type'Image(HDU.NAXIS) );
	 TIO.Put_Line( "CardCount " & Positive'Image(HDU.CardCount) );
	 TIO.Put_Line( "BITPIX    " & Integer'Image(HDU.BITPIX) );
	 TIO.Put( "NAXISn    " );
	 for I in HDU.NAXISn'Range
	 loop
		 TIO.Put(Positive'Image(HDU.NAXISn(I)) & " " );
	 end loop;
	 TIO.Put_Line("");
 end Read_HDU;



 -- END   testing FITSlib.hdu FITSlib.header .FITSlib.key FITSlib.fomrulas
 -- ----------------------------------------------------------


  -- Instantiate Header for file-media

 function SIO_File_Next(File : Ada.Streams.Stream_IO.File_Type) return Card_Block  
  is 
   HBlk : Card_Block; 
  begin
   Card_Block'Read(Stream(File), HBlk); 
   return HBlk; 
  end SIO_File_Next; 
  pragma Inline(SIO_File_Next); 

  package SIO_File_Media is new Media(Source_Type =>  Ada.Streams.Stream_IO.File_Type, 
 	                              Index_Type  =>  Ada.Streams.Stream_IO.Positive_Count, 
        	                      Next        =>  SIO_File_Next, 
                	              Index       =>  Ada.Streams.Stream_IO.Index, 
                        	      Set_Index   =>  Ada.Streams.Stream_IO.Set_Index); 
  use SIO_File_Media;


  -- File management functions

   procedure Move_Index
             (FitsFile : in SIO.File_Type;
              ByCount  : in SIO.Positive_Count) is
   begin
     SIO.Set_Index(FitsFile, SIO.Index(FitsFile) + ByCount);
   end Move_Index;
   pragma Inline (Move_Index);



   procedure Set_Index(FitsFile : in SIO.File_Type;
                       HDUNum   : in Positive)
   is
    CurDUSize_bytes  : FITS_IO.Count;
    CurHDUNum : Positive := 1;

   begin

    SIO.Set_Index(FitsFile, 1);
    -- move to begining of the Primary HDU

    while CurHDUNum < HDUNum
    loop

     CurDUSize_bytes  := Read_DUSize_bytes(FitsFile);
     -- CurDUSize_bytes  := SIO_File_Header.Read_DUSize_bytes(FitsFile);

     Ada.Text_IO.Put_Line("NEWSet_Index: " & FITS_IO.Count'Image(CurDUSize_bytes));


     if CurDUSize_bytes /= 0
     then
       Move_Index(FitsFile, SIO.Positive_Count(CurDUSize_bytes));
     end if;

     CurHDUNum := CurHDUNum + 1;
    end loop;

   end Set_Index;

end FITS_IO.File;
