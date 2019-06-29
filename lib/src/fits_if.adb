

-- FIXME Card_Block defined twice: in FITS.ads and FITS_IO.ads ; see also BIO.Read call
-- resolve, use only one. For now workaround: set FITS.Card_Block in fits_io-media.adb:41 (and ads)

with FITS; use FITS;
with FITS_IO; use FITS_IO;
with FITS_IO.Media;

package body FITS_IF is

   -- Instantiate FITS_IO.Media for block-io BIO.File

  function BIONext(File : in BIO.File_Type) return FITS.Card_Block
   is
    HBlk : FITS.Card_Block;
   begin
    BIO.Read(File, HBlk); -- FIXME this needs FITS.Card_Block
    return HBlk;
   end BIONext;
 --  pragma Inline(BIO_File_Next);

   package BIO_File_Media is new FITS_IO.Media(Source_Type =>  BIO.File_Type,
                                               Index_Type  =>  BIO.Positive_Count,
                                               Next        =>  BIONext,
                                               Index       =>  BIO.Index,
                                               Set_Index   =>  BIO.Set_Index);
   use BIO_File_Media;


   -------------------------------------
   -- HDU Input and Output Operations --
   -------------------------------------

   procedure Read_Data
     (File : File_Type;
      Data : out Data_Block)
   is
   begin
	   null;
   end Read_Data;

   procedure Write_Data
     (File : File_Type;
      Data : Data_Block) 
   is
   begin
	   null;
   end Write_Data;


  -- Positioning

  procedure Move_Index
             (FitsFile : in File_Type;
              ByCount  : in BIO.Positive_Count) is
   begin
     Set_Index(FitsFile, Index(FitsFile) + ByCount);
   end Move_Index;
   pragma Inline (Move_Index);



   procedure Set_HDU(File : File_Type; HDUNum : Positive_Count) 
   is
	CurDUSize_bytes  : FITS_IO.Count;
    	CurHDUNum : Positive_Count := 1;
   begin
	   -- implementaion like fits_io-file.adb Set_Index(File,HDUNum)
	   -- -- except it is for SIO not BIO
	   --

      Set_Index(File, 1);
      
      while CurHDUNum < HDUNum
	loop

     	CurDUSize_bytes  := BIO_File_Media.Read_DUSize_bytes(BIO.File_Type(File));
	-- FIXME explicit conversion of File_Type

	-- Ada.Text_IO.Put_Line("NEWSet_Index: " & Count'Image(CurDUSize_bytes));


     	if CurDUSize_bytes /= 0
     	then
       		Move_Index(File, BIO.Positive_Count(CurDUSize_bytes));
		-- FIXME explicit conversion of Count
     	end if;

     	CurHDUNum := CurHDUNum + 1;
    end loop;

   end Set_HDU;

end FITS_IF;
