
with FITS; use FITS;
with FITS_IO.Media;

package body FITS_IF is

   -- Instantiate FITS_IO.Media for block-io BIO.File

  function BIONext(File : in BIO.File_Type) return Card_Block
   is
    HBlk : Card_Block;
   begin
    BIO.Read(File, HBlk);
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

   procedure Set_HDU(File : File_Type; HDUNum : Positive_Count) 
   is
   begin
	   null;
   end Set_HDU;

end FITS_IF;
