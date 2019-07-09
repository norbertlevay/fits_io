
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

  use SIO_HDU;

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
 


 function Read_DataSize_bits (FitsFile : in SIO.File_Type) return Natural
 is
 begin
         return SIO_HDU.Read_Data_Size_bits(FitsFile);
 end Read_DataSize_bits;



        procedure Read_Data_Dimensions
                (Source  : SIO.File_Type;
                 DDims   : out Data_Dimensions_Type)
        is
		HSize      : HeaderSize_Type;
                ConfExt    : Conforming_Extension_Type;
                RandGroups : Random_Groups_Type;
                PrimImg    : Primary_Image_Type;
                -- analyze first block (after caller did Set_Index(1 or n)
                HBlk : Card_Block  := HDUSIO_File_Next(Source);
                Var  : HDU_Variant := Parse(HBlk);
        begin
		HSize.CardCount := 0; 
                DDims.HDUVar := Var;

                case Var is
                        when UNKNOWN =>
                                null;
                                -- raise exception if Index() is 1 
                                -- (if >1 it will be unspecified extension)
                                -- FIXME better solution here ? if not a fitsfile 
                                -- we should not even call this function

                        when PRIM_UNKNOWN =>
                                -- raise exception and exit
                                null;

                        when PRIM_NON_STANDARD =>
                                -- raise exception and exit
                                null;

                        when PRIM_NO_DATA =>
                                -- FIXME what to do ?? read until end of header?
                                -- or leave FileIndex after 1st block?
                                DDims.NAXIS := 0;

                        when PRIM_IMAGE =>
                                Read_Primary (Source, HBlk, HSize, PrimImg);
                                DDims.CardsCount := HSize.CardCount;
                                DDims.BITPIX := PrimImg.BITPIX;
                                DDims.NAXIS  := PrimImg.NAXIS;
                                DDims.NAXISn := PrimImg.NAXISn;

                        when RAND_GROUPS =>
                                Read_Random_Groups (Source, HBlk, HSize, RandGroups);
                                DDims.CardsCount := HSize.CardCount;
                                DDims.BITPIX := RandGroups.BITPIX;
                                DDims.NAXIS  := RandGroups.NAXIS;
				DDims.NAXISn(1) := 1;
                                DDims.NAXISn(2 .. RandGroups.NAXISn'Last) := RandGroups.NAXISn;

                        when EXT_IMAGE .. EXT_BINTABLE =>
                                Read_Conforming_Extensions (Source, HBlk, HSize, ConfExt);
                                DDims.CardsCount := HSize.CardCount;
                                DDims.BITPIX := ConfExt.BITPIX;
                                DDims.NAXIS  := ConfExt.NAXIS;
                                DDims.NAXISn := ConfExt.NAXISn;

                        when EXT_UNKNOWN =>
                                -- raise exception and exit
                                null;
                end case;

        end Read_Data_Dimensions;


end FITSlib.File;
