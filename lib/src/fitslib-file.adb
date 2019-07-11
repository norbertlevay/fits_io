
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


	-- -----------------------------------------------------------------------------
	-- exprimental, will be in HDU later, and HDU not generic but this func only
	-- -----------------------------------------------------------------------------
	
	generic
		type Source_Type is limited private;
		with function Next(Source : Source_Type) return Card_Block;
		with function First_Block(Blk : Card_Arr) return Boolean;
		with function Parse_Cards(Pos : Positive; Blk : Card_Arr) return Boolean;
	procedure Read_Cards (Source : Source_Type);



	procedure Read_Cards (Source : Source_Type)
	is
		Blk  : Card_Block;
		Cont : Boolean := True;
		Pos  : Natural := 0;
	begin

		Blk  := Next(Source);
		Pos  := Pos + 1;
		
		-- pass the first block to callbacks

		Cont := First_Block(Blk);
		if (not Cont) then
		       return;
		end if;

		Cont := Parse_Cards(Pos, Blk);
		if (not Cont) then
		       return;
		end if;

		-- pass blocks 2 ... to the callback

		loop
			Blk  := Next(Source);
			Pos  := Pos + 1;
			Cont := Parse_Cards(Pos, Blk);
			exit when not Cont;
		end loop;

	end Read_Cards;

	-- test the above generic : try to create read func for type X
	-- using types from Header; 
	-- first: instantiate generic for it
	
	function SIO_File_Next(File : SIO.File_Type) return Card_Block
	is 
		Blk : Card_Block;
	begin
		Card_Block'Read(Stream(File), Blk);
		return Blk;
	end SIO_File_Next;


	Var  : HDU_Variant;
 	HEnd : HeaderSize_Type := (False, 0);


	function a_First_Block(Blk : Card_Arr) return Boolean
	is
		Cont : Boolean := True;
		coff : Positive := 1; -- FIXME from Header.Parse func 
	begin
		Var := Parse(Blk);
		
		case Var is
			when UNKNOWN .. PRIM_NO_DATA =>
				Cont := False;
			when PRIM_IMAGE .. EXT_BINTABLE =>
				Cont := True;
			when EXT_UNKNOWN =>
				Cont := False;
		end case;

		Parse(Blk, HEnd);
		HEnd.CardCount := coff;
		if(HEnd.ENDCardFound) then
			Cont := False;
		end if;


		return Cont;
	end a_First_Block;


	
	PrimImg    : Primary_Image_Type;
	RandGroups : Random_Groups_Type;
	ConfExt    : Conforming_Extension_Type;

	function a_Parse_Cards (Pos : Positive; Blk : Card_Arr) return Boolean
	is
		Cont : Boolean := True;
		CardPos : Positive;
	begin

                for I in Blk'Range
                loop
			case Var is
				when UNKNOWN .. PRIM_NO_DATA =>
					Cont := False;
				when PRIM_IMAGE =>
					ParseCard(Blk(I), PrimImg);
					Cont := True;
				when RAND_GROUPS =>
					ParseCard(Blk(I), RandGroups);
					Cont := True;
				when EXT_IMAGE .. EXT_BINTABLE =>
					ParseCard(Blk(I), ConfExt);
					Cont := True;
				when EXT_UNKNOWN =>
					Cont := False;
			end case;

			CardPos := I + (Pos-1)*CardsPerBlock;
			Match_Card(CardPos, Blk(I), HEnd);
			if(HEnd.ENDCardFound) then
				Cont := False;
			end if;
	
		end loop;

--		Parse(Blk, HEnd);-- has no ParseCard on Header
--		HEnd.CardCount := Pos*32 - 32 + coff;
--		if(HEnd.ENDCardFound) then
--			Cont := False;
--		end if;
	return Cont;
	end a_Parse_Cards;



	procedure Read_Cards_Exp is new Read_Cards (
				Source_Type => SIO.File_Type,
				Next        => SIO_File_Next,
				First_Block => a_First_Block, 
				Parse_Cards => a_Parse_Cards);





	--procedure Read_Exp (File : SIO.File_Type; Exp : out Exp_Type)
	procedure Read_Exp (File : SIO.File_Type; DDims : out Data_Dimensions_Type)
	is
	begin
		Read_Cards_Exp(File);

		-- now Prim filled in, convert to Exp

		DDims.HDUVar     := Var;
		DDims.CardsCount := HEnd.CardCount;
		case Var is
			when UNKNOWN .. PRIM_NO_DATA =>
				null;

			when PRIM_IMAGE =>
				DDims.BITPIX := PrimImg.BITPIX;
	                        DDims.NAXIS  := PrimImg.NAXIS;
                                DDims.NAXISn := PrimImg.NAXISn;

                        when RAND_GROUPS =>
                                DDims.BITPIX     := RandGroups.BITPIX;
                                DDims.NAXIS      := RandGroups.NAXIS;
				DDims.NAXISn(1)  := 1;-- FIXME why this? DDims NAXISn is type Positive
                                DDims.NAXISn(2 .. RandGroups.NAXISn'Last) := RandGroups.NAXISn;

                        when EXT_IMAGE .. EXT_BINTABLE =>
                                DDims.BITPIX     := ConfExt.BITPIX;
                                DDims.NAXIS      := ConfExt.NAXIS;
                                DDims.NAXISn     := ConfExt.NAXISn;

			when EXT_UNKNOWN =>
				null;
		end case;
	
	end Read_Exp;



 
 end FITSlib.File;
