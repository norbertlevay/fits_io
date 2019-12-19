
with Ada.Text_IO;


with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Bounded;   use Ada.Strings.Bounded;

with Ada.Unchecked_Deallocation;

with Strict;
with Formulas;
with Keyword_Record;  use Keyword_Record;


package body File is

	package TIO renames Ada.Text_IO;
	package KW renames Keyword_Record;
   
   --
   -- Read File until ENDCard found
   --

   procedure Read_Card (File : in SIO.File_Type;
			HStart : in SIO.Positive_Count;
                        CurBlkNum : in out Natural;
                        Blk : in out Card_Block;
                        CardNum : in Positive;
                        Card    : out  String)
   is
	 BlockSize_SIOunits : constant SIO.Positive_Count := 2880;
         BlkNum : Positive;
         CardNumInBlk : Positive;
         BlkNumIndex : SIO.Positive_Count;
   begin
         BlkNum := 1 + (CardNum - 1) / 36; 
         CardNumInBlk := CardNum - (BlkNum - 1) * 36; 
    
         if(BlkNum /= CurBlkNUm)
         then
                        -- FIXME BEGIN only this section depends on SIO. file access
                        -- make it Read_Block(SIO.File, FileBlkNum, Blk)
                        -- where FileBlkNum := HStart + BlkNum
                        -- BlkNum - relative to HDU start
                        -- FileBlkNum - relative to File start
               BlkNumIndex := SIO.Positive_Count( Positive(HStart) + (BlkNum-1) 
                                                * Positive(BlockSize_SIOunits) );

               SIO.Set_Index(File, BlkNumIndex);
               Card_Block'Read(SIO.Stream(File), Blk);
               CurBlkNum := BlkNum;
               -- FIXME END   only this section depends on SIO. file access
         end if;

         Card := Blk(CardNumInBlk);

   end Read_Card;


-- read info help in Mandatory keys of the Header
  function  Get_Mandatory (FitsFile : in SIO.File_Type) return Strict.Result_Rec
  is
		HeaderStart : SIO.Positive_Count := SIO.Index(FitsFile);	
                CardNum : Natural;
                Card : String(1..80);

                CurBlkNum : Natural := 0; -- none read yet
                Blk : Card_Block;
   begin
                CardNum := Strict.Reset_State;
                loop
                        Read_Card(FitsFile, HeaderStart, CurBlkNum, Blk, CardNum, Card);
                        CardNum := Strict.Next(CardNum, Card);
                        exit when (CardNum = 0); 
                end loop;

                -- calc HDU size

                declare
                        PSize : Strict.Result_Rec := Strict.Get;
                begin
			return PSize;
                end;

   end Get_Mandatory;






   function  Get (FitsFile : in SIO.File_Type) return HDU_Info_Type
   is
	PSize   : Strict.Result_Rec := Get_Mandatory(FitsFile);
	HDUInfo : HDU_Info_Type(PSize.NAXIS_Last);
   begin
	HDUInfo.XTENSION := Max20.To_Bounded_String(
					Strict.HDU_Type'Image(Psize.HDU));
					-- FIXME PSize.HSU is enum: 20 length might not be enough
					-- find other solution then Bounded_String??
	HDUInfo.CardsCnt := FPositive(PSize.CardsCount); --FPositive
	HDUInfo.BITPIX   := PSize.BITPIX;--Integer; 

	-- FIXME unify types:   HDUInfo.NAXISn := PSize.NAXISArr;
        for I in HDUInfo.NAXISn'Range
        loop
        	HDUInfo.NAXISn(I) := FInteger(PSize.NAXISArr(I));
        end loop;

        return HDUInfo;

   end Get;




   function  Get_Cards (FitsFile : in  SIO.File_Type;
                       Keys : in Optional.Bounded_String_8_Arr)
      return Card_Arr
   is
 	HeaderStart : SIO.Positive_Count := SIO.Index(FitsFile);	
     	CardNum : Natural;
        Card : String(1..80);

	CurBlkNum : Natural := 0; -- none read yet
	Blk : Card_Block;
   begin
        CardNum := Optional.Init(Keys);
        loop
		Read_Card(FitsFile, HeaderStart, CurBlkNum, Blk, CardNum, Card);
                CardNum := Optional.Next(CardNum, Card);
                exit when (CardNum = 0); 
        end loop;


	declare
		Cards : Card_Arr := Optional.Get_Cards;
	begin
		return Cards;
	end;

   end Get_Cards;





   --
   -- Set file index to HDU start given by HDUNum
   --



        function  Calc_HeaderUnit_Size_blocks
                (CardsCount : in Positive) 
                return Positive
        is
                HUSize : Positive;
        begin 
                HUSize := Positive(1 + (CardsCount - 1)/36);

                return HUSize;
        end Calc_HeaderUnit_Size_blocks;


        -- FIXME consider funcs depending on variable record, put where the variable record is
        -- they always must check presence of the variable fields and 
        -- call external funcs accordingly
function  Calc_DataUnit_Size_blocks  
                (Res : in Strict.Result_Rec) return KW.FNatural
is
	Size_bits : KW.FNatural;

	BitsPerBlock : constant KW.FPositive := (2880*8);
		-- FIXME generic FITS-constant: move elsewhere
begin
	case(Res.HDU) is
	when Strict.NO_DATA =>
		Size_bits := 0;

	when Strict.IMAGE =>
		Size_bits := Formulas.PrimaryImage_DataSize_Bits(Res.BITPIX, Res.NAXISArr);

	when Strict.RANDOM_GROUPS =>
		Size_bits := Formulas.RandomGroups_DataSize_bits
                 		(Res.BITPIX, Res.NAXISArr,
                  		Res.PCOUNT, Res.GCOUNT);

	when Strict.CONFORMING_EXTENSION .. Strict.STANDARD_BINTABLE =>
		Size_bits := Formulas.ConformingExtension_DataSize_bits
                 		(Res.BITPIX, Res.NAXISArr,
                  		Res.PCOUNT, Res.GCOUNT);
	end case;

	return (1 + (Size_bits - 1) / BitsPerBlock); 
		-- FIXME consider separate func for bits -> blocks

end  Calc_DataUnit_Size_blocks;





procedure Set_Index
           (File   : SIO.File_Type;
            HDUNum : Positive)
is
        package TIO renames Ada.Text_IO;

        BlockSize_SIOunits : constant SIO.Positive_Count := 2880;
    
        Card : String(1..80);
        CurHDUNum : Positive;
        HeaderStart : SIO.Positive_Count;
        Blk : Card_Block;
        HDUSize_blocks : KW.FPositive;
        CardNum : Natural;
        CurBlkNum : Natural := 0; -- none read yet
begin
        HeaderStart := 1;
        SIO.Set_Index(File,HeaderStart);

        CurHDUNum := 1;

        if(CurHDUNum = HDUNum) then
                return;
        end if;

        while ( CurHDUNum < HDUNum )
        loop
                TIO.New_Line;

                -- Read Header
    
                CardNum := Strict.Reset_State;
                loop
                        Read_Card(File, HeaderStart, CurBlkNum, Blk, CardNum, Card);
                        CardNum := Strict.Next(CardNum, Card);
                        exit when (CardNum = 0); 
                end loop;

                -- calc HDU size

               declare
                        PSize : Strict.Result_Rec := Strict.Get;
                begin
 			TIO.New_Line;TIO.Put_Line("DBG> HDU_Type: " 
                                                & Strict.HDU_Type'Image(PSize.HDU));

                 	HDUSize_blocks := KW.FPositive(Calc_HeaderUnit_Size_blocks(PSize.CardsCount))
					+ Calc_DataUnit_Size_blocks(PSize); 
					-- FIXME conversion for HeaderUnit
                end;

                TIO.Put_Line("DBG> HDUSize [blocks]: "
                                        & KW.FPositive'Image(HDUSize_blocks));

                -- move to next HDU

                HeaderStart := HeaderStart
                                + SIO.Positive_Count(HDUSize_blocks) * BlockSize_SIOunits;
                TIO.New_Line;
                TIO.Put_Line("DBG> Next ExtHeaderStart: " & SIO.Positive_Count'Image(HeaderStart));

                SIO.Set_Index(File, HeaderStart);

                CurHDUNum := CurHDUNum + 1;

        end loop;

-- FIXME add handle Random Blocks if exist at fits-file end 

end Set_Index;

--
-- Data access
-- assume File_Index points to DataUnit'First element
--

-- from Coords and Cube dimensions (MaxCOords) calc
-- offset in data-element count
function To_Offset (Coords    : in  Strict.Positive_Arr;
                     MaxCoords : in  Strict.Positive_Arr)
   return FPositive
 is
  Offset : FPositive;
  Sizes  : Strict.Positive_Arr := MaxCoords;
 begin
  if Coords'Length /= MaxCoords'Length
  then
   null;
   -- raise exception <-- needed this if ?
   -- no, check only high level inputs, this is not direct API call
   -- assume if code corrct, it is corrct here
  end if;

  --  
  -- generate size of each plane
  --  
  declare
    Accu  : FPositive := 1;
  begin
    for I in MaxCoords'First .. (MaxCoords'Last - 1)
    loop
     Accu := Accu * MaxCoords(I);
     Sizes(I) := Accu;
     -- FIXME Acc is not needed, init Sizes(1):=1 and use Sizes
    end loop;
  end;

  Offset := Coords(1);
  for I in (Coords'First + 1) .. Coords'Last
  loop
   Offset := Offset + (Coords(I) - 1) * Sizes(I - 1); 
  end loop;

  return Offset;
 end To_Offset;



-- read one data element at coordinates 'Coords'
-- from cube of dimensions 'MaxCoords'
generic
	type T is private; -- UInt_8 Int_16 ... Float_32 Float_64
function Element_Value
		(File : File_Type; 
		DUStart   : in SIO.Positive_Count;
		Coords    : in Strict.Positive_Arr;
		MaxCoords : in Strict.Positive_Arr)
 return T;

function Element_Value
		(File : File_Type; 
		DUStart   : in SIO.Positive_Count;
		Coords    : in Strict.Positive_Arr;
		MaxCoords : in Strict.Positive_Arr) 
		-- MaxCoords is HDU_Info_Type.NAXISn() 
 return T
is
	tt : T;
	-- calc Offset from Coord	
	Offset : FInteger := To_Offset(Coords, MaxCoords);
	DataElementSize : FInteger := T'Size / Ada.Streams.Stream_Element'Size;
	-- FIXME check if not divisable
begin
	-- move to Offset
	Set_Index(File, DUStart + SIO.Positive_Count(DataElementSize * Offset));
					-- FIXME explicit conversions
	-- read tt
	T'Read(Stream(File), tt);
	-- FIXME check is Endianness handled
	return tt;
end Element_Value;



end File;

