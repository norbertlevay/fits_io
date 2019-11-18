


with Strict; -- Positive_Arr needed

package body Formulas is

	-- implements [FITS] Eq(1)

        function PrimaryImage_DataSize_bits
                (BITPIX : Integer;
                 NAXIS  : Strict.Positive_Arr) return Positive
	is
		Nbits : Positive := 1;
	begin

		for I in NAXIS'Range
		loop
			Nbits := Nbits * NAXIS(I);
		end loop;

		Nbits := Natural(abs BITPIX) * Nbits;
		
		return Nbits;

	end PrimaryImage_DataSize_bits;


        -- implements [FITS] Eq(2)
 
        function ConformingExtension_DataSize_bits
                (BITPIX : Integer;
                 NAXIS  : Strict.Positive_Arr;
                 PCOUNT : Natural;
                 GCOUNT : Positive) return Positive
	is
		Nbits : Positive := PrimaryImage_DataSize_bits(BITPIX,NAXIS);
	begin

		Nbits := Nbits + Natural(abs BITPIX) * PCOUNT;
		Nbits := Nbits * GCOUNT;

		return Nbits;

	end ConformingExtension_DataSize_bits;
	
	
        -- implements [FITS] Eq(4) 
	
	function RandomGroups_DataSize_bits
                (BITPIX : Integer;
                 NAXIS  : Strict.Positive_Arr;
                 PCOUNT : Natural;
                 GCOUNT : Positive) return Positive
	is
		NAXISCopy : Strict.Positive_Arr := NAXIS;
	begin
		
		-- in RandomGroups NAXIS1 = 0
		-- with NAXIS=1, the formula (4) is the same as (2)
		
		NAXISCopy(1) := 1;

		return ConformingExtension_DataSize_bits(BITPIX,NAXISCopy,PCOUNT,GCOUNT);

        end RandomGroups_DataSize_bits;
	
	
	--
	-- calc number of free cards to fill up HeaderBlock
	--
	
	function  Free_Card_Slots (CardsCnt : in FPositive ) return Natural
	is
		CardsPerBlock : constant Positive := 36; -- FIXME generix FITS constant: put elsewehere 
		FreeSlotCnt : Natural := Natural( CardsCnt mod FPositive(CardsPerBlock) );
		-- explicit conversion ok: mod < CardsCntInBlock = 36;
	begin
		if FreeSlotCnt /= 0 then
			FreeSlotCnt := CardsPerBlock - FreeSlotCnt;
		end if;
		return FreeSlotCnt;
	end Free_Card_Slots;



end Formulas;
