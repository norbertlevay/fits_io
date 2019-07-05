





package body FITSlib.Formulas is

	-- implements [FITS] Eq(1)

        function PrimaryHDU_DataSize_bits
                (BITPIX : Integer;
                 NAXIS  : NAXIS_Arr) return FPositive
	is
		Nbits : FPositive := 1;
	begin

		for I in NAXIS'Range
		loop
			Nbits := Nbits * NAXIS(I);
		end loop;

		Nbits := FNatural(abs BITPIX) * Nbits;
		
		return Nbits;

	end PrimaryHDU_DataSize_bits;


        -- implements [FITS] Eq(2)
 
        function ConformingExtension_DataSize_bits
                (BITPIX : Integer;
                 NAXIS  : NAXIS_Arr;
                 PCOUNT : FNatural;
                 GCOUNT : FPositive) return FPositive
	is
		Nbits : FPositive := PrimaryHDU_DataSize_bits(BITPIX,NAXIS);
	begin

		Nbits := Nbits + FNatural(abs BITPIX) * PCOUNT;
		Nbits := Nbits * GCOUNT;

		return Nbits;

	end ConformingExtension_DataSize_bits;
	
	
        -- implements [FITS] Eq(4) 
	
	function RandomGroups_DataSize_bits
                (BITPIX : Integer;
                 NAXIS  : NAXIS_Arr;
                 PCOUNT : FNatural;
                 GCOUNT : FPositive) return FPositive
	is
		NAXISCopy : NAXIS_Arr := NAXIS;
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
		FreeSlotCnt : Natural := Natural( CardsCnt mod FPositive(CardsPerBlock) );
		-- explicit conversion ok: mod < CardsCntInBlock = 36;
	begin
		if FreeSlotCnt /= 0 then
			FreeSlotCnt := CardsPerBlock - FreeSlotCnt;
		end if;
		return FreeSlotCnt;
	end Free_Card_Slots;



end FITSlib.Formulas;
