
-- Grammar [Get(Prim/Ext) HDU_Size_Info_Type]: 
        -- analogue to JSON or XML+DTD
        -- In FITS header variables have implicit types: 
        -- a variable name (Card.Key) implies type as defined in standard
        -- covers rules on relationships between cards:
        -- * individual cards, -> like varibles of basic types
        -- * card-arrays, -> like arrays of basic types
        -- * card-sets, -> like structs/records
        -- * alternative sets -> like ??

-- This is not Lexar, rather grammar rules
        --
        -- it is here that we check whether all values were set and so results are valid
        -- also interpret here syntax, like: 
        --  * if NAXIS exist, also NAXIS1... array must exist
        --  * if one card of a group present, all other cards of that group-type must be present
        --  * recognize alternative calibration sets cccccA card set and cccccB card set
        --  That all data for SizeClaculation was Read/Set
        --  and all those data is valid/within the range



with Ada.Exceptions; use Ada.Exceptions;

with FITS; use FITS;
with Keyword_Record; use Keyword_Record;

with FA_Primary;   use FA_Primary;
with FA_Extension; use FA_Extension;




package body Interpret 
is

	function To_HDU_Type(StateName : in FA_Primary.State_Name) return HDU_Type
        is
                t : HDU_Type;
        begin
		case(StateName) is
			when NO_DATA 	   => t := PRIMARY_WITHOUT_DATA;
			when IMAGE   	   => t := PRIMARY_IMAGE;
			when RANDOM_GROUPS => t := RANDOM_GROUPS;
			when others =>
				Raise_Exception(Programming_Error'Identity, 
				"Not all cards read. State "& 
				FA_Primary.State_Name'Image(StateName) );
		end case;

                return t;

        end To_HDU_Type;


        function  Get(State : in FA_Primary.State_Type) return HDU_Size_Info_Type
        is
                HDUSizeInfo : HDU_Size_Info_Type;
                NAXIS : Positive;
        begin
                        
		HDUSizeInfo.HDUType := To_HDU_Type(State.Name);

                if(State.ENDCardSet) then
                        HDUSizeInfo.CardsCount := State.ENDCardPos;
                else
			Raise_Exception(Card_Not_Found'Identity, "END");
                end if;

		-- FIXME how about SIMPLE value, should we check it was set ?

                if(State.BITPIX.Read) then
                        HDUSizeInfo.BITPIX := To_Integer(State.BITPIX.Value);
                else
			Raise_Exception(Card_Not_Found'Identity, "BITPIX");
                end if;

                if(State.NAXIS.Read) then
                        NAXIS := To_Integer(State.NAXIS.Value);
                else
			Raise_Exception(Card_Not_Found'Identity, "NAXIS");
                end if;

                for I in 1 .. NAXIS
                loop
                        if(State.NAXISn(I).Read) then
                                HDUSizeInfo.NAXISArr(I) := To_Integer(State.NAXISn(I).Value);
                        else
				Raise_Exception(Card_Not_Found'Identity, "NAXIS"&Integer'Image(I));
                        end if;

                end loop;

                -- FIXME dirty fix: should return NAXISArr only NAXIS-long
                for I in NAXIS+1 .. NAXIS_Max
                loop
                        HDUSizeInfo.NAXISArr(I) := 1;
                end loop;


                return HDUSizeInfo;
        end Get;






       function To_HDU_Type(StateName : in FA_Extension.State_Name) return HDU_Type
        is
                t : HDU_Type;
        begin
		case(StateName) is
			when SPECIAL_RECORDS =>	t := SPECIAL_RECORDS;
			when IMAGE 	     =>	t := EXT_IMAGE;
			when TABLE 	     =>	t := EXT_ASCII_TABLE;
			when BINTABLE 	     =>	t := EXT_BIN_TABLE;
			when others =>
				Raise_Exception(Programming_Error'Identity, 
				"Not all cards read. State "& 
				FA_Extension.State_Name'Image(StateName) );
		end case;

                return t;

        end To_HDU_Type;


        function  Get(State : in FA_Extension.State_Type) return HDU_Size_Info_Type
        is
                HDUSizeInfo : HDU_Size_Info_Type;
                NAXIS : Positive;
        begin
               
	       	HDUSizeInfo.HDUType := To_HDU_Type(State.Name);

                if(State.ENDCardSet) then
                        HDUSizeInfo.CardsCount := State.ENDCardPos;
                else
			Raise_Exception(Card_Not_Found'Identity, "END");
                end if;

		-- FIXME how about XTENSION value, shoule we check it was set ?

                if(State.BITPIX.Read) then
                        HDUSizeInfo.BITPIX := To_Integer(State.BITPIX.Value);
                else
			Raise_Exception(Card_Not_Found'Identity, "BITPIX");
                end if;

                if(State.NAXIS.Read) then
                        NAXIS := To_Integer(State.NAXIS.Value);
                else
			Raise_Exception(Card_Not_Found'Identity, "NAXIS");
                end if;

                for I in 1 .. NAXIS
                loop
                        if(State.NAXISn(I).Read) then
                                HDUSizeInfo.NAXISArr(I) := To_Integer(State.NAXISn(I).Value);
                        else
				Raise_Exception(Card_Not_Found'Identity, "NAXIS"&Integer'Image(I));
                        end if;

                end loop;

                -- FIXME dirty fix: should return NAXISArr only NAXIS-long
                for I in NAXIS+1 .. NAXIS_Max
                loop
                        HDUSizeInfo.NAXISArr(I) := 1;
                end loop;


                return HDUSizeInfo;
        end Get;


end Interpret;
