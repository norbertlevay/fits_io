
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
        function  Get(State : in FA_Primary.State_Type) return HDU_Size_Info_Type
        is
                HDUSizeInfo : HDU_Size_Info_Type;
                NAXIS : Positive;
        begin
                if(State.HDUTypeSet) then
                        HDUSizeInfo.HDUType := State.HDUTypeVal;
                else
			Raise_Exception(Programming_Error'Identity, "Primary HDU type undetermined.");
                end if;

                if(State.ENDCardSet) then
                        HDUSizeInfo.CardsCount := State.ENDCardPos;
                else
			Raise_Exception(Card_Not_Found'Identity, "END");
                end if;

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






-- type HDU_Type is
  --      (PRIMARY_WITHOUT_DATA, PRIMARY_IMAGE, RANDOM_GROUPS,
  --      EXT_IMAGE, EXT_ASCII_TABLE, EXT_BIN_TABLE, SPECIAL_RECORDS);
        function To_HDU_Type(XTENSION_Value : in String) return HDU_Type
        is
                t : HDU_Type;
        begin
                if(XTENSION_Value    = "'IMAGE   '          ") then
                                t := EXT_IMAGE;

                elsif(XTENSION_Value = "'TABLE   '          ") then
                                t := EXT_ASCII_TABLE;

                elsif(XTENSION_Value = "'BINTABLE'          ") then
                                t := EXT_BIN_TABLE;
                end if;
                -- FIXME RAND_BLOCKS ?

                return t;

        end To_HDU_Type;


        function  Get(State : in FA_Extension.State_Type) return HDU_Size_Info_Type
        is
                HDUSizeInfo : HDU_Size_Info_Type;
                NAXIS : Positive;
        begin
                if(State.XTENSION.Read) then
                        HDUSizeInfo.HDUType := To_HDU_Type(State.XTENSION.Value);
                else
			Raise_Exception(Card_Not_Found'Identity, "XTENSION");
                end if;

                if(State.ENDCardSet) then
                        HDUSizeInfo.CardsCount := State.ENDCardPos;
                else
			Raise_Exception(Card_Not_Found'Identity, "END");
                end if;

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
