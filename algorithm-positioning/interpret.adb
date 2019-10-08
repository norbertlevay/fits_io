
-- Grammar [Get(Prim/Ext) HDU_Size_Info_Type]: 
        -- analogue to JSON or XML+DTD
        -- In FITS header variables have implicit types: 
        -- a variable name (Card.Key) implies type as defined in standard
        -- covers rules on relationships between cards:
        -- * individual cards, -> like varibles of basic types
        -- * card-arrays, -> like arrays of basic types
        -- * card-sets, -> like structs/records
        -- * alternative sets -> like ??



with FITS; use FITS;

with Primary_Size_Info; use Primary_Size_Info;
with Ext_Strict;        use Ext_Strict;


package body Interpret 
is
        function  Get(MandVals : in Primary_Mandatory_Card_Values) return HDU_Size_Info_Type
        is
                HDUSizeInfo : HDU_Size_Info_Type;
                NAXIS : Positive;
        begin
                if(MandVals.HDUTypeSet) then
                        HDUSizeInfo.HDUType := MandVals.HDUTypeVal;
                else
                        null;
                        -- ERROR raise exception No END card found
                end if;
                --HDUSizeInfo.HDUType := To_HDU_Type(State);

                if(MandVals.ENDCardSet) then
                        HDUSizeInfo.CardsCount := MandVals.ENDCardPos;
                else
                        null;
                        -- ERROR raise exception No END card found
                end if;

                if(MandVals.BITPIX.Read) then
                        HDUSizeInfo.BITPIX := Integer'Value(MandVals.BITPIX.Value);
                else
                        null;
                        -- ERROR raise exception No BITPIX card found
                end if;

                if(MandVals.NAXIS.Read) then
                        NAXIS := Integer'Value(MandVals.NAXIS.Value);
                else
                        null;
                        -- ERROR raise exception No NAXIS card found
                end if;

                for I in 1 .. NAXIS
                loop
                        if(MandVals.NAXISn(I).Read) then
                                HDUSizeInfo.NAXISArr(I) := Positive'Value(MandVals.NAXISn(I).Value);
                        else
                                null;
                                -- ERROR raise exception No NAXIS(I) card found
                        end if;

                end loop;

                -- FIXME dirty fix: should return NAXISArr only NAXIS-long
                for I in NAXIS+1 .. NAXIS_Last
                loop
                        HDUSizeInfo.NAXISArr(I) := 1;
                end loop;


                return HDUSizeInfo;
        end Get;






-- type HDU_Type is
  --      (PRIMARY_WITHOUT_DATA, PRIMARY_IMAGE, RANDOM_GROUPS,
  --      EXT_IMAGE, EXT_ASCII_TABLE, EXT_BIN_TABLE, RANDOM_BLOCKS);
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


        function  Get(MandVals : in Extension_Mandatory_Card_Values) return HDU_Size_Info_Type
        is
                HDUSizeInfo : HDU_Size_Info_Type;
                NAXIS : Positive;
        begin
                if(MandVals.XTENSION.Read) then
                        HDUSizeInfo.HDUType := To_HDU_Type(MandVals.XTENSION.Value);
                else
                        null;
                        -- ERROR raise exception: XTENSION card not found 
                end if;
                --HDUSizeInfo.HDUType    := State.XTENSION;

                if(MandVals.ENDCardSet) then
                        HDUSizeInfo.CardsCount := MandVals.ENDCardPos;
                else
                        null;
                        -- ERROR raise exception: END card found
                end if;

                if(MandVals.BITPIX.Read) then
                        HDUSizeInfo.BITPIX := Integer'Value(MandVals.BITPIX.Value);
                else
                        null;
                        -- ERROR raise exception No BITPIX card found
                end if;

                if(MandVals.NAXIS.Read) then
                        NAXIS := Integer'Value(MandVals.NAXIS.Value);
                else
                        null;
                        -- ERROR raise exception No NAXIS card found
                end if;

                for I in 1 .. NAXIS
                loop
                        if(MandVals.NAXISn(I).Read) then
                                HDUSizeInfo.NAXISArr(I) := Positive'Value(MandVals.NAXISn(I).Value);
                        else
                                null;
                                -- ERROR raise exception No NAXIS(I) card found
                        end if;

                end loop;

                -- FIXME dirty fix: should return NAXISArr only NAXIS-long
                for I in NAXIS+1 .. NAXIS_Last
                loop
                        HDUSizeInfo.NAXISArr(I) := 1;
                end loop;


                return HDUSizeInfo;
        end Get;


end Interpret;
