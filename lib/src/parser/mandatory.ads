

with Ada.Strings.Unbounded;-- use Ada.Strings.Unbounded;
--with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO; -- (Positive_)Count needed
with FITS; use FITS; -- (Positive_)Count needed
with Keyword_Record; -- FIndex needed

package Mandatory is

--    package SIO renames Ada.Streams.Stream_IO;-- Count needed

    type HDU_Type is
        (NO_DATA, IMAGE, RANDOM_GROUPS, -- Primary
        CONFORMING_EXTENSION,
        STANDARD_IMAGE, STANDARD_TABLE, STANDARD_BINTABLE);

--    type NAXIS_Arr is array (Keyword_Record.FIndex range <>) of Positive_Count;
    type TFORM_Arr is array (Positive range <>) of Ada.Strings.Unbounded.Unbounded_String;

    type Result_Rec(HDU : HDU_Type;
            NAXIS_Last   : Natural;
            TFIELDS_Last : Natural) is
        record
            CardsCount : Positive_Count;
            BITPIX     : Integer;

            case HDU is
            when IMAGE .. STANDARD_BINTABLE  =>
                NAXISn : NAXIS_Arr(1 .. NAXIS_Last);
                case HDU is
                when RANDOM_GROUPS .. STANDARD_BINTABLE =>
                    PCOUNT : Count;
                    GCOUNT : Positive_Count;

                    case HDU is
                    when STANDARD_TABLE | STANDARD_BINTABLE =>
                        TFORMn : TFORM_Arr(1..TFIELDS_Last);
                        case HDU is
                        when STANDARD_TABLE =>
                            TBCOLn : NAXIS_Arr(1..TFIELDS_Last);
                        when others => null;
                        end case;
                    when others => null;
                    end case;

                when others => null;
                end case;
            when others => null;
            end case;
        end record;
-- FIXME using ranges after case-when ... is dangerous: if HDU_Type enum changes order of elements

    function Reset_State return Positive_Count; 
    function Next (Pos : in Positive_Count; Card : in Keyword_Record.String_80) return Count;
    function Get return Result_Rec;


    Unexpected_First_Card : exception;-- possibly Special Records
    Unexpected_Card       : exception;
    Unexpected_Card_Value : exception;
    Duplicate_Card        : exception;
    Card_Not_Found        : exception;
    Invalid_Card          : exception;
    Programming_Error     : exception;

end Mandatory;

