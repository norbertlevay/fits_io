
with Ada.Text_IO;--use Ada.Text_IO;
--with Ada.Streams.Stream_IO;use Ada.Streams.Stream_IO; -- Positive_Count needed
with Ada.Streams; -- Stream access needed

with FITS; use FITS;-- Positive_Count needed

with Numeric_Type;
with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;

with Array_IO;



package body Buffer_Type is


--    package SIO renames Ada.Streams.Stream_IO;


 type Float_Arr is array (Positive_Count range <>) of Float;
 type I16_Arr   is array (Positive_Count range <>) of Short_Integer;

 package I16Raw  is new Numeric_Type(Short_Integer, I16_Arr,    Float_Arr);
 package F32Raw  is new Numeric_Type(Float,         Float_Arr,  Float_Arr);

 package I16_AIO is new Array_IO(I16Raw, Phys);
 package F32_AIO is new Array_IO(F32Raw, Phys);





procedure Read_Buffer
    (S: not null access Ada.Streams.Root_Stream_Type'Class;
    Item : out Buffer)
is
begin

    if(File_Undefined_Valid)
    then

        I16Raw.Set_Undefined(+(File_Undefined_Value));

        -- did user force undefined value ? it has precedence
        if(Memory_Undefined_Valid)
        then
             case(Memory_BITPIX) is
                when  0 | 8 | 16 | 32 | 64 | -32 | -64 => 
                    Phys.Set_Undefined(Memory_Undefined_Value);
                when others => null; -- Error: "Invalid BITPIX for target"
            end case;
        else
            null; -- lib calculates
        end if;

    end if;


    case(File_BITPIX) is
        when  16 => I16_AIO.Read(S, A,B, Item);
        when -32 => F32_AIO.Read(S, A,B, Item);
        when  8 | 32 | 64 | -64 => null; -- Warn: "Not implemented"
        when others => null; -- Error: "Invalid BITPIX for target"
    end case;
end Read_Buffer;





procedure Write_Buffer
    (S: not null access Ada.Streams.Root_Stream_Type'Class;
    Item : in Buffer)
is
begin

    if(Memory_Undefined_Valid)
    then
        Phys.Set_Undefined(Memory_Undefined_Value);

        -- did caller force undefined value ? it has precedence
        if(File_Undefined_Valid)
        then
            case(File_BITPIX) is
                when  16 => I16Raw.Set_Undefined(I16Raw.To_Numeric(+File_Undefined_Value));
                when -32 => F32Raw.Set_Undefined(F32Raw.To_Numeric(+File_Undefined_Value));
                when  8 | 32 | 64 | -64 => null; -- Warn: "Not implemented"
                when others => null; -- Error: "Invalid BITPIX for target"
            end case;
        end if;

    end if;


    case(File_BITPIX) is
        when  16 => I16_AIO.Write(S, A,B, Item);
        when -32 => F32_AIO.Write(S, A,B, Item);
        when  8 | 32 | 64 | -64 => null; -- Warn: "Not implemented"
        when others => null; -- Error: "Invalid BITPIX for target"
    end case;
end Write_Buffer;



end Buffer_Type;

