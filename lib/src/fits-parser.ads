
with FITS; use FITS; -- Card_Block type
with FITS.Header; use FITS.Header; -- Max_8 type

generic
   type Source_Type is private;
   with function Next(Source : Source_Type) return Card_Block is <>;
   -- source can be fits-file, memory (list of cards), network etc...
package FITS.Parser is

   -- FIXME consider variable record for Key types:
   -- Key, Indexed_Key, Variant Key
   type Key is
    record
     Name    : Max_8.Bounded_String; -- keyword string
     Indexed : Boolean;
     Min     : Natural;
     Max     : Positive;
--     Variant : Character;
     Found   : Boolean;
    end record;

   -- position Source_Type to begining of the Header
   -- before calling this
   generic
    type Parsed_Type is private;
    type User_Type   is private;
    with function Parse_Card
                    (Card      : in     Card_Type;
                     Data      : in out Parsed_Type;
                     UData     : in out User_Type)
                     return Boolean;
   procedure Parse_Header(Source     : in Source_Type;
                          ParsedData : in out Parsed_Type;
                          UserData   : in out User_Type);

end FITS.Parser;
 