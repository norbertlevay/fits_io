

--with FITS; use FITS;
with Ada.Streams.Stream_IO;

with Keyword_record; use Keyword_Record; -- FPositive needed

package NCube_Funcs is

 type Coord_Type is array (FPositive range <> ) of FPositive;

 function To_Offset (Coords    : in  Coord_Type;
                     MaxCoords : in  Coord_Type)
   return FPositive;



 procedure To_Coords (Offset    : in  FPositive;
                      MaxCoords : in  Coord_Type;
                      Coords    : out Coord_Type);

end NCube_Funcs;


