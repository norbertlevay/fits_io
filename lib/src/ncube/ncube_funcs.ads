
with Mandatory; use Mandatory; -- NAXIS_Arr needed

--with Keyword_record; use Keyword_Record; -- FPositive needed
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO; -- Positive_Count needed

package NCube_Funcs is

 function To_Offset (Coords    : in  NAXIS_Arr;
                     MaxCoords : in  NAXIS_Arr)
   return Positive_Count;



 procedure To_Coords (Offset    : in  Positive_Count;
                      MaxCoords : in  NAXIS_Arr;
                      Coords    : out NAXIS_Arr);

end NCube_Funcs;


