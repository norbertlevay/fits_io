
with Mandatory; use Mandatory; -- Positive_Arr needed
with Keyword_record; use Keyword_Record; -- FPositive needed

package NCube_Funcs is

 function To_Offset (Coords    : in  Positive_Arr;
                     MaxCoords : in  Positive_Arr)
   return FPositive;



 procedure To_Coords (Offset    : in  FPositive;
                      MaxCoords : in  Positive_Arr;
                      Coords    : out Positive_Arr);

end NCube_Funcs;


