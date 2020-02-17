
with Mandatory; use Mandatory; -- NAXIS_Arr needed
with Keyword_record; use Keyword_Record; -- FPositive needed

package NCube_Funcs is

 function To_Offset (Coords    : in  NAXIS_Arr;
                     MaxCoords : in  NAXIS_Arr)
   return FPositive;



 procedure To_Coords (Offset    : in  FPositive;
                      MaxCoords : in  NAXIS_Arr;
                      Coords    : out NAXIS_Arr);

end NCube_Funcs;


