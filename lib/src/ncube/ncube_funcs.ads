
with Mandatory; use Mandatory; -- NAXIS_Arr needed

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO; -- Positive_Count needed

package NCube_Funcs is

 function To_DU_Index (Coords    : in  NAXIS_Arr;
                     MaxCoords : in  NAXIS_Arr)
   return Positive_Count;


 procedure To_Coords (Offset    : in  Positive_Count;
                      MaxCoords : in  NAXIS_Arr;
                      Coords    : out NAXIS_Arr);



 function Plane_Length  (Plane       : in NAXIS_Arr) return Positive_Count;
 function Volume_Length (First, Last : in NAXIS_Arr) return Positive_Count;
 -- should be used to size T_Arr in Raw and Physical

end NCube_Funcs;


