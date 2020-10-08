

-- performs NCube coordinate<->LinearIndex conversions


with Mandatory; use Mandatory; -- NAXIS_Arr needed
--with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO; -- Positive_Count needed
with FITS; use FITS; -- Positive_Count needed

package Raw_Funcs is

 function To_DU_Index (Coords    : in  NAXIS_Arr;
                     MaxCoords : in  NAXIS_Arr)
   return Positive_Count;


 procedure To_Coords (Offset    : in  Positive_Count;
                      MaxCoords : in  NAXIS_Arr;
                      Coords    : out NAXIS_Arr);



 function Plane_Length  (Plane       : in NAXIS_Arr) return Positive_Count;
 function Volume_Length (First, Last : in NAXIS_Arr) return Positive_Count;
 -- should be used to size T_Arr in Raw and Physical


  procedure Coordinates
    (NAXISn : in NAXIS_Arr; 
    BlockNum : in Positive_Count; 
    IndexInBlock : in Positive; 
    Coord : out NAXIS_Arr) is null;
  -- util can be called within callbacks of DU Read/Write to determine coordinates of 
  -- any point if needed
  -- In callbacks do count Blocks and call above Coordinates() to determine
  -- coord of any data element in Block


end Raw_Funcs;

