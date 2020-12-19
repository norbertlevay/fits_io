

-- performs NCube coordinate<->LinearIndex conversions

with FITS_IO; use FITS_IO; -- Positive_Count needed

package Raw_Funcs is

 function To_DU_Index (Coords    : in  NAXIS_Array;
                     MaxCoords : in  NAXIS_Array)
   return Positive_Count;


 procedure To_Coords (Offset    : in  Positive_Count;
                      MaxCoords : in  NAXIS_Array;
                      Coords    : out NAXIS_Array);



 function Plane_Length  (Plane       : in NAXIS_Array) return Positive_Count;
 function Volume_Length (First, Last : in NAXIS_Array) return Positive_Count;
 -- should be used to size T_Arr in Raw and Physical


  procedure Coordinates
    (NAXISn : in NAXIS_Array; 
    BlockNum : in Positive_Count; 
    IndexInBlock : in Positive; 
    Coord : out NAXIS_Array) is null;
  -- util can be called within callbacks of DU Read/Write to determine coordinates of 
  -- any point if needed
  -- In callbacks do count Blocks and call above Coordinates() to determine
  -- coord of any data element in Block


end Raw_Funcs;

