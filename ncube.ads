
with FITS.Data; -- Data_Arr needed

package ncube is

-- -------------------------------------------------
 -- Solution 1:
 -- write all data in one step - "small" data compared to computer memory

 -- implements [FITS Sect ??] ordering N-dimensional data by columns
 -- into an 1-dimensional array

 type Coord_Type is array (FPositive range <> ) of FPositive;

 procedure To_Coords (Offset    : in  FPositive;
                      MaxCoords : in  Coord_Type;
                      Coords    : out Coord_Type);
 generic
  -- type Item is (<>); -- this allows only discete types like Integer
  type Item is private; -- this allows also Float
  type Coll is array(FPositive range <>) of Item;
  with function Value ( Coord : in Coord_Type ) return Item;
 procedure Fill_In (DataVector : in out Coll; MaxCoords : in Coord_Type);-- is null;
 -- FIXME:
 -- Rename Value() to:               Set(coord,Item)
 --   and Fill_In() to is Set array: Set_All(array,...)
 -- Add generic get func:            Get(array, coord) return Item


 -- Solution 2:
 -- For "big" data: write by group of N-rows
 -- ...

end ncube;

-- NOTES:
-- below compiles but see FIXME
-- generic
--  type Index is (<>);
--  type Item is (<>);-- OR: private; OR limited private;
--  with function DValue ( Coord : in Coord_Type ) return Item;
-- procedure DFill_In (Data_Acc :  access FITS.Data.Data_Arr);-- is null;
 -- FIXME above commented line works BUT
 --       constraint not allowed after .Data_Arr above: However we know that ValueType chould be DType
 --       use directly array !? instead of Data_Arr?


