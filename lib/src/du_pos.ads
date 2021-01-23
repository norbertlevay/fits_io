

-- implement all logic of checking access Index to be 
-- correct for given HDU
-- and setting / reading various offesets in:
-- Read_Write_Header() Read/Write_Cards() Read/Write_Data() funcs
-- and init and file-complete:
-- Open/Create() HDU_Type=Set_Ext_Numebr(F, i:T) and Close()


-- instantiate with e.g. SIO.Positive_Count etc...

generic
type T is range <>;
--with function Index(??) return T;
--with procedure Set_Index(??, To : T);
package DU_Pos is

   type Pos is
      record
         HDU_First : T;
         ENDCard_Pos : T;
         DU_First : T;
         DU_Last : T;
         DU_End_Written : Boolean;
      end record;

   procedure At_Header_Start(Ix : T);-- store HDU_First 
   procedure At_END Card(Ix : T); -- store ENDCard_Pos and add padding and DU_First and calc DU_Last and set DU_End_Written = False

   procedure At_DU_Last_Element_Written; -- set DU_End_Written = True and write padding
   function Last_DU_Elem_Position(Curr_Ix : T; Chunk_Len : T) return Boolean; -- return True (and caller has to do At_DU_Last_Elem_Written)
   -- based on Current Index and Chunk Length calc whether this will include/cover also DU_Last position

end DU_Pos;
