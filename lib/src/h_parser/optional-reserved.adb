


package body Optional.Reserved is














 function Parse_Reserved_Keys (Stream :not null access Ada.Streams.Root_Stream_Type'Class)
     return Reserved_Key_Arr
 is
    ResKeys : Reserved_Key_Arr(1..10);-- FIXME use Lists
    KeyRec  : Valued_Key_Record;
    Ix : SIO.Positive_Count := 1;
    use SIO;
 begin
     loop

         Valued_Key_Record'Read(Stream, KeyRec);

         for K in Array_Keys'Range
         loop
             if(KeyRec.Key = Array_Keys(K))
             then
                 ResKeys(Ix) := KeyRec;
                 Ix := Ix + 1;
            end if;
         end loop;

         exit when (KeyRec.Key = "END");
     end loop;

     -- return correct length array

     declare
         FoundKeys : Reserved_Key_Arr(1..(Ix-1));
     begin
         for I in FoundKeys'Range
         loop
             FoundKeys(I) := ResKeys(I);
         end loop;
        return FoundKeys;
     end;
 end Parse_Reserved_Keys;





end Optional.Reserved;
