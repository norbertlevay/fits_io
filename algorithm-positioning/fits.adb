
with Ada.Exceptions; use Ada.Exceptions;

package body FITS is


        -- FIXME Set Check_And_Set not used yet: should in the same module with type CardValue
    
        procedure Set (Card : in Card_Type; V : in out CardValue)
        is
        begin
                V.Value := String(Card(11..30));
                V.Read  := True;
        end Set;

        procedure Check_And_Set (Card : in Card_Type; V : in out CardValue)
        is
        begin
                if (NOT V.Read)
                then    
                        Set(Card, V); 
                else    
                        -- FIXME only duplicates with diff values raises exception
                        -- duplicate with equal values: make configurable what to do...
                        Raise_Exception(Duplicate_Card'Identity, Card);
                end if;
        end Check_And_Set;


end FITS;
