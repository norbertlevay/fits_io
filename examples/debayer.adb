
with Ada.Text_IO;

with FITS;
with FITS_IO.V3_Types_For_DU;
with V3_Types; use V3_Types;

package body Debayer is

   package TIO renames Ada.Text_IO;

   procedure Closest_Neighbour
      (Col_Length : Positive_Count;
      Frame : in out U8_Array)
   is
      Ix_First, Ix_Last : Positive_Count;
      Odd : Count := 1;
      Red, Gr1, Gr2, Blue : Positive_Count; -- indexes
      Val : Unsigned_8;
      Do_Calc : Boolean := True;
   begin
      TIO.Put_Line("Closest_Neighbour");

      while(Odd < (RowsCnt - 2)) -- 1 .. 511=(513 - 2)
         loop

            Odd := Odd + 2;

            Ix_First := Frame'First + ((Odd - 1)*ColsCnt);
            Ix_Last  := Ix_First + ColsCnt - 1;

            TIO.Put(Count'Image(Odd) &": " & Count'Image(1 + Ix_Last - Ix_First) & " = " & Count'Image(Ix_First)& " " & Count'Image(Ix_Last) );
                  TIO.New_Line;


            Do_Calc := True;
            for I in Ix_First .. Ix_Last
            loop

               if(Do_Calc)
               then

                  Red := I;
                  Gr1 := Red + 1;
                  Gr2 := Red + ColsCnt;
                  Blue := Gr2 + 1;

                  Val := Frame(Red)/4 + Frame(Gr1)/4 + Frame(Gr2)/4 + Frame(Blue)/4;

                  Frame(Red) := Val;
                  Frame(Gr1) := Val;
                  Frame(Gr2) := Val;
                  Frame(Blue) := Val;

               end if;

               Do_Calc := not Do_Calc;

            end loop;

         end loop;


      end Closest_Neighbour;


   end Debayer;
