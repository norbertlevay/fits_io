
with Ada.Text_IO;

with FITS;
with FITS_IO.V3_Types_For_DU;
with V3_Types; use V3_Types;

package body Debayer is

   package TIO renames Ada.Text_IO;

   procedure Closest_Neighbour
      (Scan_Length : Positive_Count;
      Frame : in out U8_Array)
   is
      Ix_First, Ix_Last,I : Positive_Count;
      Odd : Count := 1;
      Red, Gr1, Gr2, Blue : Positive_Count; -- indexes
      Val : Unsigned_8;
   begin
      TIO.Put_Line("Closest_Neighbour");

      while(Odd < (ScanCnt - 2)) -- 1 .. 511=(513 - 2)
         loop

            Ix_First := Frame'First + ((Odd - 1)*ScanLen);
            Ix_Last  := Ix_First + ScanLen - 1;

--            TIO.Put(Count'Image(Odd) &": " & Count'Image(1 + Ix_Last - Ix_First) & " = " & Count'Image(Ix_First)& " " & Count'Image(Ix_Last) );
--             TIO.New_Line;


--            TIO.Put_Line("------- New Scan -------");

            I := Ix_First;
            while(I <= Ix_Last)
               loop


                  Red := I;
                  Gr1 := Red + ScanLen;
                  Blue := Gr1 + ScanLen;
                  --Gr2 := Blue + 1;

                  -- FIXME raw video moon.raw was actually rgb24 not Bayer RGGB
                  -- take new video, and set back to de-Bayer RGGB

                  Val := Frame(Red)/3 + Frame(Gr1)/3 + Frame(Blue)/3;

                  Frame(Red) := Val;
                  Frame(Gr1) := Val;
--                  Frame(Gr2) := Val;
                  Frame(Blue) := Val;

                  if(I = 1)
                  then
                     Frame(Red) := 0;
                     Frame(Gr1) := 10;
                    -- Frame(Gr2) := 10;
                     Frame(Blue) := 3;
                  end if;
                  if(I = 3)
                  then
                     Frame(Red) := 0;
                     Frame(Gr1) := 10;
                     --Frame(Gr2) := 10;
                     Frame(Blue) := 3;
                  end if;

                  I := I + 1;

               end loop;

               Odd := Odd + 3;

         end loop;


      end Closest_Neighbour;


   end Debayer;
