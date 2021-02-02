
with Ada.Text_IO;

with FITS;
with FITS_IO.V3_Types_For_DU;
with V3_Types; use V3_Types;

package body Debayer is

   package TIO renames Ada.Text_IO;

   procedure Grey_8
      (Scan_Length : Positive_Count;
      Frame : in out U8_Array)
   is
      Ix_First, Ix_Last,I,Iw : Positive_Count;
      Red, Gr1, Blue : Positive_Count; -- indexes
      Val : Unsigned_8;
   begin
      TIO.Put_Line("Grey_8");

      for Odd in 1 .. 510
         loop

            Ix_First := Frame'First + (FITS.Count(Odd - 1)*ScanLen);
            Ix_Last  := Ix_First + ScanLen - 1;

            I := Ix_First;
            Iw := I;
            while(I <= Ix_Last)
               loop

                  Red  := I;
                  Gr1  := I+1;--Red + 640;
                  Blue := I+2;--Gr1 + 640;

                  -- FIXME raw video moon.raw was actually rgb24 not Bayer RGGB
                  -- take new video, and set back to de-Bayer RGGB

                 -- TIO.Put(Count'Image(I)&"i");

                  Val := Frame(Red)/3 + Frame(Gr1)/3 + Frame(Blue)/3;

                  Frame(Iw) := Val;

                  if(I = 1)
                  then
                     Frame(Red) := 0;
                     Frame(Gr1) := 10;
                     Frame(Blue) := 5;
                  end if;
                  if(I = 4)
                  then
                     Frame(Red) := 0;
                     Frame(Gr1) := 10;
                     Frame(Blue) := 5;
                  end if;

                  I  := I  + 3;
                  Iw := Iw + 1;

               end loop;

         end loop;


      end Grey_8;




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

--            TIO.Put(Count'Image(Odd) &": " & Count'Image(1 + Ix_Last - Ix_First) 
--                         & " = " & Count'Image(Ix_First)& " " & Count'Image(Ix_Last) );
--            TIO.New_Line;

            I := Ix_First;
            while(I <= Ix_Last)
               loop

                  -- Bayer pattern
                  -- RG...........
                  -- GB...........
                  Red  := I;
                  Gr1  := Red + 1;
                  Gr2  := Red + ScanLen;
                  Blue := Gr2 + 1;

                  Val := Frame(Red)/3 + Frame(Gr1)/3 + Frame(Blue)/3;

                  Frame(Red) := Val;
                  Frame(Gr1) := Val;
                  Frame(Gr2) := Val;
                  Frame(Blue) := Val;

                  I := I + 2;

               end loop;

               Odd := Odd + 2;

         end loop;


      end Closest_Neighbour;


   end Debayer;
