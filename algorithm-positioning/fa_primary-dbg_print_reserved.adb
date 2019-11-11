with Ada.Text_IO;



separate(FA_Primary)
procedure DBG_Print_Reserved
is
 package TIO renames Ada.Text_IO;
 Res : Key_Rec_Arr := Get((DATAMAX,DATAMIN,INSTRUME,TELESCOP));
begin
for I in Res'Range
loop
 TIO.Put("Get Res> "&Reserved_Key'Image(Res(I).Key));
 TIO.Put(" : "&Res(I).Value);
 TIO.New_Line;
end loop;

end DBG_Print_Reserved;


