
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

generic
package Scaling.Streams is

package SIO renames Ada.Streams.Stream_IO;

-- Read: replaces source-array with Stream
procedure Linear(Ssrc : SIO.Stream_Access; Aout : out Tdst.Numeric_Arr);

-- Write: replaces destination-array with Stream
procedure Linear(Ain : in Tsrc.Numeric_Arr; Sdst : SIO.Stream_Access);

end Scaling.Streams;

