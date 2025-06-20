-- Hash the test vectors in Appendix C of the Skein document
-- Copyright (C) 2025 by PragmAda Software Engineering
-- SPDX-License-Identifier: GPL-3.0-only
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com

with Ada.Text_IO;
with PragmARC.Encryption.Threefish;
with PragmARC.Images;
with PragmARC.Skein;

procedure Skein_Test is
   procedure Dump (List : in PragmARC.Skein.Byte_List);
   -- Outputs the values of List in hexadecimal, preceeded by spaces, followed by a new line

   procedure Dump (List : in PragmARC.Skein.Byte_List) is
      function Image is new PragmARC.Images.Modular_Image (Number => PragmARC.Encryption.Threefish.Byte);
   begin -- Dump
      All_Bytes : for B of List loop
         Ada.Text_IO.Put (Item => ' ' & Image (B, Width => 2, Zero_Filled => True, Base => 16) );
      end loop All_Bytes;

      Ada.Text_IO.New_Line;
   end Dump;

   use type PragmARC.Skein.Byte_List;

   Msg_1 : constant PragmARC.Skein.Byte_List := (1 => 16#FF#);
   Res_1 : constant PragmARC.Skein.Byte_List := (16#0B#, 16#98#, 16#DC#, 16#D1#, 16#98#, 16#EA#, 16#0E#, 16#50#,
                                                 16#A7#, 16#A2#, 16#44#, 16#C4#, 16#44#, 16#E2#, 16#5C#, 16#23#,
                                                 16#DA#, 16#30#, 16#C1#, 16#0F#, 16#C9#, 16#A1#, 16#F2#, 16#70#,
                                                 16#A6#, 16#63#, 16#7F#, 16#1F#, 16#34#, 16#E6#, 16#7E#, 16#D2#);
   Res_1_512 : constant PragmARC.Skein.Byte_List := (16#71#, 16#B7#, 16#BC#, 16#E6#, 16#FE#, 16#64#, 16#52#, 16#22#,
                                                     16#7B#, 16#9C#, 16#ED#, 16#60#, 16#14#, 16#24#, 16#9E#, 16#5B#,
                                                     16#F9#, 16#A9#, 16#75#, 16#4C#, 16#3A#, 16#D6#, 16#18#, 16#CC#,
                                                     16#C4#, 16#E0#, 16#AA#, 16#E1#, 16#6B#, 16#31#, 16#6C#, 16#C8#,
                                                     16#CA#, 16#69#, 16#8D#, 16#86#, 16#43#, 16#07#, 16#ED#, 16#3E#,
                                                     16#80#, 16#B6#, 16#EF#, 16#15#, 16#70#, 16#81#, 16#2A#, 16#C5#,
                                                     16#27#, 16#2D#, 16#C4#, 16#09#, 16#B5#, 16#A0#, 16#12#, 16#DF#,
                                                     16#2A#, 16#57#, 16#91#, 16#02#, 16#F3#, 16#40#, 16#61#, 16#7A#);
   Res_1_1024 : constant PragmARC.Skein.Byte_List := (16#E6#, 16#2C#, 16#05#, 16#80#, 16#2E#, 16#A0#, 16#15#, 16#24#,
                                                      16#07#, 16#CD#, 16#D8#, 16#78#, 16#7F#, 16#DA#, 16#9E#, 16#35#,
                                                      16#70#, 16#3D#, 16#E8#, 16#62#, 16#A4#, 16#FB#, 16#C1#, 16#19#,
                                                      16#CF#, 16#F8#, 16#59#, 16#0A#, 16#FE#, 16#79#, 16#25#, 16#0B#,
                                                      16#CC#, 16#C8#, 16#B3#, 16#FA#, 16#F1#, 16#BD#, 16#24#, 16#22#,
                                                      16#AB#, 16#5C#, 16#0D#, 16#26#, 16#3F#, 16#B2#, 16#F8#, 16#AF#,
                                                      16#B3#, 16#F7#, 16#96#, 16#F0#, 16#48#, 16#00#, 16#03#, 16#81#,
                                                      16#53#, 16#1B#, 16#6F#, 16#00#, 16#D8#, 16#51#, 16#61#, 16#BC#,
                                                      16#0F#, 16#FF#, 16#4B#, 16#EF#, 16#24#, 16#86#, 16#B1#, 16#EB#,
                                                      16#CD#, 16#37#, 16#73#, 16#FA#, 16#BF#, 16#50#, 16#AD#, 16#4A#,
                                                      16#D5#, 16#63#, 16#9A#, 16#F9#, 16#04#, 16#0E#, 16#3F#, 16#29#,
                                                      16#C6#, 16#C9#, 16#31#, 16#30#, 16#1B#, 16#F7#, 16#98#, 16#32#,
                                                      16#E9#, 16#DA#, 16#09#, 16#85#, 16#7E#, 16#83#, 16#1E#, 16#82#,
                                                      16#EF#, 16#8B#, 16#46#, 16#91#, 16#C2#, 16#35#, 16#65#, 16#65#,
                                                      16#15#, 16#D4#, 16#37#, 16#D2#, 16#BD#, 16#A3#, 16#3B#, 16#CE#,
                                                      16#C0#, 16#01#, 16#C6#, 16#7F#, 16#FD#, 16#E1#, 16#5B#, 16#A8#);
   Msg_2 : constant PragmARC.Skein.Byte_List := (16#FF#, 16#FE#, 16#FD#, 16#FC#, 16#FB#, 16#FA#, 16#F9#, 16#F8#,
                                                 16#F7#, 16#F6#, 16#F5#, 16#F4#, 16#F3#, 16#F2#, 16#F1#, 16#F0#,
                                                 16#EF#, 16#EE#, 16#ED#, 16#EC#, 16#EB#, 16#EA#, 16#E9#, 16#E8#,
                                                 16#E7#, 16#E6#, 16#E5#, 16#E4#, 16#E3#, 16#E2#, 16#E1#, 16#E0#);
   Res_2 : constant PragmARC.Skein.Byte_List := (16#8D#, 16#0F#, 16#A4#, 16#EF#, 16#77#, 16#7F#, 16#D7#, 16#59#,
                                                 16#DF#, 16#D4#, 16#04#, 16#4E#, 16#6F#, 16#6A#, 16#5A#, 16#C3#,
                                                 16#C7#, 16#74#, 16#AE#, 16#C9#, 16#43#, 16#DC#, 16#FC#, 16#07#,
                                                 16#92#, 16#7B#, 16#72#, 16#3B#, 16#5D#, 16#BF#, 16#40#, 16#8B#);
   Msg_3 : constant PragmARC.Skein.Byte_List := (16#FF#, 16#FE#, 16#FD#, 16#FC#, 16#FB#, 16#FA#, 16#F9#, 16#F8#,
                                                 16#F7#, 16#F6#, 16#F5#, 16#F4#, 16#F3#, 16#F2#, 16#F1#, 16#F0#,
                                                 16#EF#, 16#EE#, 16#ED#, 16#EC#, 16#EB#, 16#EA#, 16#E9#, 16#E8#,
                                                 16#E7#, 16#E6#, 16#E5#, 16#E4#, 16#E3#, 16#E2#, 16#E1#, 16#E0#,
                                                 16#DF#, 16#DE#, 16#DD#, 16#DC#, 16#DB#, 16#DA#, 16#D9#, 16#D8#,
                                                 16#D7#, 16#D6#, 16#D5#, 16#D4#, 16#D3#, 16#D2#, 16#D1#, 16#D0#,
                                                 16#CF#, 16#CE#, 16#CD#, 16#CC#, 16#CB#, 16#CA#, 16#C9#, 16#C8#,
                                                 16#C7#, 16#C6#, 16#C5#, 16#C4#, 16#C3#, 16#C2#, 16#C1#, 16#C0#);
   Res_3 : constant PragmARC.Skein.Byte_List := (16#DF#, 16#28#, 16#E9#, 16#16#, 16#63#, 16#0D#, 16#0B#, 16#44#,
                                                 16#C4#, 16#A8#, 16#49#, 16#DC#, 16#9A#, 16#02#, 16#F0#, 16#7A#,
                                                 16#07#, 16#CB#, 16#30#, 16#F7#, 16#32#, 16#31#, 16#82#, 16#56#,
                                                 16#B1#, 16#5D#, 16#86#, 16#5A#, 16#C4#, 16#AE#, 16#16#, 16#2F#);
   Res_3_512 : constant PragmARC.Skein.Byte_List := (16#45#, 16#86#, 16#3B#, 16#A3#, 16#BE#, 16#0C#, 16#4D#, 16#FC#,
                                                     16#27#, 16#E7#, 16#5D#, 16#35#, 16#84#, 16#96#, 16#F4#, 16#AC#,
                                                     16#9A#, 16#73#, 16#6A#, 16#50#, 16#5D#, 16#93#, 16#13#, 16#B4#,
                                                     16#2B#, 16#2F#, 16#5E#, 16#AD#, 16#A7#, 16#9F#, 16#C1#, 16#7F#,
                                                     16#63#, 16#86#, 16#1E#, 16#94#, 16#7A#, 16#FB#, 16#1D#, 16#05#,
                                                     16#6A#, 16#A1#, 16#99#, 16#57#, 16#5A#, 16#D3#, 16#F8#, 16#C9#,
                                                     16#A3#, 16#CC#, 16#17#, 16#80#, 16#B5#, 16#E5#, 16#FA#, 16#4C#,
                                                     16#AE#, 16#05#, 16#0E#, 16#98#, 16#98#, 16#76#, 16#62#, 16#5B#);
   Msg_4 : constant PragmARC.Skein.Byte_List := (16#FF#, 16#FE#, 16#FD#, 16#FC#, 16#FB#, 16#FA#, 16#F9#, 16#F8#,
                                                 16#F7#, 16#F6#, 16#F5#, 16#F4#, 16#F3#, 16#F2#, 16#F1#, 16#F0#,
                                                 16#EF#, 16#EE#, 16#ED#, 16#EC#, 16#EB#, 16#EA#, 16#E9#, 16#E8#,
                                                 16#E7#, 16#E6#, 16#E5#, 16#E4#, 16#E3#, 16#E2#, 16#E1#, 16#E0#,
                                                 16#DF#, 16#DE#, 16#DD#, 16#DC#, 16#DB#, 16#DA#, 16#D9#, 16#D8#,
                                                 16#D7#, 16#D6#, 16#D5#, 16#D4#, 16#D3#, 16#D2#, 16#D1#, 16#D0#,
                                                 16#CF#, 16#CE#, 16#CD#, 16#CC#, 16#CB#, 16#CA#, 16#C9#, 16#C8#,
                                                 16#C7#, 16#C6#, 16#C5#, 16#C4#, 16#C3#, 16#C2#, 16#C1#, 16#C0#,
                                                 16#BF#, 16#BE#, 16#BD#, 16#BC#, 16#BB#, 16#BA#, 16#B9#, 16#B8#,
                                                 16#B7#, 16#B6#, 16#B5#, 16#B4#, 16#B3#, 16#B2#, 16#B1#, 16#B0#,
                                                 16#AF#, 16#AE#, 16#AD#, 16#AC#, 16#AB#, 16#AA#, 16#A9#, 16#A8#,
                                                 16#A7#, 16#A6#, 16#A5#, 16#A4#, 16#A3#, 16#A2#, 16#A1#, 16#A0#,
                                                 16#9F#, 16#9E#, 16#9D#, 16#9C#, 16#9B#, 16#9A#, 16#99#, 16#98#,
                                                 16#97#, 16#96#, 16#95#, 16#94#, 16#93#, 16#92#, 16#91#, 16#90#,
                                                 16#8F#, 16#8E#, 16#8D#, 16#8C#, 16#8B#, 16#8A#, 16#89#, 16#88#,
                                                 16#87#, 16#86#, 16#85#, 16#84#, 16#83#, 16#82#, 16#81#, 16#80#);
   Res_4 : constant PragmARC.Skein.Byte_List := (16#91#, 16#CC#, 16#A5#, 16#10#, 16#C2#, 16#63#, 16#C4#, 16#DD#,
                                                 16#D0#, 16#10#, 16#53#, 16#0A#, 16#33#, 16#07#, 16#33#, 16#09#,
                                                 16#62#, 16#86#, 16#31#, 16#F3#, 16#08#, 16#74#, 16#7E#, 16#1B#,
                                                 16#CB#, 16#AA#, 16#90#, 16#E4#, 16#51#, 16#CA#, 16#B9#, 16#2E#,
                                                 16#51#, 16#88#, 16#08#, 16#7A#, 16#F4#, 16#18#, 16#87#, 16#73#,
                                                 16#A3#, 16#32#, 16#30#, 16#3E#, 16#66#, 16#67#, 16#A7#, 16#A2#,
                                                 16#10#, 16#85#, 16#6F#, 16#74#, 16#21#, 16#39#, 16#00#, 16#00#,
                                                 16#71#, 16#F4#, 16#8E#, 16#8B#, 16#A2#, 16#A5#, 16#AD#, 16#B7#);
   Res_4_1024 : constant PragmARC.Skein.Byte_List := (16#1F#, 16#3E#, 16#02#, 16#C4#, 16#6F#, 16#B8#, 16#0A#, 16#3F#,
                                                      16#CD#, 16#2D#, 16#FB#, 16#BC#, 16#7C#, 16#17#, 16#38#, 16#00#,
                                                      16#B4#, 16#0C#, 16#60#, 16#C2#, 16#35#, 16#4A#, 16#F5#, 16#51#,
                                                      16#18#, 16#9E#, 16#BF#, 16#43#, 16#3C#, 16#3D#, 16#85#, 16#F9#,
                                                      16#FF#, 16#18#, 16#03#, 16#E6#, 16#D9#, 16#20#, 16#49#, 16#31#,
                                                      16#79#, 16#ED#, 16#7A#, 16#E7#, 16#FC#, 16#E6#, 16#9C#, 16#35#,
                                                      16#81#, 16#A5#, 16#A2#, 16#F8#, 16#2D#, 16#3E#, 16#0C#, 16#7A#,
                                                      16#29#, 16#55#, 16#74#, 16#D0#, 16#CD#, 16#7D#, 16#21#, 16#7C#,
                                                      16#48#, 16#4D#, 16#2F#, 16#63#, 16#13#, 16#D5#, 16#9A#, 16#77#,
                                                      16#18#, 16#EA#, 16#D0#, 16#7D#, 16#07#, 16#29#, 16#C2#, 16#48#,
                                                      16#51#, 16#D7#, 16#E7#, 16#D2#, 16#49#, 16#1B#, 16#90#, 16#2D#,
                                                      16#48#, 16#91#, 16#94#, 16#E6#, 16#B7#, 16#D3#, 16#69#, 16#DB#,
                                                      16#0A#, 16#B7#, 16#AA#, 16#10#, 16#6F#, 16#0E#, 16#E0#, 16#A3#,
                                                      16#9A#, 16#42#, 16#EF#, 16#C5#, 16#4F#, 16#18#, 16#D9#, 16#37#,
                                                      16#76#, 16#08#, 16#09#, 16#85#, 16#F9#, 16#07#, 16#57#, 16#4F#,
                                                      16#99#, 16#5E#, 16#C6#, 16#A3#, 16#71#, 16#53#, 16#A5#, 16#78#);
   Msg_5 : constant PragmARC.Skein.Byte_List := (16#FF#, 16#FE#, 16#FD#, 16#FC#, 16#FB#, 16#FA#, 16#F9#, 16#F8#,
                                                 16#F7#, 16#F6#, 16#F5#, 16#F4#, 16#F3#, 16#F2#, 16#F1#, 16#F0#,
                                                 16#EF#, 16#EE#, 16#ED#, 16#EC#, 16#EB#, 16#EA#, 16#E9#, 16#E8#,
                                                 16#E7#, 16#E6#, 16#E5#, 16#E4#, 16#E3#, 16#E2#, 16#E1#, 16#E0#,
                                                 16#DF#, 16#DE#, 16#DD#, 16#DC#, 16#DB#, 16#DA#, 16#D9#, 16#D8#,
                                                 16#D7#, 16#D6#, 16#D5#, 16#D4#, 16#D3#, 16#D2#, 16#D1#, 16#D0#,
                                                 16#CF#, 16#CE#, 16#CD#, 16#CC#, 16#CB#, 16#CA#, 16#C9#, 16#C8#,
                                                 16#C7#, 16#C6#, 16#C5#, 16#C4#, 16#C3#, 16#C2#, 16#C1#, 16#C0#,
                                                 16#BF#, 16#BE#, 16#BD#, 16#BC#, 16#BB#, 16#BA#, 16#B9#, 16#B8#,
                                                 16#B7#, 16#B6#, 16#B5#, 16#B4#, 16#B3#, 16#B2#, 16#B1#, 16#B0#,
                                                 16#AF#, 16#AE#, 16#AD#, 16#AC#, 16#AB#, 16#AA#, 16#A9#, 16#A8#,
                                                 16#A7#, 16#A6#, 16#A5#, 16#A4#, 16#A3#, 16#A2#, 16#A1#, 16#A0#,
                                                 16#9F#, 16#9E#, 16#9D#, 16#9C#, 16#9B#, 16#9A#, 16#99#, 16#98#,
                                                 16#97#, 16#96#, 16#95#, 16#94#, 16#93#, 16#92#, 16#91#, 16#90#,
                                                 16#8F#, 16#8E#, 16#8D#, 16#8C#, 16#8B#, 16#8A#, 16#89#, 16#88#,
                                                 16#87#, 16#86#, 16#85#, 16#84#, 16#83#, 16#82#, 16#81#, 16#80#,
                                                 16#7F#, 16#7E#, 16#7D#, 16#7C#, 16#7B#, 16#7A#, 16#79#, 16#78#,
                                                 16#77#, 16#76#, 16#75#, 16#74#, 16#73#, 16#72#, 16#71#, 16#70#,
                                                 16#6F#, 16#6E#, 16#6D#, 16#6C#, 16#6B#, 16#6A#, 16#69#, 16#68#,
                                                 16#67#, 16#66#, 16#65#, 16#64#, 16#63#, 16#62#, 16#61#, 16#60#,
                                                 16#5F#, 16#5E#, 16#5D#, 16#5C#, 16#5B#, 16#5A#, 16#59#, 16#58#,
                                                 16#57#, 16#56#, 16#55#, 16#54#, 16#53#, 16#52#, 16#51#, 16#50#,
                                                 16#4F#, 16#4E#, 16#4D#, 16#4C#, 16#4B#, 16#4A#, 16#49#, 16#48#,
                                                 16#47#, 16#46#, 16#45#, 16#44#, 16#43#, 16#42#, 16#41#, 16#40#,
                                                 16#3F#, 16#3E#, 16#3D#, 16#3C#, 16#3B#, 16#3A#, 16#39#, 16#38#,
                                                 16#37#, 16#36#, 16#35#, 16#34#, 16#33#, 16#32#, 16#31#, 16#30#,
                                                 16#2F#, 16#2E#, 16#2D#, 16#2C#, 16#2B#, 16#2A#, 16#29#, 16#28#,
                                                 16#27#, 16#26#, 16#25#, 16#24#, 16#23#, 16#22#, 16#21#, 16#20#,
                                                 16#1F#, 16#1E#, 16#1D#, 16#1C#, 16#1B#, 16#1A#, 16#19#, 16#18#,
                                                 16#17#, 16#16#, 16#15#, 16#14#, 16#13#, 16#12#, 16#11#, 16#10#,
                                                 16#0F#, 16#0E#, 16#0D#, 16#0C#, 16#0B#, 16#0A#, 16#09#, 16#08#,
                                                 16#07#, 16#06#, 16#05#, 16#04#, 16#03#, 16#02#, 16#01#, 16#00#);
   Res_5 : constant PragmARC.Skein.Byte_List := (16#84#, 16#2A#, 16#53#, 16#C9#, 16#9C#, 16#12#, 16#B0#, 16#CF#,
                                                 16#80#, 16#CF#, 16#69#, 16#49#, 16#1B#, 16#E5#, 16#E2#, 16#F7#,
                                                 16#51#, 16#5D#, 16#E8#, 16#73#, 16#3B#, 16#6E#, 16#A9#, 16#42#,
                                                 16#2D#, 16#FD#, 16#67#, 16#66#, 16#65#, 16#B5#, 16#FA#, 16#42#,
                                                 16#FF#, 16#B3#, 16#A9#, 16#C4#, 16#8C#, 16#21#, 16#77#, 16#77#,
                                                 16#95#, 16#08#, 16#48#, 16#CE#, 16#CD#, 16#B4#, 16#8F#, 16#64#,
                                                 16#0F#, 16#81#, 16#FB#, 16#92#, 16#BE#, 16#F6#, 16#F8#, 16#8F#,
                                                 16#7A#, 16#85#, 16#C1#, 16#F7#, 16#CD#, 16#14#, 16#46#, 16#C9#,
                                                 16#16#, 16#1C#, 16#0A#, 16#FE#, 16#8F#, 16#25#, 16#AE#, 16#44#,
                                                 16#4F#, 16#40#, 16#D3#, 16#68#, 16#00#, 16#81#, 16#C3#, 16#5A#,
                                                 16#A4#, 16#3F#, 16#64#, 16#0F#, 16#D5#, 16#FA#, 16#3C#, 16#3C#,
                                                 16#03#, 16#0B#, 16#CC#, 16#06#, 16#AB#, 16#AC#, 16#01#, 16#D0#,
                                                 16#98#, 16#BC#, 16#C9#, 16#84#, 16#EB#, 16#D8#, 16#32#, 16#27#,
                                                 16#12#, 16#92#, 16#1E#, 16#00#, 16#B1#, 16#BA#, 16#07#, 16#D6#,
                                                 16#D0#, 16#1F#, 16#26#, 16#90#, 16#70#, 16#50#, 16#25#, 16#5E#,
                                                 16#F2#, 16#C8#, 16#E2#, 16#4F#, 16#71#, 16#6C#, 16#52#, 16#A5#);

  Value_1 : PragmARC.Skein.Byte_List (1 .. PragmARC.Skein.Bytes_For_256_Bits);
  Value_2 : PragmARC.Skein.Byte_List (1 .. PragmARC.Skein.Bytes_For_512_Bits);
  Value_3 : PragmARC.Skein.Byte_List (1 .. PragmARC.Skein.Bytes_For_1024_Bits);
begin -- Skein_Test
   Ada.Text_IO.Put_Line (Item => "Skein-256-256");
   Dump (List => Msg_1);
   Value_1 := PragmARC.Skein.Hash (Msg_1, PragmARC.Skein.Bytes_For_256_Bits, PragmARC.Skein.Size_256);
   Dump (List => Value_1);
   Ada.Text_IO.Put_Line (Item => (if Value_1 = Res_1 then "Passed" else "Failed") );
   Dump (List => Msg_2);
   Value_1 := PragmARC.Skein.Hash (Msg_2, PragmARC.Skein.Bytes_For_256_Bits, PragmARC.Skein.Size_256);
   Dump (List => Value_1);
   Ada.Text_IO.Put_Line (Item => (if Value_1 = Res_2 then "Passed" else "Failed") );
   Dump (List => Msg_3);
   Value_1 := PragmARC.Skein.Hash (Msg_3, PragmARC.Skein.Bytes_For_256_Bits, PragmARC.Skein.Size_256);
   Dump (List => Value_1);
   Ada.Text_IO.Put_Line (Item => (if Value_1 = Res_3 then "Passed" else "Failed") );
   Ada.Text_IO.Put_Line (Item => "Skein-512-512");
   Dump (List => Msg_1);
   Value_2 := PragmARC.Skein.Hash (Msg_1, PragmARC.Skein.Bytes_For_512_Bits, PragmARC.Skein.Size_512);
   Dump (List => Value_2);
   Ada.Text_IO.Put_Line (Item => (if Value_2 = Res_1_512 then "Passed" else "Failed") );
   Dump (List => Msg_3);
   Value_2 := PragmARC.Skein.Hash (Msg_3, PragmARC.Skein.Bytes_For_512_Bits, PragmARC.Skein.Size_512);
   Dump (List => Value_2);
   Ada.Text_IO.Put_Line (Item => (if Value_2 = Res_3_512 then "Passed" else "Failed") );
   Dump (List => Msg_4);
   Value_2 := PragmARC.Skein.Hash (Msg_4, PragmARC.Skein.Bytes_For_512_Bits, PragmARC.Skein.Size_512);
   Dump (List => Value_2);
   Ada.Text_IO.Put_Line (Item => (if Value_2 = Res_4 then "Passed" else "Failed") );
   Ada.Text_IO.Put_Line (Item => "Skein-1024-1024");
   Dump (List => Msg_1);
   Value_3 := PragmARC.Skein.Hash (Msg_1, PragmARC.Skein.Bytes_For_1024_Bits, PragmARC.Skein.Size_1024);
   Dump (List => Value_3);
   Ada.Text_IO.Put_Line (Item => (if Value_3 = Res_1_1024 then "Passed" else "Failed") );
   Dump (List => Msg_4);
   Value_3 := PragmARC.Skein.Hash (Msg_4, PragmARC.Skein.Bytes_For_1024_Bits, PragmARC.Skein.Size_1024);
   Dump (List => Value_3);
   Ada.Text_IO.Put_Line (Item => (if Value_3 = Res_4_1024 then "Passed" else "Failed") );
   Dump (List => Msg_5);
   Value_3 := PragmARC.Skein.Hash (Msg_5, PragmARC.Skein.Bytes_For_1024_Bits, PragmARC.Skein.Size_1024);
   Dump (List => Value_3);
   Ada.Text_IO.Put_Line (Item => (if Value_3 = Res_5 then "Passed" else "Failed") );
end Skein_Test;
