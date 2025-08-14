-- Test of the Threefish cipher for blocks of 1024 bits (Threefish-1024)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: GPL-3.0-only
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com

with Ada.Text_IO;
with PragmARC.Images;
with PragmARC.Encryption.Threefish.Block_1024;

procedure Tf_Test_1024 is
   use PragmARC.Encryption;

   Key1   : constant Threefish.Block_1024.Block := (others => 0);
   Tweak1 : constant Threefish.Couple           := (others => 0);
   In1    : constant Threefish.Block_1024.Block := (others => 0);
   Out1   : constant Threefish.Block_1024.Block := -- f05c3d0a3d05b304 f785ddc7d1e03601 5c8aa76e2f217b06 c6e1544c0bc1a90d
                                                   -- f0accb9473c24e0f d54fea68057f4332 9cb454761d6df5cf 7b2e9b3614fbd5a2
                                                   -- 0b2e4760b4060354 0d82eabc5482c171 c832afbe68406bc3 9500367a592943fa
                                                   -- 9a5b4a43286ca3c4 cf46104b443143d5 60a4b230488311df 4feef7e1dfe8391e
      Threefish.Block_1024.Block_From_Bytes ( (16#f0#, 16#5c#, 16#3d#, 16#0a#, 16#3d#, 16#05#, 16#b3#, 16#04#,
                                               16#f7#, 16#85#, 16#dd#, 16#c7#, 16#d1#, 16#e0#, 16#36#, 16#01#,
                                               16#5c#, 16#8a#, 16#a7#, 16#6e#, 16#2f#, 16#21#, 16#7b#, 16#06#,
                                               16#c6#, 16#e1#, 16#54#, 16#4c#, 16#0b#, 16#c1#, 16#a9#, 16#0d#,
                                               16#f0#, 16#ac#, 16#cb#, 16#94#, 16#73#, 16#c2#, 16#4e#, 16#0f#,
                                               16#d5#, 16#4f#, 16#ea#, 16#68#, 16#05#, 16#7f#, 16#43#, 16#32#,
                                               16#9c#, 16#b4#, 16#54#, 16#76#, 16#1d#, 16#6d#, 16#f5#, 16#cf#,
                                               16#7b#, 16#2e#, 16#9b#, 16#36#, 16#14#, 16#fb#, 16#d5#, 16#a2#,
                                               16#0b#, 16#2e#, 16#47#, 16#60#, 16#b4#, 16#06#, 16#03#, 16#54#,
                                               16#0d#, 16#82#, 16#ea#, 16#bc#, 16#54#, 16#82#, 16#c1#, 16#71#,
                                               16#c8#, 16#32#, 16#af#, 16#be#, 16#68#, 16#40#, 16#6b#, 16#c3#,
                                               16#95#, 16#00#, 16#36#, 16#7a#, 16#59#, 16#29#, 16#43#, 16#fa#,
                                               16#9a#, 16#5b#, 16#4a#, 16#43#, 16#28#, 16#6c#, 16#a3#, 16#c4#,
                                               16#cf#, 16#46#, 16#10#, 16#4b#, 16#44#, 16#31#, 16#43#, 16#d5#,
                                               16#60#, 16#a4#, 16#b2#, 16#30#, 16#48#, 16#83#, 16#11#, 16#df#,
                                               16#4f#, 16#ee#, 16#f7#, 16#e1#, 16#df#, 16#e8#, 16#39#, 16#1e#) );
--     (16#04b3053d0a3d5cf0#, 16#0136e0d1c7dd85f7#, 16#067b212f6ea78a5c#, 16#0da9c10b4c54e1c6#,
--      16#0f4ec27394cbacf0#, 16#32437f0568ea4fd5#, 16#cff56d1d7654b49c#, 16#a2d5fb14369b2e7b#,
--      16#540306b460472e0b#, 16#71c18254bcea820d#, 16#c36b4068beaf32c8#, 16#fa4329597a360095#,
--      16#c4a36c28434a5b9a#, 16#d54331444b1046cf#, 16#df11834830b2a460#, 16#1e39e8dfe1f7ee4f#);
   Key2   : constant Threefish.Block_1024.Block := -- 1011121314151617 18191a1b1c1d1e1f 2021222324252627 28292a2b2c2d2e2f
                                                   -- 3031323334353637 38393a3b3c3d3e3f 4041424344454647 48494a4b4c4d4e4f
                                                   -- 5051525354555657 58595a5b5c5d5e5f 6061626364656667 68696a6b6c6d6e6f
                                                   -- 7071727374757677 78797a7b7c7d7e7f 8081828384858687 88898a8b8c8d8e8f
      Threefish.Block_1024.Block_From_Bytes ( (16#10#, 16#11#, 16#12#, 16#13#, 16#14#, 16#15#, 16#16#, 16#17#,
                                               16#18#, 16#19#, 16#1a#, 16#1b#, 16#1c#, 16#1d#, 16#1e#, 16#1f#,
                                               16#20#, 16#21#, 16#22#, 16#23#, 16#24#, 16#25#, 16#26#, 16#27#,
                                               16#28#, 16#29#, 16#2a#, 16#2b#, 16#2c#, 16#2d#, 16#2e#, 16#2f#,
                                               16#30#, 16#31#, 16#32#, 16#33#, 16#34#, 16#35#, 16#36#, 16#37#,
                                               16#38#, 16#39#, 16#3a#, 16#3b#, 16#3c#, 16#3d#, 16#3e#, 16#3f#,
                                               16#40#, 16#41#, 16#42#, 16#43#, 16#44#, 16#45#, 16#46#, 16#47#,
                                               16#48#, 16#49#, 16#4a#, 16#4b#, 16#4c#, 16#4d#, 16#4e#, 16#4f#,
                                               16#50#, 16#51#, 16#52#, 16#53#, 16#54#, 16#55#, 16#56#, 16#57#,
                                               16#58#, 16#59#, 16#5a#, 16#5b#, 16#5c#, 16#5d#, 16#5e#, 16#5f#,
                                               16#60#, 16#61#, 16#62#, 16#63#, 16#64#, 16#65#, 16#66#, 16#67#,
                                               16#68#, 16#69#, 16#6a#, 16#6b#, 16#6c#, 16#6d#, 16#6e#, 16#6f#,
                                               16#70#, 16#71#, 16#72#, 16#73#, 16#74#, 16#75#, 16#76#, 16#77#,
                                               16#78#, 16#79#, 16#7a#, 16#7b#, 16#7c#, 16#7d#, 16#7e#, 16#7f#,
                                               16#80#, 16#81#, 16#82#, 16#83#, 16#84#, 16#85#, 16#86#, 16#87#,
                                               16#88#, 16#89#, 16#8a#, 16#8b#, 16#8c#, 16#8d#, 16#8e#, 16#8f#) );
--     (16#1716151413121110#, 16#1f1e1d1c1b1a1918#, 16#2726252423222120#, 16#2f2e2d2c2b2a2928#,
--      16#3736353433323130#, 16#3f3e3d3c3b3a3938#, 16#4746454443424140#, 16#4f4e4d4c4b4a4948#,
--      16#5756555453525150#, 16#5f5e5d5c5b5a5958#, 16#6766656463626160#, 16#6f6e6d6c6b6a6968#,
--      16#7776757473727170#, 16#7f7e7d7c7b7a7978#, 16#8786858483828180#, 16#8f8e8d8c8b8a8988#);
   Tweak2 : constant Threefish.Couple := -- 000102030405060708090a0b0c0d0e0f
      (Threefish.Word_From_Bytes ( (16#00#, 16#01#, 16#02#, 16#03#, 16#04#, 16#05#, 16#06#, 16#07#) ),
       Threefish.Word_From_Bytes ( (16#08#, 16#09#, 16#0a#, 16#0b#, 16#0c#, 16#0d#, 16#0e#, 16#0f#) ) );
--      (16#0706050403020100#, 16#0f0e0d0c0b0a0908#)
   In2    : constant Threefish.Block_1024.Block := -- fffefdfcfbfaf9f8 f7f6f5f4f3f2f1f0 efeeedecebeae9e8 e7e6e5e4e3e2e1e0
                                                   -- dfdedddcdbdad9d8 d7d6d5d4d3d2d1d0 cfcecdcccbcac9c8 c7c6c5c4c3c2c1c0
                                                   -- bfbebdbcbbbab9b8 b7b6b5b4b3b2b1b0 afaeadacabaaa9a8 a7a6a5a4a3a2a1a0
                                                   -- 9f9e9d9c9b9a9998 9796959493929190 8f8e8d8c8b8a8988 8786858483828180
      Threefish.Block_1024.Block_From_Bytes ( (16#ff#, 16#fe#, 16#fd#, 16#fc#, 16#fb#, 16#fa#, 16#f9#, 16#f8#,
                                               16#f7#, 16#f6#, 16#f5#, 16#f4#, 16#f3#, 16#f2#, 16#f1#, 16#f0#,
                                               16#ef#, 16#ee#, 16#ed#, 16#ec#, 16#eb#, 16#ea#, 16#e9#, 16#e8#,
                                               16#e7#, 16#e6#, 16#e5#, 16#e4#, 16#e3#, 16#e2#, 16#e1#, 16#e0#,
                                               16#df#, 16#de#, 16#dd#, 16#dc#, 16#db#, 16#da#, 16#d9#, 16#d8#,
                                               16#d7#, 16#d6#, 16#d5#, 16#d4#, 16#d3#, 16#d2#, 16#d1#, 16#d0#,
                                               16#cf#, 16#ce#, 16#cd#, 16#cc#, 16#cb#, 16#ca#, 16#c9#, 16#c8#,
                                               16#c7#, 16#c6#, 16#c5#, 16#c4#, 16#c3#, 16#c2#, 16#c1#, 16#c0#,
                                               16#bf#, 16#be#, 16#bd#, 16#bc#, 16#bb#, 16#ba#, 16#b9#, 16#b8#,
                                               16#b7#, 16#b6#, 16#b5#, 16#b4#, 16#b3#, 16#b2#, 16#b1#, 16#b0#,
                                               16#af#, 16#ae#, 16#ad#, 16#ac#, 16#ab#, 16#aa#, 16#a9#, 16#a8#,
                                               16#a7#, 16#a6#, 16#a5#, 16#a4#, 16#a3#, 16#a2#, 16#a1#, 16#a0#,
                                               16#9f#, 16#9e#, 16#9d#, 16#9c#, 16#9b#, 16#9a#, 16#99#, 16#98#,
                                               16#97#, 16#96#, 16#95#, 16#94#, 16#93#, 16#92#, 16#91#, 16#90#,
                                               16#8f#, 16#8e#, 16#8d#, 16#8c#, 16#8b#, 16#8a#, 16#89#, 16#88#,
                                               16#87#, 16#86#, 16#85#, 16#84#, 16#83#, 16#82#, 16#81#, 16#80#) );
-- (16#f8f9fafbfcfdfeff#, 16#f0f1f2f3f4f5f6f7#, 16#e8e9eaebecedeeef#, 16#e0e1e2e3e4e5e6e7#,
--  16#d8d9dadbdcdddedf#, 16#d0d1d2d3d4d5d6d7#, 16#c8c9cacbcccdcecf#, 16#c0c1c2c3c4c5c6c7#,
--  16#b8b9babbbcbdbebf#, 16#b0b1b2b3b4b5b6b7#, 16#a8a9aaabacadaeaf#, 16#a0a1a2a3a4a5a6a7#,
--  16#98999a9b9c9d9e9f#, 16#9091929394959697#, 16#88898a8b8c8d8e8f#, 16#8081828384858687#);
   Out2   : constant Threefish.Block_1024.Block := -- a6654ddbd73cc3b0 5dd777105aa849bc e49372eaaffc5568 d254771bab85531c
                                                   -- 94f780e7ffaae430 d5d8af8c70eebbe1 760f3b42b737a89c b363490d670314bd
                                                   -- 8aa41ee63c2e1f45 fbd477922f8360b3 88d6125ea6c7af0a d7056d01796e90c8
                                                   -- 3313f4150a5716b3 0ed5f569288ae974 ce2b4347926fce57 de44512177dd7cde
      Threefish.Block_1024.Block_From_Bytes ( (16#a6#, 16#65#, 16#4d#, 16#db#, 16#d7#, 16#3c#, 16#c3#, 16#b0#,
                                               16#5d#, 16#d7#, 16#77#, 16#10#, 16#5a#, 16#a8#, 16#49#, 16#bc#,
                                               16#e4#, 16#93#, 16#72#, 16#ea#, 16#af#, 16#fc#, 16#55#, 16#68#,
                                               16#d2#, 16#54#, 16#77#, 16#1b#, 16#ab#, 16#85#, 16#53#, 16#1c#,
                                               16#94#, 16#f7#, 16#80#, 16#e7#, 16#ff#, 16#aa#, 16#e4#, 16#30#,
                                               16#d5#, 16#d8#, 16#af#, 16#8c#, 16#70#, 16#ee#, 16#bb#, 16#e1#,
                                               16#76#, 16#0f#, 16#3b#, 16#42#, 16#b7#, 16#37#, 16#a8#, 16#9c#,
                                               16#b3#, 16#63#, 16#49#, 16#0d#, 16#67#, 16#03#, 16#14#, 16#bd#,
                                               16#8a#, 16#a4#, 16#1e#, 16#e6#, 16#3c#, 16#2e#, 16#1f#, 16#45#,
                                               16#fb#, 16#d4#, 16#77#, 16#92#, 16#2f#, 16#83#, 16#60#, 16#b3#,
                                               16#88#, 16#d6#, 16#12#, 16#5e#, 16#a6#, 16#c7#, 16#af#, 16#0a#,
                                               16#d7#, 16#05#, 16#6d#, 16#01#, 16#79#, 16#6e#, 16#90#, 16#c8#,
                                               16#33#, 16#13#, 16#f4#, 16#15#, 16#0a#, 16#57#, 16#16#, 16#b3#,
                                               16#0e#, 16#d5#, 16#f5#, 16#69#, 16#28#, 16#8a#, 16#e9#, 16#74#,
                                               16#ce#, 16#2b#, 16#43#, 16#47#, 16#92#, 16#6f#, 16#ce#, 16#57#,
                                               16#de#, 16#44#, 16#51#, 16#21#, 16#77#, 16#dd#, 16#7c#, 16#de#) );

--     (16#b0c33cd7db4d65a6#, 16#bc49a85a1077d75d#, 16#6855fcafea7293e4#, 16#1c5385ab1b7754d2#,
--      16#30e4aaffe780f794#, 16#e1bbee708cafd8d5#, 16#9ca837b7423b0f76#, 16#bd1403670d4963b3#,
--      16#451f2e3ce61ea48a#, 16#b360832f9277d4fb#, 16#0aafc7a65e12d688#, 16#c8906e79016d05d7#,
--      16#b316570a15f41333#, 16#74e98a2869f5d50e#, 16#57ce6f9247432bce#, 16#de7cdd77215144de#);
   Key3   : constant Threefish.Block_1024.Block := (1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768);
   Tweak3 : constant Threefish.Couple           := (16, 32);

   function Image is new PragmARC.Images.Modular_Image (Number => Threefish.Word);
   function Image is new PragmARC.Images.Modular_Image (Number => PragmARC.Byte);

   procedure Put (Text : in Threefish.Block_1024.Block); -- Outputs the images of the words ot Text on a single line

   procedure Put (Text : in Threefish.Block_1024.Block) is
      -- Empty
   begin -- Put
      All_Words : for I in Text'Range loop
         Ada.Text_IO.Put (Item => ' ' & Image (Text (I), Width => 16, Zero_Filled => True, Base => 16) );
      end loop All_Words;
   end Put;

   Ks   : Threefish.Block_1024.Key_Schedule_Handle;
   Text : Threefish.Block_1024.Block;

   use type Threefish.Block_1024.Block;
begin -- Tf_Test
   Threefish.Block_1024.Create_Key_Schedule (Key => Key1, Tweak =>Tweak1, Key_Schedule => Ks);
   Ada.Text_IO.Put_Line (Item => "Test case 1: Encryption key = (others => 0), tweak = (others => 0), text = (others => 0)");
   Text := In1;
   Threefish.Block_1024.Encrypt (Key_Schedule => Ks, Text => Text);
   Put (Text => Text);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line (Item => "Test passes: " & Boolean'Image (Text = Out1) );
   Threefish.Block_1024.Decrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put_Line (Item => "Decryption Test passes: " & Boolean'Image (Text = in1) );
   Ada.Text_IO.Put (Item => "Decrypts to:");
   Put (Text => Text);
   Ada.Text_IO.New_Line (2);

   Threefish.Block_1024.Create_Key_Schedule (Key => Key2, Tweak =>Tweak2, Key_Schedule => Ks);
   Ada.Text_IO.Put (Item => "Test case 2: key =");
   Put (Text => Key2);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line (Item => "tweak = " & Image (Tweak2 (0), Width => 16, Zero_Filled => True, Base => 16) & ' ' &
                                 Image (Tweak2 (1), Width => 16, Zero_Filled => True, Base => 16) );
   Ada.Text_IO.Put_Line (Item => "text =");
   Put (Text => In2);
   Ada.Text_IO.New_Line;
   Text := In2;
   Threefish.Block_1024.Encrypt (Key_Schedule => Ks, Text => Text);
   Put (Text => Text);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line (Item => "Test passes: " & Boolean'Image (Text = Out2) );
   Threefish.Block_1024.Decrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put_Line (Item => "Decryption Test passes: " & Boolean'Image (Text = in2) );
   Ada.Text_IO.Put (Item => "Decrypts to:");
   Put (Text => Text);
   Ada.Text_IO.New_Line(2);

   Threefish.Block_1024.Create_Key_Schedule (Key => Key3, Tweak =>Tweak3, Key_Schedule => Ks);
   Ada.Text_IO.Put_Line(Item => "Key is now (1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768), tweak (16, 32)");
   Text := (others => 0);
   Threefish.Block_1024.Encrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put (Item => "Encryption of (others => 0):");
   Put (Text => Text);
   Ada.Text_IO.New_Line;
   Threefish.Block_1024.Decrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put (Item => "Decrypts to:");
   Put (Text => Text);
   Ada.Text_IO.New_Line (2);

   Text := (others => 16#4141414141414141#); -- All 'A's
   Threefish.Block_1024.Encrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put (Item => "Encryption of (others => 16#4141414141414141#):");
   Put (Text => Text);
   Ada.Text_IO.New_Line;
   Threefish.Block_1024.Decrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put (Item => "Decrypts to:");
   Put (Text => Text);
   Ada.Text_IO.New_Line (2);

   Text := Key3;
   Threefish.Block_1024.Encrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put (Item => "Encryption of (1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768):");
   Put (Text => Text);
   Ada.Text_IO.New_Line;
   Threefish.Block_1024.Decrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put (Item => "Decrypts to:");
   Put (Text => Text);
   Ada.Text_IO.New_Line (2);

   declare
      Ct : constant Threefish.Block_1024.Block_List := -- "ABCDEFGHIJKL"
         Threefish.Block_1024.Encrypt
            (Ks, (16#41#, 16#42#, 16#43#, 16#44#, 16#45#, 16#46#, 16#47#, 16#48#, 16#49#, 16#50#, 16#51#, 16#52#) );
      Pt : constant PragmARC.Byte_List  := Threefish.Block_1024.Decrypt (Ks, Ct);
   begin
      Ada.Text_IO.Put (Item => "Encryption of (16#41# .. 16#52#):");
      for I in Ct'Range loop
         Put (Text => Ct (I) );
         Ada.Text_IO.New_Line;
      end loop;
      Ada.Text_IO.Put (Item => "Decrypts to:");
      for I in Pt'Range loop
         Ada.Text_IO.Put (Item => ' ' & Image (Pt (I), Width => 2, Zero_Filled => True, Base => 16) );
      end loop;
      Ada.Text_IO.New_Line;
   end;
end Tf_Test_1024;
