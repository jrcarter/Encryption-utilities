-- Test of the Threefish cipher for blocks of 512 bits (Threefish-512)
-- Copyright (C) 2022 by PragmAda Software Engineering
-- Released under the terms of the GPL license version 3; see https://opensource.org/licenses

with Ada.Text_IO;
with PragmARC.Images;
with PragmARC.Encryption.Threefish.Block_512;

procedure Tf_Test_512 is
   use PragmARC.Encryption;

   Key1   : constant Threefish.Block_512.Block := (others => 0);
   Tweak1 : constant Threefish.Couple          := (others => 0);
   In1    : constant Threefish.Block_512.Block := (others => 0);
   Out1   : constant Threefish.Block_512.Block := -- b1a2bbc6ef6025bc 40eb3822161f36e3 75d1bb0aee3186fb d19e47c5d479947b
                                                  -- 7bc2f8586e35f0cf f7e7f03084b0b7b1 f1ab3961a580a3e9 7eb41ea14a6d7bbe
      Threefish.Block_512.Block_From_Bytes ( (16#b1#, 16#a2#, 16#bb#, 16#c6#, 16#ef#, 16#60#, 16#25#, 16#bc#,
                                              16#40#, 16#eb#, 16#38#, 16#22#, 16#16#, 16#1f#, 16#36#, 16#e3#,
                                              16#75#, 16#d1#, 16#bb#, 16#0a#, 16#ee#, 16#31#, 16#86#, 16#fb#,
                                              16#d1#, 16#9e#, 16#47#, 16#c5#, 16#d4#, 16#79#, 16#94#, 16#7b#,
                                              16#7b#, 16#c2#, 16#f8#, 16#58#, 16#6e#, 16#35#, 16#f0#, 16#cf#,
                                              16#f7#, 16#e7#, 16#f0#, 16#30#, 16#84#, 16#b0#, 16#b7#, 16#b1#,
                                              16#f1#, 16#ab#, 16#39#, 16#61#, 16#a5#, 16#80#, 16#a3#, 16#e9#,
                                              16#7e#, 16#b4#, 16#1e#, 16#a1#, 16#4a#, 16#6d#, 16#7b#, 16#be#) );
--     (16#bc2560efc6bba2b1#, 16#e3361f162238eb40#, 16#fb8631ee0abbd175#, 16#7b9479d4c5479ed1#,
--      16#cff0356e58f8c27b#, 16#b1b7b08430f0e7f7#, 16#e9a380a56139abf1#, 16#be7b6d4aa11eb47e#);
   Key2   : constant Threefish.Block_512.Block := -- 1011121314151617 18191a1b1c1d1e1f 2021222324252627 28292a2b2c2d2e2f
                                                  -- 3031323334353637 38393a3b3c3d3e3f 4041424344454647 48494a4b4c4d4e4f
      Threefish.Block_512.Block_From_Bytes ( (16#10#, 16#11#, 16#12#, 16#13#, 16#14#, 16#15#, 16#16#, 16#17#,
                                              16#18#, 16#19#, 16#1a#, 16#1b#, 16#1c#, 16#1d#, 16#1e#, 16#1f#,
                                              16#20#, 16#21#, 16#22#, 16#23#, 16#24#, 16#25#, 16#26#, 16#27#,
                                              16#28#, 16#29#, 16#2a#, 16#2b#, 16#2c#, 16#2d#, 16#2e#, 16#2f#,
                                              16#30#, 16#31#, 16#32#, 16#33#, 16#34#, 16#35#, 16#36#, 16#37#,
                                              16#38#, 16#39#, 16#3a#, 16#3b#, 16#3c#, 16#3d#, 16#3e#, 16#3f#,
                                              16#40#, 16#41#, 16#42#, 16#43#, 16#44#, 16#45#, 16#46#, 16#47#,
                                              16#48#, 16#49#, 16#4a#, 16#4b#, 16#4c#, 16#4d#, 16#4e#, 16#4f#) );
--     (16#1716151413121110#, 16#1f1e1d1c1b1a1918#, 16#2726252423222120#, 16#2f2e2d2c2b2a2928#,
--      16#3736353433323130#, 16#3f3e3d3c3b3a3938#, 16#4746454443424140#, 16#4f4e4d4c4b4a4948#);
   Tweak2 : constant Threefish.Couple := -- 000102030405060708090a0b0c0d0e0f
      (Threefish.Word_From_Bytes ( (16#00#, 16#01#, 16#02#, 16#03#, 16#04#, 16#05#, 16#06#, 16#07#) ),
       Threefish.Word_From_Bytes ( (16#08#, 16#09#, 16#0a#, 16#0b#, 16#0c#, 16#0d#, 16#0e#, 16#0f#) ) );
--      (16#0706050403020100#, 16#0f0e0d0c0b0a0908#)
   In2    : constant Threefish.Block_512.Block := -- fffefdfcfbfaf9f8 f7f6f5f4f3f2f1f0 efeeedecebeae9e8 e7e6e5e4e3e2e1e0
                                                  -- dfdedddcdbdad9d8 d7d6d5d4d3d2d1d0 cfcecdcccbcac9c8 c7c6c5c4c3c2c1c0
      Threefish.Block_512.Block_From_Bytes ( (16#ff#, 16#fe#, 16#fd#, 16#fc#, 16#fb#, 16#fa#, 16#f9#, 16#f8#,
                                              16#f7#, 16#f6#, 16#f5#, 16#f4#, 16#f3#, 16#f2#, 16#f1#, 16#f0#,
                                              16#ef#, 16#ee#, 16#ed#, 16#ec#, 16#eb#, 16#ea#, 16#e9#, 16#e8#,
                                              16#e7#, 16#e6#, 16#e5#, 16#e4#, 16#e3#, 16#e2#, 16#e1#, 16#e0#,
                                              16#df#, 16#de#, 16#dd#, 16#dc#, 16#db#, 16#da#, 16#d9#, 16#d8#,
                                              16#d7#, 16#d6#, 16#d5#, 16#d4#, 16#d3#, 16#d2#, 16#d1#, 16#d0#,
                                              16#cf#, 16#ce#, 16#cd#, 16#cc#, 16#cb#, 16#ca#, 16#c9#, 16#c8#,
                                              16#c7#, 16#c6#, 16#c5#, 16#c4#, 16#c3#, 16#c2#, 16#c1#, 16#c0#) );
--     (16#f8f9fafbfcfdfeff#, 16#f0f1f2f3f4f5f6f7#, 16#e8e9eaebecedeeef#, 16#e0e1e2e3e4e5e6e7#,
--      16#d8d9dadbdcdddedf#, 16#d0d1d2d3d4d5d6d7#, 16#c8c9cacbcccdcecf#, 16#e7e6e5e4e3e2e1e0#);
   Out2   : constant Threefish.Block_512.Block := -- e304439626d45a2c b401cad8d636249a 6338330eb06d45dd 8b36b90e97254779
                                                  -- 272a0a8d99463504 784420ea18c9a725 af11dffea1016234 8927673d5c1caf3d
      Threefish.Block_512.Block_From_Bytes ( (16#e3#, 16#04#, 16#43#, 16#96#, 16#26#, 16#d4#, 16#5a#, 16#2c#,
                                              16#b4#, 16#01#, 16#ca#, 16#d8#, 16#d6#, 16#36#, 16#24#, 16#9a#,
                                              16#63#, 16#38#, 16#33#, 16#0e#, 16#b0#, 16#6d#, 16#45#, 16#dd#,
                                              16#8b#, 16#36#, 16#b9#, 16#0e#, 16#97#, 16#25#, 16#47#, 16#79#,
                                              16#27#, 16#2a#, 16#0a#, 16#8d#, 16#99#, 16#46#, 16#35#, 16#04#,
                                              16#78#, 16#44#, 16#20#, 16#ea#, 16#18#, 16#c9#, 16#a7#, 16#25#,
                                              16#af#, 16#11#, 16#df#, 16#fe#, 16#a1#, 16#01#, 16#62#, 16#34#,
                                              16#89#, 16#27#, 16#67#, 16#3d#, 16#5c#, 16#1c#, 16#af#, 16#3d#) );
--     (16#2c5ad426964304e3#, 16#9a2436d6d8ca01b4#, 16#dd456db00e333863#, 16#794725970eb9368b#,
--      16#043546998d0a2a27#, 16#25a7c918ea204478#, 16#346201a1fedf11af#, 16#3daf1c5c3d672789#);
   Key3   : constant Threefish.Block_512.Block := (1, 2, 4, 8, 16, 32, 64, 128);
   Tweak3 : constant Threefish.Couple          := (16, 32);

   function Image is new PragmARC.Images.Modular_Image (Number => Threefish.Word);
   function Image is new PragmARC.Images.Modular_Image (Number => Threefish.Byte);

   procedure Put (Text : in Threefish.Block_512.Block); -- Outputs the images of the words ot Text on a single line

   procedure Put (Text : in Threefish.Block_512.Block) is
      -- Empty
   begin -- Put
      All_Words : for I in Text'Range loop
         Ada.Text_IO.Put (Item => ' ' & Image (Text (I), Width => 16, Zero_Filled => True, Base => 16) );
      end loop All_Words;
   end Put;

   Ks   : Threefish.Block_512.Key_Schedule_Handle;
   Text : Threefish.Block_512.Block;

   use type Threefish.Block_512.Block;
begin -- Tf_Test
   Threefish.Block_512.Create_Key_Schedule (Key => Key1, Tweak =>Tweak1, Key_Schedule => Ks);
   Ada.Text_IO.Put_Line (Item => "Test case 1: Encryption key = (others => 0), tweak = (others => 0), text = (others => 0)");
   Text := In1;
   Threefish.Block_512.Encrypt (Key_Schedule => Ks, Text => Text);
   Put (Text => Text);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line (Item => "Test passes: " & Boolean'Image (Text = Out1) );
   Threefish.Block_512.Decrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put_Line (Item => "Decryption Test passes: " & Boolean'Image (Text = in1) );
   Ada.Text_IO.Put (Item => "Decrypts to:");
   Put (Text => Text);
   Ada.Text_IO.New_Line (2);

   Threefish.Block_512.Create_Key_Schedule (Key => Key2, Tweak =>Tweak2, Key_Schedule => Ks);
   Ada.Text_IO.Put (Item => "Test case 2: key =");
   Put (Text => Key2);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line (Item => "tweak = " & Image (Tweak2 (0), Width => 16, Zero_Filled => True, Base => 16) & ' ' &
                                 Image (Tweak2 (1), Width => 16, Zero_Filled => True, Base => 16) );
   Ada.Text_IO.Put_Line (Item => "text =");
   Put (Text => In2);
   Ada.Text_IO.New_Line;
   Text := In2;
   Threefish.Block_512.Encrypt (Key_Schedule => Ks, Text => Text);
   Put (Text => Text);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line (Item => "Test passes: " & Boolean'Image (Text = Out2) );
   Threefish.Block_512.Decrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put_Line (Item => "Decryption Test passes: " & Boolean'Image (Text = In2) );
   Ada.Text_IO.Put (Item => "Decrypts to:");
   Put (Text => Text);
   Ada.Text_IO.New_Line(2);

   Threefish.Block_512.Create_Key_Schedule (Key => Key3, Tweak =>Tweak3, Key_Schedule => Ks);
   Ada.Text_IO.Put_Line(Item => "Key is now (1, 2, 4, 8, 16, 32, 64, 128), tweak (16, 32)");
   Text := (others => 0);
   Threefish.Block_512.Encrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put (Item => "Encryption of (others => 0):");
   Put (Text => Text);
   Ada.Text_IO.New_Line;
   Threefish.Block_512.Decrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put (Item => "Decrypts to:");
   Put (Text => Text);
   Ada.Text_IO.New_Line (2);

   Text := (others => 16#4141414141414141#); -- All 'A's
   Threefish.Block_512.Encrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put (Item => "Encryption of (others => 16#4141414141414141#):");
   Put (Text => Text);
   Ada.Text_IO.New_Line;
   Threefish.Block_512.Decrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put (Item => "Decrypts to:");
   Put (Text => Text);
   Ada.Text_IO.New_Line (2);

   Text := Key3;
   Threefish.Block_512.Encrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put (Item => "Encryption of (1, 2, 4, 8, 16, 32, 64, 128):");
   Put (Text => Text);
   Ada.Text_IO.New_Line;
   Threefish.Block_512.Decrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put (Item => "Decrypts to:");
   Put (Text => Text);
   Ada.Text_IO.New_Line (2);

   declare
      Ct : constant Threefish.Block_512.Block_List := -- "ABCDEFGHIJKL"
         Threefish.Block_512.Encrypt
            (Ks, (16#41#, 16#42#, 16#43#, 16#44#, 16#45#, 16#46#, 16#47#, 16#48#, 16#49#, 16#50#, 16#51#, 16#52#) );
      Pt : constant Threefish.Byte_List := Threefish.Block_512.Decrypt (Ks, Ct);
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
end Tf_Test_512;
