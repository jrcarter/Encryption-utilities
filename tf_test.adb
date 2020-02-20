-- Test of the Threefish cipher for blocks of 256 bits (Threefish-256)
-- Copyright (C) 2020 by PragmAda Software Engineering
-- Released under the terms of the GPL license version 3; see https://opensource.org/licenses

with Ada.Text_IO;
with PragmARC.Images;
with Threefish;

procedure Tf_Test is
   Key1   : constant Threefish.Block  := (others => 0);
   Tweak1 : constant Threefish.Couple := (others => 0);
   In1    : constant Threefish.Block  := (others => 0);
   Out1   : constant Threefish.Block  := -- 84da2a1f8beaee947066ae3e3103f1ad536db1f4a1192495116b9f3ce6133fd8
      Threefish.Block_From_Bytes ( (16#84#, 16#da#, 16#2a#, 16#1f#, 16#8b#, 16#ea#, 16#ee#, 16#94#,
                                    16#70#, 16#66#, 16#ae#, 16#3e#, 16#31#, 16#03#, 16#f1#, 16#ad#,
                                    16#53#, 16#6d#, 16#b1#, 16#f4#, 16#a1#, 16#19#, 16#24#, 16#95#,
                                    16#11#, 16#6b#, 16#9f#, 16#3c#, 16#e6#, 16#13#, 16#3f#, 16#d8#) );
--     (16#94EEEA8B1F2ADA84#, 16#ADF103313EAE6670#, 16#952419A1F4B16D53#, 16#D83F13E63C9F6B11#);
   Key2   : constant Threefish.Block  := -- 101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f
      Threefish.Block_From_Bytes ( (16#10#, 16#11#, 16#12#, 16#13#, 16#14#, 16#15#, 16#16#, 16#17#,
                                    16#18#, 16#19#, 16#1a#, 16#1b#, 16#1c#, 16#1d#, 16#1e#, 16#1f#,
                                    16#20#, 16#21#, 16#22#, 16#23#, 16#24#, 16#25#, 16#26#, 16#27#,
                                    16#28#, 16#29#, 16#2a#, 16#2b#, 16#2c#, 16#2d#, 16#2e#, 16#2f#) );
--     (16#1716151413121110#, 16#1f1e1d1c1b1a1918#, 16#2726252423222120#, 16#2f2e2d2c2b2a2928#);
   Tweak2 : constant Threefish.Couple := -- 000102030405060708090a0b0c0d0e0f
      (Threefish.Word_From_Bytes ( (16#00#, 16#01#, 16#02#, 16#03#, 16#04#, 16#05#, 16#06#, 16#07#) ),
       Threefish.Word_From_Bytes ( (16#08#, 16#09#, 16#0a#, 16#0b#, 16#0c#, 16#0d#, 16#0e#, 16#0f#) ) );
--      (16#0706050403020100#, 16#0f0e0d0c0b0a0908#)
   In2    : constant Threefish.Block  := -- FFFEFDFCFBFAF9F8F7F6F5F4F3F2F1F0EFEEEDECEBEAE9E8E7E6E5E4E3E2E1E0
      Threefish.Block_From_Bytes ( (16#FF#, 16#FE#, 16#FD#, 16#FC#, 16#FB#, 16#FA#, 16#F9#, 16#F8#,
                                    16#F7#, 16#F6#, 16#F5#, 16#F4#, 16#F3#, 16#F2#, 16#F1#, 16#F0#,
                                    16#EF#, 16#EE#, 16#ED#, 16#EC#, 16#EB#, 16#EA#, 16#E9#, 16#E8#,
                                    16#E7#, 16#E6#, 16#E5#, 16#E4#, 16#E3#, 16#E2#, 16#E1#, 16#E0#) );
--     (16#f8f9fafbfcfdfeff#, 16#f0f1f2f3f4f5f6f7#, 16#e8e9eaebecedeeef#, 16#e0e1e2e3e4e5e6e7#);
   Out2   : constant Threefish.Block  := -- e0d091ff0eea8fdfc98192e62ed80ad59d865d08588df476657056b5955e97df
      Threefish.Block_From_Bytes ( (16#e0#, 16#d0#, 16#91#, 16#ff#, 16#0e#, 16#ea#, 16#8f#, 16#df#,
                                    16#c9#, 16#81#, 16#92#, 16#e6#, 16#2e#, 16#d8#, 16#0a#, 16#d5#,
                                    16#9d#, 16#86#, 16#5d#, 16#08#, 16#58#, 16#8d#, 16#f4#, 16#76#,
                                    16#65#, 16#70#, 16#56#, 16#b5#, 16#95#, 16#5e#, 16#97#, 16#df#) );
--     (16#df8fea0eff91d0e0#, 16#d50ad82ee69281c9#, 16#76f48d58085d869d#, 16#df975e95b5567065#);
   Key3   : constant Threefish.Block  := (1, 2, 4, 8);
   Tweak3 : constant Threefish.Couple := (16, 32);

   function Image is new PragmARC.Images.Modular_Image (Number => Threefish.Word);
   function Image is new PragmARC.Images.Modular_Image (Number => Threefish.Byte);

   procedure Put (Text : in Threefish.Block); -- Outputs the images of the words ot Text on a single line

   procedure Put (Text : in Threefish.Block) is
      -- Empty
   begin -- Put
      All_Words : for I in Text'Range loop
         Ada.Text_IO.Put (Item => ' ' & Image (Text (I), Width => 16, Zero_Filled => True, Base => 16) );
      end loop All_Words;
   end Put;

   Ks   : Threefish.Key_Schedule_Handle;
   Text : Threefish.Block;

   use type Threefish.Block;
begin -- Tf_Test
   Threefish.Create_Key_Schedule (Key => Key1, Tweak =>Tweak1, Key_Schedule => Ks);
   Ada.Text_IO.Put_Line (Item => "Test case 1: key = (others => 0), tweak = (others => 0), text = (others => 0)");
   Text := In1;
   Threefish.Encrypt (Key_Schedule => Ks, Text => Text);
   Put (Text => Text);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line (Item => "Test passes: " & Boolean'Image (Text = Out1) );
   Threefish.Decrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put (Item => "Decrypts to:");
   Put (Text => Text);
   Ada.Text_IO.New_Line;

   Threefish.Create_Key_Schedule (Key => Key2, Tweak =>Tweak2, Key_Schedule => Ks);
   Ada.Text_IO.Put_Line (Item => "Test case 2: key =");
   Put (Text => Key2);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line (Item => "tweak = " & Image (Tweak2 (0), Width => 16, Zero_Filled => True, Base => 16) & ' ' &
                                 Image (Tweak2 (1), Width => 16, Zero_Filled => True, Base => 16) );
   Ada.Text_IO.Put_Line (Item => "text =");
   Put (Text => In2);
   Ada.Text_IO.New_Line;
   Text := In2;
   Threefish.Encrypt (Key_Schedule => Ks, Text => Text);
   Put (Text => Text);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line (Item => "Test passes: " & Boolean'Image (Text = Out2) );
   Threefish.Decrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put (Item => "Decrypts to:");
   Put (Text => Text);
   Ada.Text_IO.New_Line;

   Threefish.Create_Key_Schedule (Key => Key3, Tweak =>Tweak3, Key_Schedule => Ks);
   Ada.Text_IO.Put_Line(Item => "Key is now (1, 2, 4, 8), tweak (16, 32)");
   Text := (others => 0);
   Threefish.Encrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put (Item => "Encryption of (others => 0):");
   Put (Text => Text);
   Ada.Text_IO.New_Line;
   Threefish.Decrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put (Item => "Decrypts to:");
   Put (Text => Text);
   Ada.Text_IO.New_Line;

   Text := (others => 16#4141414141414141#); -- All 'A's
   Threefish.Encrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put (Item => "Encryption of (others => 16#4141414141414141#):");
   Put (Text => Text);
   Ada.Text_IO.New_Line;
   Threefish.Decrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put (Item => "Decrypts to:");
   Put (Text => Text);
   Ada.Text_IO.New_Line;

   Text := Key3;
   Threefish.Encrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put (Item => "Encryption of (1, 2, 4, 8):");
   Put (Text => Text);
   Ada.Text_IO.New_Line;
   Threefish.Decrypt (Key_Schedule => Ks, Text => Text);
   Ada.Text_IO.Put (Item => "Decrypts to:");
   Put (Text => Text);
   Ada.Text_IO.New_Line;

   declare
      Ct : constant Threefish.Block_List := -- "ABCDEFGHIJKL"
         Threefish.Encrypt (Ks, (16#41#, 16#42#, 16#43#, 16#44#, 16#45#, 16#46#, 16#47#, 16#48#, 16#49#, 16#50#, 16#51#, 16#52#) );
      Pt : constant Threefish.Byte_List  := Threefish.Decrypt (Ks, Ct);
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
end Tf_Test;
