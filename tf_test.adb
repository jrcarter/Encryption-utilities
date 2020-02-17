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
   Out1   : constant Threefish.Block  := (0 => 16#94EEEA8B1F2ADA84#, 1 => 16#ADF103313EAE6670#,
                                          2 => 16#952419A1F4B16D53#, 3 => 16#D83F13E63C9F6B11#);
   Key2   : constant Threefish.Block  := (0 => 16#1716151413121110#, 1 => 16#1f1e1d1c1b1a1918#,
                                          2 => 16#2726252423222120#, 3 => 16#2f2e2d2c2b2a2928#);
   Tweak2 : constant Threefish.Couple := (0 => 16#0706050403020100#, 1 => 16#0f0e0d0c0b0a0908#);
   In2    : constant Threefish.Block  := (0 => 16#f8f9fafbfcfdfeff#, 1 => 16#f0f1f2f3f4f5f6f7#,
                                          2 => 16#e8e9eaebecedeeef#, 3 => 16#e0e1e2e3e4e5e6e7#);
   Out2   : constant Threefish.Block  := (0 => 16#df8fea0eff91d0e0#, 1 => 16#d50ad82ee69281c9#,
                                          2 => 16#76f48d58085d869d#, 3 => 16#df975e95b5567065#);
   Key3   : constant Threefish.Block  := (0 => 1, 1 => 2, 2 => 4, 3 => 8);
   Tweak3 : constant Threefish.Couple := (0 => 16, 1 => 32);

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

   Text := (others => 16#4141414141414141#); -- All As
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
      Ct : constant Threefish.Block_List := Threefish.Encrypt (Ks, (1 .. 31 => 16#41#) );
      Pt : constant Threefish.Byte_List  := Threefish.Decrypt (Ks, Ct);
   begin
      Ada.Text_IO.Put (Item => "Encryption of (1 .. 31 => 16#41#):");
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
