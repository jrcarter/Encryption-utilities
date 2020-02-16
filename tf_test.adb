-- Test of the Threefish cipher for blocks of 256 bits (Threefish-256)
-- Copyright (C) 2020 by PragmAda Software Engineering
-- Released under the terms of the GPL license version 3; see https://opensource.org/licenses

with Ada.Text_IO;
with Threefish;

procedure Tf_Test is
   Key   : constant Threefish.Block  := (0 => 1, 1 => 2, 2 => 4, 3 => 8);
   Tweak : constant Threefish.Couple := (0 => 16, 1 => 32);

   procedure Put (Text : in Threefish.Block); -- Outputs the images of the words ot Text on a single line

   procedure Put (Text : in Threefish.Block) is
      -- Empty
   begin -- Put
      All_Words : for I in Text'Range loop
         Ada.Text_IO.Put (Item => Threefish.Word'Image (Text (I) ) );
      end loop All_Words;
   end Put;

   Ks   : Threefish.Key_Schedule_Handle;
   Text : Threefish.Block := (others => 0);
begin -- Tf_Test
   Threefish.Create_Key_Schedule (Key => Key, Tweak =>Tweak, Key_Schedule => Ks);
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

   Text := Key;
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
         Ada.Text_IO.Put (Item => Threefish.Byte'Image (Pt (I) ) );
      end loop;
      Ada.Text_IO.New_Line;
   end;
end Tf_Test;
