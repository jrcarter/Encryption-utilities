-- Encryptuion/dectryption of files with the Threefish cipher for blocks of 256 bits (Threefish-256)
-- Copyright (C) 2020 by PragmAda Software Engineering
-- Released under the terms of the GPL license version 3; see https://opensource.org/licenses

with Ada.Command_Line;
with Ada.Directories;
with Ada.Sequential_IO;
with Ada.Text_IO;
with Password_Line;
with Ada.Unchecked_Conversion;
with Threefish;

procedure TF_Crypt is
   package Byte_IO is new Ada.Sequential_IO (Element_Type => Threefish.Byte);

   Bytes_Per_Block  : constant := 32;
   Bytes_Per_Couple : constant := 16;

   subtype Block_As_Bytes is Threefish.Block_As_Bytes;

   subtype Block_As_String  is String (1 .. Bytes_Per_Block);
   subtype Couple_As_String is String (1 .. Bytes_Per_Couple);

   function To_Block  (Source : Block_As_String)  return Threefish.Block;
   function To_Couple (Source : Couple_As_String) return Threefish.Couple;

   procedure Usage; -- Displays usage instructions

   procedure Encrypt (KS: in Threefish.Key_Schedule_Handle; Name : in String); -- Encrypts Name using KS
   procedure Decrypt (KS: in Threefish.Key_Schedule_Handle; Name : in String); -- Decrypts Name using KS

   function To_Block  (Source : Block_As_String)  return Threefish.Block is
      function To_List is new Ada.Unchecked_Conversion (Source => Block_As_String, Target => Block_As_Bytes);

      List : constant Block_As_Bytes := To_List (Source);
   begin -- To_Block
      return Threefish.Block_From_Bytes (List);
   end To_Block;

   function To_Couple (Source : Couple_As_String) return Threefish.Couple is
      subtype Couple_As_Bytes is Threefish.Byte_List (1 .. Source'Length);

      function To_List is new Ada.Unchecked_Conversion (Source => Couple_As_String, Target => Couple_As_Bytes);

      List : constant Couple_As_Bytes := To_List (Source);

      Result : Threefish.Couple;
   begin -- To_Couple
      Result (Result'First) := Threefish.Word_From_Bytes (List (List'First .. List'First + Threefish.Word_As_Bytes'Length - 1) );
      Result (Result'Last)  := Threefish.Word_From_Bytes (List (List'First + Threefish.Word_As_Bytes'Length .. List'Last) );

      return Result;
   end To_Couple;

   procedure Usage is
      -- Empty
   begin -- Usage
      Ada.Text_IO.Put_Line (Item => "usage: tf_crypt [-d] <file name>");
      Ada.Text_IO.Put_Line (Item => "   prompts the user to enter a passphrase");
      Ada.Text_IO.Put_Line (Item => "   if -d decrypts <file name>, else encrypts <file name>");
      Ada.Text_IO.Put_Line (Item => "   the passphrase should be 48 characters, but it will be truncated");
      Ada.Text_IO.Put_Line (Item => "      or padded with X to achieve that length");
      Ada.Text_IO.Put_Line (Item => "   the output file when encrypting, will have the same name as the input, with .tfe appended");
      Ada.Text_IO.Put_Line (Item => "   when decrypting,");
      Ada.Text_IO.Put_Line (Item => "      if <file name> ends with .tfe, the output file will be the input with .tfe removed");
      Ada.Text_IO.Put_Line (Item => "      else the output file will be the same name as the input, with .tfd appended");
   end Usage;

   procedure Encrypt (KS: in Threefish.Key_Schedule_Handle; Name : in String) is
      Input      : Byte_IO.File_Type;
      Output     : Byte_IO.File_Type;
      Length     : Threefish.Word_As_Bytes;
      Byte_Block : Block_As_Bytes;
      Word_Block : Threefish.Block;
   begin -- Encrypt
      Byte_IO.Open (File => Input, Mode => Byte_IO.In_File, Name => Name);
      Byte_IO.Create (File => Output, Name => Name & ".tfe");
      Length := Threefish.Bytes_From_Word (Threefish.Word (Ada.Directories.Size (Name) ) );

      Write_Length : for I in Length'Range loop
         Byte_IO.Write (File => Output, Item => Length (I) );
      end loop Write_Length;

      All_Blocks : loop
         exit All_Blocks when Byte_IO.End_Of_File (Input);

         Byte_Block := (others => 0);

         One_Block : for I in Byte_Block'Range loop
            exit One_Block when Byte_IO.End_Of_File (Input);

            Byte_IO.Read (File => Input, Item => Byte_Block (I) );
         end loop One_Block;

         Word_Block := Threefish.Block_From_Bytes (Byte_Block);
         Threefish.Encrypt (Key_Schedule => KS, Text => Word_Block);
         Byte_Block := Threefish.Bytes_From_Block (Word_Block);

         Write_Block : for I in Byte_Block'Range loop
            Byte_IO.Write (File => Output, Item => Byte_Block (I) );
         end loop Write_Block;
      end loop All_Blocks;

      Byte_IO.Close (File => Input);
      Byte_IO.Close (File => Output);
   end Encrypt;

   procedure Decrypt (KS: in Threefish.Key_Schedule_Handle; Name : in String) is
      Input      : Byte_IO.File_Type;
      Output     : Byte_IO.File_Type;
      Len_Bytes  : Threefish.Word_As_Bytes;
      Length     : Threefish.Word;
      Count      : Threefish.Word := 0;
      Byte_Block : Block_As_Bytes;
      Word_Block : Threefish.Block;

      use type Threefish.Word;
   begin -- Decrypt
      Byte_IO.Open (File => Input, Mode => Byte_IO.In_File, Name => Name);

      if Name'Length > 4 and then Name (Name'Last - 3 .. Name'Last) = ".tfe" then
         Byte_IO.Create (File => Output, Name => Name (Name'First .. Name'Last - 4) );
      else
         Byte_IO.Create (File => Output, Name => Name & ".tfd");
      end if;

      Read_Length : for I in Len_Bytes'Range loop
         Byte_IO.Read (File => Input, Item => Len_Bytes (I) );
      end loop Read_Length;

      Length := Threefish.Word_From_Bytes (Len_Bytes);

      All_Blocks : loop
         exit All_Blocks when Byte_IO.End_Of_File (Input);

         Read_Block : for I in Byte_Block'Range loop
            Byte_IO.Read (File => Input, Item => Byte_Block (I) );
         end loop Read_Block;

         Word_Block := Threefish.Block_From_Bytes (Byte_Block);
         Threefish.Decrypt (Key_Schedule => KS, Text => Word_Block);
         Byte_Block := Threefish.Bytes_From_Block (Word_Block);

         Write_Bytes : for I in Byte_Block'Range loop
            exit Write_Bytes when Count >= Length;

            Byte_IO.Write (File => Output, Item => Byte_Block (I) );
            Count := Count + 1;
         end loop Write_Bytes;
      end loop All_Blocks;

      Byte_IO.Close (File => Input);
      Byte_IO.Close (File => Output);
   end Decrypt;

   Encrypting : Boolean := True;
   Name_Arg   : Positive := 1;
   Key        : Threefish.Block;
   Tweak      : Threefish.Couple;
   KS         : Threefish.Key_Schedule_Handle;

   use type Ada.Directories.File_Kind;
begin -- TF_Crypt
   if Ada.Command_Line.Argument_Count = 0 then
      Usage;

      return;
   end if;

   if Ada.Command_Line.Argument_Count >= 2 and then Ada.Command_Line.Argument (1) = "-d" then
      Encrypting := False;
      Name_Arg := 2;
   end if;

   if not Ada.Directories.Exists (Ada.Command_Line.Argument (Name_Arg) ) or else
      Ada.Directories.Kind (Ada.Command_Line.Argument (Name_Arg) ) /= Ada.Directories.Ordinary_File
   then
      Ada.Text_IO.Put_Line (Item => Ada.Command_Line.Argument (Name_Arg) & " cannot be read");
      Usage;

      return;
   end if;

   Ada.Text_IO.Put_Line (Item => "Enter passphrase:");

   Get_Passphrase : declare
      Line : constant String := Password_Line;
      Pass : constant String := Line & (Line'Last + 1 .. Line'First + Bytes_Per_Block + Bytes_Per_Couple - 1 => 'X');
   begin -- Get_Passphrase
      Key := To_Block (Pass (Pass'First .. Pass'First + Bytes_Per_Block - 1) );
      Tweak := To_Couple (Pass (Pass'First + Bytes_Per_Block .. Pass'First + Bytes_Per_Block + Bytes_Per_Couple - 1) );
   end Get_Passphrase;

   Threefish.Create_Key_Schedule (Key => Key, Tweak => Tweak, Key_Schedule => KS);

   if Encrypting then
      Encrypt (KS => KS, Name => Ada.Command_Line.Argument (Name_Arg) );
   else
      Decrypt (KS => KS, Name => Ada.Command_Line.Argument (Name_Arg) );
   end if;
end TF_Crypt;
