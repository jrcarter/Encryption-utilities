-- Encryptuion/dectryption of files with the XOR cipher
-- Copyright (C) 2021 by PragmAda Software Engineering
-- SPDX-License-Identifier: GPL-3.0-only
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com

with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;
with Password_Line;
with PragmARC.Encryption.Simple_XOR;

procedure Xcrypt is
   use PragmARC.Encryption;

   procedure Usage; -- Displays usage instructions

   procedure Usage is
      -- Empty
   begin -- Usage
      Ada.Text_IO.Put_Line (Item => "usage: xcrypt <input file> [<output file>]");
      Ada.Text_IO.Put_Line (Item => "   en- and decrypts files with the XOR cipher");
      Ada.Text_IO.Put_Line (Item => "   prompts the user to enter a passphrase");
      Ada.Text_IO.Put_Line (Item => "   the output file, if not given, will have the same name as the input, with .xor appended");
   end Usage;

   use type Ada.Directories.File_Kind;
begin -- Xcrypt
   if Ada.Command_Line.Argument_Count = 0 then
      Usage;

      return;
   end if;

   if not Ada.Directories.Exists (Ada.Command_Line.Argument (1) ) or else
      Ada.Directories.Kind (Ada.Command_Line.Argument (1) ) /= Ada.Directories.Ordinary_File
   then
      Ada.Text_IO.Put_Line (Item => Ada.Command_Line.Argument (1) & " cannot be read");
      Usage;

      return;
   end if;

   Ada.Text_IO.Put_Line (Item => "Enter passphrase:");

   Get_Passphrase : declare
      Line   : constant String := Password_Line;
      Output : constant String :=
         (if Ada.Command_Line.Argument_Count > 1 then Ada.Command_Line.Argument (2) else Ada.Command_Line.Argument (1) & ".xor");
   begin -- Get_Passphrase
      Simple_XOR.Crypt (Input_Name => Ada.Command_Line.Argument (1), Output_Name => Output, Key => Simple_XOR.To_Bytes (Line) );
   end Get_Passphrase;
end Xcrypt;
