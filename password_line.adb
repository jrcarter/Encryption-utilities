-- Non-echoed input for obtaining passwords
-- Works with GNAT/Linux and Windows and ObjectAda/Windows
-- Copyright (C) 2022 by PragmAda Software Engineering
--
-- History:
-- 2022 Feb 15     J. Carter          V1.3--OA 10.3 does not need a workaround
-- 2021 May 15     J. Carter          V1.2--Mention OA workaround
-- 2021 Feb 01     J. Carter          V1.1--Improve backspace handling
-- 2017 Feb 01     J. Carter          V1.0--Initial release
--
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

function Password_Line (Echo : Boolean := True) return String is
   LF  : Character renames Ada.Characters.Latin_1.LF;
   CR  : Character renames Ada.Characters.Latin_1.CR;
   DEL : Character renames Ada.Characters.Latin_1.DEL;
   BS  : Character renames Ada.Characters.Latin_1.BS;

   use Ada.Strings.Unbounded;

   Result : Unbounded_String;
   Ch     : Character;
begin -- Password_Line
   All_Characters : loop
      Ada.Text_IO.Get_Immediate (Item => Ch);

      case Ch is
      when LF | CR =>
         Ada.Text_IO.New_Line;

         return To_String (Result);
      when DEL | BS =>
         Delete (Source => Result, From => Length (Result), Through => Length (Result) );

         if Echo then
            Ada.Text_IO.Put (Item => BS & ' ' & BS);
         end if;
      when others =>
         Append (Source => Result, New_Item => Ch);

         if Echo then
            Ada.Text_IO.Put (Item => '*');
         end if;
      end case;
   end loop All_Characters;
end Password_Line;
--
-- SPDX-License-Identifier: GPL-2.0-or-later WITH GNAT-exception
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
