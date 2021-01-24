-- Non-echoed input for obtaining passwords
-- Works with GNAT/Linux and Windows; does not work with ObjectAda/Windows
-- Copyright (C) 2021 by PragmAda Software Engineering
--
-- History:
-- 2021 Feb 01     J. Carter          V1.1--Improve backspace handling
-- 2017 Feb 01     J. Carter          V1.0--Initial release
--
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

function Password_Line (Echo : Boolean := True) return String is
   LF  : Character renames Ada.Characters.Latin_1.LF;
   EOT : Character renames Ada.Characters.Latin_1.EOT;
   DEL : Character renames Ada.Characters.Latin_1.DEL;
   BS  : Character renames Ada.Characters.Latin_1.BS;

   use Ada.Strings.Unbounded;

   Result : Unbounded_String;
   Ch     : Character;
begin -- Password_Line
   All_Characters : loop
      Ada.Text_IO.Get_Immediate (Item => Ch);

      case Ch is
      when LF | EOT =>
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
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
