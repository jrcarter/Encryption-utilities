-- Implementeation of the Threefish cipher for blocks of 256 bits (Threefish-256)
-- Copyright (C) 2020 by PragmAda Software Engineering
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses

with Interfaces;

package Threefish is
   subtype Word is Interfaces.Unsigned_64;

   type Word_List is array (Natural range <>) of Word;
   -- The Threefixh specification uses zeor-based indexing

   Num_Words : constant := 4; -- A block is Num_Words words

   subtype Block  is Word_List (0 .. Num_Words - 1);
   subtype Couple is Word_List (0 .. 1);

   type Key_Schedule_Handle is limited private;

   procedure Create_Key_Schedule (Key : in Block; Tweak : in Couple; Key_Schedule : out Key_Schedule_Handle);
   -- Creates Key_Schedule from Key and Tweak

   procedure Encrypt (Key_Schedule : in Key_Schedule_Handle; Text : in out Block);
   -- Encrypts Text using Key_Schedule, which should have been created using Create_Key_Schedule

   procedure Decrypt (Key_Schedule : in Key_Schedule_Handle; Text : in out Block);
   -- Decrypts Text, which should have been encrypted with Encrypt and Key_Schedule

   subtype Byte is Interfaces.Unsigned_8;

   type Byte_List  is array (Positive range <>) of Byte;
   type Block_List is array (Positive range <>) of Block;

   function Encrypt (Key_Schedule : Key_Schedule_Handle; Text : Byte_List) return Block_List;
   -- Pads Text to a multiple of 32 bytes with zeros, then converts 32-byte slices using Key_Schedule

   function Decrypt (Key_Schedule : Key_Schedule_Handle; Text : Block_List) return Byte_List;
   -- Decrypts the blocks of Text and converts the results to a Byte_List
   -- Results includes any padding added by Encrypt

   -- En- & Decrypt for Byte_Lists use unchecked conversion to convert between blocks of 32 Bytes and Block,
   -- so en- & decryption need to be done on machines with the same endianness
   -- Threefish mandates little-endian conversions between 8-Byte blocks and Word; an endian-aware conversion is needed to be
   -- compliant and portable
private -- Threefish
   Num_Rounds : constant := 72;

   type Key_List is array (0 .. Num_Rounds / 4) of Block;

   type Key_Schedule_Handle is record
      Key    : Word_List (0 .. Num_Words);
      Tweak  : Word_List (0 .. 2);
      Subkey : Key_List;
   end record;
end Threefish;
