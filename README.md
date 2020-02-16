# Threefish
Ada Implementation of the Threefish-256 Encryption Algorithm

This is an Ada implementation of Threefixh-256 directly from the specification in Section 3.3 of the Skein paper V1.3 (https://www.schneier.com/skein.pdf).

There do not appear to be any test cases specifically for Threefish-256, so I am not sure that the implementation is correct, though I think it is. I would appreciate additional sets of eyes inspecting this for correctness. If you know of any test cases for Threefish-256, I would appreciate learning of them. Until I receive independent verification of correctness, I consider this a beta implementation.

This version uses unchecked conversion to convert between arrays of bytes (Unsigned_8) and words (Unsigned_64) and arrays of words, so it is only correct on little-endian platforms. Endian-aware conversion functions are needed to make this correct on all platforms.

Threefish implements the encryption and decryption algorithms.

Tf_Test is not really a test, but an experiment. It encrypts certain blocks and presents the encrypted blocks, and shows that they decrypt correctly.

TF_Crypt is a command-line program for encrypting and decrypting files. It also makes use of unchecked conversion and needs modification to work correctly on big-endian platforms. TF_Crypt uses Password_Line.

Password_Line is a function for obtaining secret input from a user without echoing it to the screen. It has only been tested on Linus.

Only Threefish is licensed under the BSD 3-Clause license. Tf_Test and TF_Crypt are GPL-3; Password_Line is GMGPL.
