# Threefish
Ada Implementation of the Threefish-256 Encryption Algorithm

This is an Ada implementation of Threefixh-256 directly from the specification in Section 3.3 of the Skein paper V1.3 (https://www.schneier.com/skein.pdf).

Test cases specifically for Threefish are at https://sites.google.com/site/bartoszmalkowski/threefish and https://github.com/bmalkow/java-bc-threefish.

Threefish implements the encryption and decryption algorithms.

Tf_Test runs the test cases for Threefish-256 successfully (on a little-endian machine). It also encrypts certain blocks and presents the encrypted blocks, and shows that they decrypt correctly. Tf_Test uses PragmARC.Images from https://github.com/jrcarter/PragmARC.

TF_Crypt is a command-line program for encrypting and decrypting files. TF_Crypt uses Password_Line.

Password_Line is a function for obtaining secret input from a user without echoing it to the screen. It has only been tested on Linux.

Only Threefish is licensed under the BSD 3-Clause license. Tf_Test and TF_Crypt are GPL-3; Password_Line is GMGPL.
