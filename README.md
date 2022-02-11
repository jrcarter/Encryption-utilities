# Encryption utilities

The PragmAda Reusable Components (https://github.com/jrcarter/PragmARC) contain implementations of the Threefish-256 (implemented directly from the specification in Section 3.3 of the Skein paper V1.3 [https://www.schneier.com/skein.pdf]), Threefish-512, Threefish-1024, and XOR encryption algorithms. Threefish-512 and Threefish-1024 are provided by Daniel Norte de Moraes (https://github.com/danieagle).

Test cases specifically for Threefish are at https://sites.google.com/site/bartoszmalkowski/threefish and https://github.com/bmalkow/java-bc-threefish.

Tf_Test runs the test cases for Threefish-256 successfully (on a little-endian machine). It also encrypts certain blocks and presents the encrypted blocks, and shows that they decrypt correctly. Tf_Test uses PragmARC.Images from https://github.com/jrcarter/PragmARC. Tf_Test_512 and Tf_Test_1024, provided by Daniel Norte de Moraes (https://github.com/danieagle), do the same for Threefish-512 and Threefish-1024.

TF_Crypt is a command-line program for encrypting and decrypting files with Threefish-256. TF_Crypt uses Password_Line. Files encrypted with TF_Crypt built with GNAT and running on Linux are correctly decrypted by TF_Crypt build with ObjectAda and running on Windows, and vice versa.

Xcrypt is a command-line program for encrypting and decrypting files with XOR. It uses Password_Line. Since XOR is symmetrical, the same algorithm is used for encrypting and decrypting. One can encrypt twice with multiple keys, and decrypt with the keys in any order. This gives results equivalent to encrypting with a key length of the least common multiple of the lengths of the keys.

Password_Line is a function for obtaining secret input from a user without echoing it to the screen. It has been tested and works with GNAT on Linux and Windows and ObjectAda 10.3 on Windows.

Tf_Test*, TF_Crypt, and Xcrypt are GPL-3; Password_Line is GMGPL.
