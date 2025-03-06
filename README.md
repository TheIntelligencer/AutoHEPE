# AutoHEPE

## Description
 Hard disk eraser bootexecute utility for Windows
 
## How to use
 This program cannot be launched in Win32 mode directly<br>
 In order to launch:<br>
 1. Copy this program to system32 folder<br> 
 2. Add to registry key<br>
 HKEY_LOCAL_MACHINE\\SYSTEM\\CurrentControlSet\\Session Manager<br>
 to BootExecute value this string:<br>
 autohepe \<block size in bytes\> \<path to hard disk in format ''\\Device\\PhysicalDriveX''\><br>
 3. Reboot machine<br>
 WARNING: You can destroy all data on your computer! Use extremely carefully!
 
## How to build
 You need at least Visual Studio 2010 in order to build this program
