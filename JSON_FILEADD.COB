       Identification Division.
       Program-Id.     JSON_FILEADD.
       Date-Written.   08-DEC-2020.
      ******************************************************************
      *                     Module JSON_FILEADD                        *
      *                                                                *
      * Version:     1.0                                               *
      *                                                                *
      * Facility:    Interface Utility Testing                         *
      *                                                                *
      * Abstract:    This module adds an example JSON string to a      *
      *              sequential file for input to program JSON_PARSER. *
      *                                                                *
      * Environment: OpenVMS Alpha 8.4, COBOL, User Mode, Interactive  *
      *              and Batch                                         *
      *                                                                *
      * Notes:       Single record added to file contains two objects  *
      *                                                                *
      * Disclaimer:  Program is for educational use only. No liability *
      *              is assumed for usage of this program. None at all.*
      ******************************************************************
       Environment Division.
       Configuration Section.
       Source-Computer. AXP.
       Object-Computer. AXP.

       Input-Output Section.
       File-Control.

           Select Optional JSON-File Assign To 'JSON_FILE.DAT'
                  Organization Is Sequential
                  Access Mode  Is Sequential.

       Data Division.
      ******************************************************************
      * File Section                                                   *
      ******************************************************************
       File Section.

       FD  JSON-File
           Record Varying From 16 To 4096 Characters
           Depending On Ws-JSON-File-Reclen
           Data Record Is JSON-File-Record
           File Status Is Ws-JSON-File-Status.

       01  JSON-File-Record                      Pic X(4096).

      ******************************************************************
      * Working Storage Variables                                      *
      ******************************************************************
       Working-Storage Section.

       01  General-Variables.
           03 Ws-JSON-File-Status                Pic X(02).
           03 Ws-JSON-File-Reclen                Pic 9(4)  Comp.
           03 Ws-Status-Code                     Pic S9(9) Comp.

       01  Ws-Constants-And-Literals.
           03 Ws-JSON-Text.
              05 Filler                          Pic X(50) Value
                 '{"id":1,"method":"test.method123","params":[abcd]}'.
              05 Filler                          Pic X(50) Value
                 '{"fname":"Johnny","lname":"Rocket","age":"27","gen'.
              05 Filler                          Pic X(50) Value
                 'der":"male","amount":"1000","spouse":"Sally","chil'.
              05 Filler                          Pic X(50) Value
                 'd":"none yet","mother":"Martha","father":"George"}'.
           03 Ws-JSON-Text-Len               Pic 9(4)  Comp Value 200.

      ******************************************************************
      * Mainline Program                                               *
      ******************************************************************
       Procedure Division.
       Main-Routine.
       
           Initialize General-Variables
                      JSON-File-Record

           Display 'Program Start'
           Display ' '

           Open Output JSON-File

           Move Ws-JSON-Text-Len To Ws-JSON-File-Reclen
           Move Ws-JSON-Text     To JSON-File-Record
           Write JSON-File-Record

           Display 'Written to file: Len'
                   Ws-JSON-Text-Len With Conversion
                   ' Text: ' 
                   JSON-File-Record(1:Ws-JSON-File-Reclen)

           Close JSON-File

           Display ' '
           Display 'Program End'

           Stop Run
           .
