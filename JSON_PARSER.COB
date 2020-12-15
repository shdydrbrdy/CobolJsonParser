       Identification Division.
       Program-ID.    JSON_PARSER.
       Date-Written.  14-DEC-2020.
      ******************************************************************
      *                     Module JSON_PARSER                         *
      *                                                                *
      * Version:     1.2                                               *
      *                                                                *
      * Facility:    Interface Utilities                               *
      *                                                                *
      * Abstract:    This module parses JSON strings to extract token  *
      *              keys and their values. It is a partial implemen-  *
      *              tation of the JSON standard as described below.   *
      *                                                                *
      * Environment: OpenVMS Alpha 8.4, COBOL, User Mode, Interactive  *
      *              and Batch                                         *
      *                                                                *
      * Definitions: Input text consists of one or more JSON objects:  *
      *              Object - Collection of one or more key/value      *
      *                       pairs enclosed in curly braces           *
      *              Key/Value Pair - Token key and token value pair   *
      *                       delimited by a colon. Multiple pairs     *
      *                       are separated by a comma.                *
      *              Value - A string enclosed by double-quotes, a     *
      *                      numeric value, a boolean word 'true' or   *
      *                      'false' and the word 'null'.              *
      *              Array - Ordered set of zero, one or more values   *
      *                      enclosed in square brackets. Each value   *
      *                      is separated by a comma. Arrays can be    *
      *                      single- or multi-dimensional.             *
      *                                                                *
      * Notes:       This program is useful for simple object parsing. *
      *              JSON source strings are read from a sequential    *
      *              file, token keys and values extracted and saved   *
      *              in a table for display. Program limitations:      *
      *                 Does not handle embedded objects               *
      *                 Does not parse arrays (entire array is saved   *
      *                    as a token key value; elements not parsed)  *
      *                 Does not validate JSON source syntax - assumes *
      *                    source string is correctly formatted        *
      *                 Does not handle control chars or escape seq's  *
      *                 Max 6 objects and 20 items per object          *
      *                 Max length of input string 4096 characters     *
      *                 Max length of token value 2048 characters      *
      *                                                                *
      * Disclaimer:  Program is for educational use only. No liability *
      *              is assumed for usage of this program. None at all.*
      ******************************************************************
       Environment Division.
       Configuration Section.
       Source-Computer. AXP.
       Object-Computer. AXP.

      ******************************************************************
      * Input and Output Section                                       *
      ******************************************************************
       Input-Output Section.
       File-Control.

           Select JSON-File Assign To 'JSON_FILE.DAT'
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
      * Working Storage Variables and Constants                        *
      ******************************************************************
       Working-Storage Section.

       01  Ws-Alphanumeric-Variables.
           03 Ws-Char-Work                       Pic X.
           03 Ws-Next-Char-Work                  Pic X.
           03 Ws-JSON-File-Status                Pic X(02).
           03 Ws-Time-Stamp                      Pic X(23).
           03 Ws-Temp-Key                        Pic X(20).
           03 Ws-Temp-Val                        Pic X(2048).

       01  Ws-Numeric-Variables.
           03 Ws-Counter-Display                 Pic 9(3).
           03 Ws-Counter-1                       Pic 9(4)  Comp.
           03 Ws-Counter-2                       Pic 9(4)  Comp.
           03 Ws-JSON-File-Reclen                Pic 9(4)  Comp.
           03 Ws-JSON-Text-Len                   Pic 9(4)  Comp.
           03 Ws-Char-Counter                    Pic 9(4)  Comp.
           03 Ws-Next-Char-Work-Posn             Pic 9(4)  Comp.
           03 Ws-Object-Counter                  Pic 9(4)  Comp.
           03 Ws-Item-Counter                    Pic 9(4)  Comp.
           03 Ws-Array-Level-Counter             Pic 9(4)  Comp.
           03 Ws-Object-Posn                     Pic 9(4)  Comp.
           03 Ws-Object-Len                      Pic 9(4)  Comp.
           03 Ws-Temp-Key-Len                    Pic 9(4)  Comp.
           03 Ws-Temp-Val-Len                    Pic 9(4)  Comp.
           03 Ws-Status-Code                     Pic S9(9) Comp.

       01  Ws-JSON-Text-Work                     Pic X(4096).

       01  Ws-JSON-Parsed-Objects.
           03 Ws-JSON-Num-Objects                Pic 9(02).
           03 Ws-JSON-Object-Table     Occurs 06 Times
                               Indexed By Ws-JSON-Object-Idx.
              05 Ws-JSON-Object-Num              Pic 9(02).
              05 Ws-JSON-Object-Items            Pic 9(02).
              05 Ws-JSON-Object-Len              Pic 9(04).
              05 Ws-JSON-Object-Text             Pic X(2048).
              05 Ws-JSON-Item-Table    Occurs 20 Times
                               Indexed By Ws-JSON-Item-Idx.
                 07 Ws-JSON-Key-Len                 Pic 9(04).
                 07 Ws-JSON-Key                     Pic X(10).
                 07 Ws-JSON-Val-Len                 Pic 9(04).
                 07 Ws-JSON-Val-Type                Pic X(04).
                 07 Ws-JSON-Val                     Pic X(2048).

       01  Ws-Switches-And-Indicators.
           03 Ws-JSON-File-EOF-Switch            Pic X.
              88 JSON-File-EOF         Value 'Y'.
           03 Ws-Processing-Type-Ind             Pic X.
              88 Processing-Array      Value 'A'.
              88 Processing-Key        Value 'K'.
              88 Processing-Value      Value 'V'.
              88 Processing-None       Value Space.
           03 Ws-Key-Read-Switch                 Pic X.
              88 Reading-A-Key         Value 'Y'.
              88 Not-Reading-A-Key     Value Space.
           03 Ws-Val-Read-Switch                 Pic X.
              88 Reading-A-Val         Value 'Y'.
              88 Not-Reading-A-Val     Value Space.
           03 Ws-Token-Ind                       Pic X.
              88 Token-Key             Value 'K'.
              88 Token-Val-Quoted      Value 'Q'.
              88 Token-Val-Unquoted    Value 'N'.

       01  Ws-Constants-And-Literals.
           03 Ws-Program-Name          Pic X(11) Value 'JSON_PARSER'.
           03 Ws-Null-Code             Pic X     Value X'00'.
           03 Ws-Double-Quote          Pic X     Value '"'.
           03 Ws-Colon                 Pic X     Value ':'.
           03 Ws-Comma                 Pic X     Value ','.
           03 Ws-Open-Brace            Pic X     Value '{'.
           03 Ws-Close-Brace           Pic X     Value '}'.
           03 Ws-Open-Bracket          Pic X     Value '['.
           03 Ws-Close-Bracket         Pic X     Value ']'.
           03 Ws-Val-Type-String       Pic X(04) Value 'STRG'.
           03 Ws-Val-Type-Numeric      Pic X(04) Value 'NUMB'.
           03 Ws-Val-Type-Boolean      Pic X(04) Value 'BOOL'.
           03 Ws-Val-Type-Null         Pic X(04) Value 'NULL'.
           03 Ws-Val-Type-Array        Pic X(04) Value 'ARRY'.
           03 Ws-Val-Type-Unknown      Pic X(04) Value 'UNKN'.
           03 Ws-JSON-Min-Len          Pic 9(4)  Comp Value 16.
           03 Ws-JSON-Max-Len          Pic 9(4)  Comp Value 4096.
           03 Ws-JSON-Max-Val-Len      Pic 9(4)  Comp Value 2048.
           03 Ws-JSON-Max-Items        Pic 9(4)  Comp Value 10.

      ******************************************************************
      *                      Main Program Routine                      *
      ******************************************************************
       Procedure Division.
       Main-Routine.

           Perform Initialization-Tasks

           Perform Until JSON-File-EOF
              If Ws-JSON-File-Reclen >= Ws-JSON-Min-Len
                 Move JSON-File-Record    To Ws-JSON-Text-Work
                 Move Ws-JSON-File-Reclen To Ws-JSON-Text-Len

                 Perform Parse-JSON-Text
              Else
                 Display 'Record is too short - bypassed: '
                         JSON-File-Record(1:Ws-JSON-File-Reclen)
                 Display ' '
              End-If

              Read JSON-File
                   At End Set JSON-File-EOF To True
              End-Read
           End-Perform

           Close JSON-File

           Perform Display-JSON-Parsed

           Perform Get-Time-Stamp

           Display Ws-Program-Name ' is ending at ' Ws-Time-Stamp

           Stop Run
           .

      ******************************************************************
      * Routine:  Initialization Tasks                                 *
      * Purpose:  Perform tasks for program initialization             *
      ******************************************************************
       Initialization-Tasks.

           Initialize Ws-Alphanumeric-Variables
                      Ws-Numeric-Variables
                      Ws-JSON-Text-Work
                      Ws-JSON-Parsed-Objects
                      Ws-Switches-And-Indicators

           Perform Get-Time-Stamp

           Display Ws-Program-Name ' is starting at ' Ws-Time-Stamp
           Display ' '

           Open Input JSON-File

           Read JSON-File 
                At End Set JSON-File-EOF To True
           End-Read
           .

      ******************************************************************
      * Routine:  Parse JSON Text                                      *
      * Purpose:  Begin by assuming a key token. Loop through the JSON *
      *           source text string, processing each character in     *
      *           order from beginning to end of the string.           *
      ******************************************************************
       Parse-JSON-Text.

           Set Token-Key To True

           Perform Varying Ws-Char-Counter From 1 By 1
                     Until Ws-Char-Counter > Ws-JSON-Text-Len

             Move Ws-JSON-Text-Work(Ws-Char-Counter:1)
               To Ws-Char-Work

             Compute Ws-Next-Char-Work-Posn = Ws-Char-Counter + 1

             If Ws-Next-Char-Work-Posn > Ws-JSON-Text-Len Then
                Move Ws-Null-Code To Ws-Next-Char-Work
             Else
                Move Ws-JSON-Text-Work(Ws-Next-Char-Work-Posn:1)
                  To Ws-Next-Char-Work
             End-If

             Evaluate Ws-Char-Work
                When Ws-Open-Brace
                     Perform Process-Open-Brace-Char
                When Ws-Close-Brace
                     Perform Process-Close-Brace-Char
                When Ws-Open-Bracket
                     Perform Process-Open-Bracket-Char
                When Ws-Close-Bracket
                     Perform Process-Close-Bracket-Char
                When Ws-Double-Quote
                     Perform Process-Double-Quote-Char
                When Ws-Colon
                     Perform Process-Colon-Char
                When Ws-Comma
                     Perform Process-Comma-Char
                When Other 
                     Perform Process-Other-Char
             End-Evaluate
           End-Perform
           .

      ******************************************************************
      * Routine:  Process Open Brace Char                              *
      * Purpose:  Open brace indicates the start of a new JSON object. *
      *           Prepare to process a token key as the first item.    *
      ******************************************************************
       Process-Open-Brace-Char.

           Set Processing-Key To True
           Set Token-Key      To True

           Add  1 To Ws-Object-Counter
                     Ws-JSON-Num-Objects

           Move Zero To Ws-Item-Counter
                        Ws-Array-Level-Counter
           Move 1    To Ws-Object-Len

           Move Ws-Char-Counter To Ws-Object-Posn

           Set Ws-JSON-Object-Idx To Ws-Object-Counter
           Set Ws-JSON-Item-Idx   To Ws-Item-Counter
           .

      ******************************************************************
      * Routine:  Process Close Brace Char                             *
      * Purpose:  Close brace indicates the end of a JSON Object.      *
      *           Save the most recent token value if needed; save     *
      *           the number of items processed within the object.     *
      ******************************************************************
       Process-Close-Brace-Char.

           If Processing-Value And Token-Val-Unquoted Then
              Perform Process-Save-Token-Value
           End-If

           Set Processing-None To True

           Add 1 To Ws-Object-Len

           Move Ws-Object-Counter
             To Ws-JSON-Object-Num(Ws-JSON-Object-Idx)
           Move Ws-Object-Len
             To Ws-JSON-Object-Len(Ws-JSON-Object-Idx)
           Move Ws-JSON-Text-Work(Ws-Object-Posn:Ws-Object-Len)
             To Ws-JSON-Object-Text(Ws-JSON-Object-Idx)
           Move Ws-Item-Counter
             To Ws-JSON-Object-Items(Ws-JSON-Object-Idx)
           .

      ******************************************************************
      * Routine:  Process Open Bracket Char                            *
      * Purpose:  Open bracket indicates the start of a JSON array.    *
      *           Level number enables handling of multi-dimensional   *
      *           arrays.                                              *
      ******************************************************************
       Process-Open-Bracket-Char.

           Perform Process-Other-Char

           Add 1 To Ws-Array-Level-Counter

           If Not Processing-Array
              Set Processing-Array To True
           End-If
           .

      ******************************************************************
      * Routine:  Process Close Bracket Char                           *
      * Purpose:  Close bracket indicates the end of a JSON array.     *
      ******************************************************************
       Process-Close-Bracket-Char.

           Perform Process-Other-Char

           Subtract 1 From Ws-Array-Level-Counter

           If Ws-Array-Level-Counter = Zero
              Set Processing-Value To True
           End-If
           .

      ******************************************************************
      * Routine:  Process Double Quote Char                            *
      * Purpose:  Double quote character encloses a token key and      *
      *           optionally a token value (non-primitive value).      *
      ******************************************************************
       Process-Double-Quote-Char.

           If Processing-Array
              Perform Process-Other-Char
           Else
              Add 1 To Ws-Object-Len

              If Token-Key Then
                 Perform Process-Token-Key
              Else
                 Perform Process-Token-Quoted-Value
              End-If
           End-If
           .

      ******************************************************************
      * Routine:  Process Token Key                                    *
      * Purpose:  If previously reading a value switch to reading a    *
      *           key; otherwise add the parsed key to the item table  *
      *           and prepare to work on a new token key.              *
      ******************************************************************
       Process-Token-Key.

           If Not-Reading-A-Key Then
              Set Reading-A-Key To True
           Else
              Set Not-Reading-A-Key To True
              Perform Process-Save-Token-Key
           End-If
           .

      ******************************************************************
      * Routine:  Process Save Token Key                               *
      * Purpose:  Save token key details to table. Increments object   *
      *           item counter and sets the current object item index  *
      *           (also used by the save token value routine).         *
      *           Temp key value variables are cleared for next use.   *
      ******************************************************************
       Process-Save-Token-Key.

           Add 1 To Ws-Item-Counter

           Set Ws-JSON-Item-Idx To Ws-Item-Counter

           Move Ws-Temp-Key
             To Ws-JSON-Key(Ws-JSON-Object-Idx, Ws-JSON-Item-Idx)
           Move Ws-Temp-Key-Len 
             To Ws-JSON-Key-Len(Ws-JSON-Object-Idx, Ws-JSON-Item-Idx)

           Move Spaces To Ws-Temp-Key   
           Move Zero   To Ws-Temp-Key-Len 
           .

      ******************************************************************
      * Routine:  Process Token Quoted Value                           *
      * Purpose:  If previously not reading a value prepare to work on *
      *           a new value; otherwise add the parsed value to the   *
      *           item table and prepare to work on a new token value. *
      ******************************************************************
       Process-Token-Quoted-Value.

           If Not-Reading-A-Val Then
              Set Reading-A-Val To True
           Else
              Set Not-Reading-A-Val To True
              Perform Process-Save-Token-Value
           End-If
           .

      ******************************************************************
      * Routine:  Process Save Token Value                             *
      * Purpose:  Save token value details to table. This routine sets *
      *           the type of value (string, numeric, boolean, etc.).  *
      *           Temp value variables are cleared for next use.       *
      ******************************************************************
       Process-Save-Token-Value.

           Move Ws-Temp-Val     
             To Ws-JSON-Val(Ws-JSON-Object-Idx, Ws-JSON-Item-Idx)
           Move Ws-Temp-Val-Len
             To Ws-JSON-Val-Len(Ws-JSON-Object-Idx, Ws-JSON-Item-Idx)

           If Ws-Temp-Val(1:Ws-Temp-Val-Len) Is Numeric
              Move Ws-Val-Type-Numeric 
                To Ws-JSON-Val-Type(Ws-JSON-Object-Idx, 
                                    Ws-JSON-Item-Idx)
           Else
              If Token-Val-Unquoted Then
                 Evaluate True 
                     When Ws-Temp-Val(1:1) = Ws-Open-Bracket
                          Move Ws-Val-Type-Array
                            To Ws-JSON-Val-Type(Ws-JSON-Object-Idx, 
                                                Ws-JSON-Item-Idx)
                     When Ws-Temp-Val(1:4) = 'true'
                          Move Ws-Val-Type-Boolean
                            To Ws-JSON-Val-Type(Ws-JSON-Object-Idx, 
                                                Ws-JSON-Item-Idx)
                     When Ws-Temp-Val(1:5) = 'false'
                          Move Ws-Val-Type-Boolean
                            To Ws-JSON-Val-Type(Ws-JSON-Object-Idx, 
                                                Ws-JSON-Item-Idx)
                     When Ws-Temp-Val(1:4) = 'null'
                          Move Ws-Val-Type-Null
                            To Ws-JSON-Val-Type(Ws-JSON-Object-Idx, 
                                                Ws-JSON-Item-Idx)
                     When Other
                          Move Ws-Val-Type-String
                            To Ws-JSON-Val-Type(Ws-JSON-Object-Idx, 
                                                Ws-JSON-Item-Idx)
                 End-Evaluate
              Else
                 Move Ws-Val-Type-String
                   To Ws-JSON-Val-Type(Ws-JSON-Object-Idx, 
                                       Ws-JSON-Item-Idx)
              End-If
           End-If

           Move Spaces To Ws-Temp-Val   
           Move Zero   To Ws-Temp-Val-Len 
           .

      ******************************************************************
      * Routine:  Process Colon Char                                   *
      * Purpose:  Colon character separates a key and its value. If    *
      *           processing a token key, switch to processing a token *
      *           value and vice-versa.                                *
      ******************************************************************
       Process-Colon-Char.

           If Processing-Array
              Perform Process-Other-Char
           Else
              Add 1 To Ws-Object-Len

              If Token-Key Then
                 If Ws-Next-Char-Work = Ws-Double-Quote Then
                    Set Token-Val-Quoted To True
                 Else
                    Set Token-Val-Unquoted To True
                 End-If
                 Set Processing-Value To True
              Else
                 Set Token-Key To True
                 Set Processing-Key To True
              End-If
           End-If
           .

      ******************************************************************
      * Routine:  Process Comma Char                                   *
      * Purpose:  Comma character separates token key and value group. *
      *           If an unquoted value was processed, save to table    *
      *           (save of quoted value was invoked by the closing     *
      *           double quote character).                             *
      *           Switch to processing a new key and increment the     *
      *           item table index.                                    *
      ******************************************************************
       Process-Comma-Char.

           If Processing-Array
              Perform Process-Other-Char
           Else
              Add 1 To Ws-Object-Len

              If Token-Val-Unquoted Then
                 Perform Process-Save-Token-Value
              End-If

              Set Token-Key To True
              Set Not-Reading-A-Key To True
              Set Processing-Key To True
           End-If
           .

      ******************************************************************
      * Routine:  Process Other Char                                   *
      * Purpose:  Add character to key or value work variables         *
      ******************************************************************
       Process-Other-Char.

           Add 1 To Ws-Object-Len

           If Processing-Key Then
              Add 1 To Ws-Temp-Key-Len
              String Ws-Temp-Key       Delimited By Spaces
                     Ws-Char-Work Delimited By Size
                Into Ws-Temp-Key
              End-String
           Else
              Add 1 To Ws-Temp-Val-Len
              String Ws-Temp-Val       Delimited By Spaces
                     Ws-Char-Work Delimited By Size
                Into Ws-Temp-Val
              End-String
           End-If
           .

      ******************************************************************
      * Routine:  Display JSON Parsed                                  *
      * Purpose:  Quick and dirty display of parsed items              *
      ******************************************************************
       Display-JSON-Parsed.

           Display 'JSON Parsed Object Count: ' Ws-JSON-Num-Objects 

           Perform Varying Ws-Counter-1 From 1 By 1
                     Until Ws-Counter-1 > Ws-JSON-Num-Objects

              Move Ws-JSON-Object-Text(Ws-Counter-1)
                To Ws-JSON-Text-Work
              Move Ws-JSON-Object-Len(Ws-Counter-1)
                To Ws-JSON-Text-Len

              Display ' '
              Display 'JSON Object Text.: ' 
                      Ws-JSON-Text-Work(1:Ws-JSON-Text-Len)
              Display 'JSON Object Num..: ' 
                      Ws-JSON-Object-Num(Ws-Counter-1)
              Display 'JSON Object Items: '
                      Ws-JSON-Object-Items(Ws-Counter-1)
              Display ' '

              Perform Varying Ws-Counter-2 From 1 By 1 Until
                      Ws-Counter-2 > Ws-JSON-Object-Items(Ws-Counter-1)

                 Move Ws-Counter-2 To Ws-Counter-Display

                 Display 'Item ' Ws-Counter-Display ' Key: Len '
                         Ws-JSON-Key-Len (Ws-Counter-1, Ws-Counter-2)
                         ' ' Ws-JSON-Key (Ws-Counter-1, Ws-Counter-2)
                         ' Value: Len '
                         Ws-JSON-Val-Len (Ws-Counter-1, Ws-Counter-2)
                         ' '
                         Ws-JSON-Val-Type (Ws-Counter-1, Ws-Counter-2)
                         ' ' With No Advancing
                         Move Ws-JSON-Val (Ws-Counter-1, Ws-Counter-2)
                           To Ws-Temp-Val
                         Move Ws-JSON-Val-Len (Ws-Counter-1, 
                                               Ws-Counter-2)
                           To Ws-Temp-Val-Len
                         Display Ws-Temp-Val(1:Ws-Temp-Val-Len)
              End-Perform
           End-Perform

           Display ' '
           .

      ******************************************************************
      * Routine:  Get System Time Stamp                                *
      * Purpose:  Obtain the current OpenVMS system date and time      *
      ******************************************************************
       Get-Time-Stamp.

           Call 'LIB$DATE_TIME' Using By Descriptor Ws-Time-Stamp
                                Giving Ws-Status-Code

           If Ws-Status-Code Is Failure
              Display 'Failed to obtain system date and time (Status)'
              Call 'LIB$SIGNAL' Using By Value Ws-Status-Code
           End-If
           .
      ******************************************************************
      *                 END MODULE JSON_PARSER                         *
      ******************************************************************