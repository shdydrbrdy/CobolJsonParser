       Identification Division.
       Program-Id.     JSON_ARRAY_PARSER.
       Date-Written.   15-DEC-2020.
      ******************************************************************
      *                     Module JSON_ARRAY_PARSER                   *
      *                                                                *
      * Version:     1.0                                               *
      *                                                                *
      * Facility:    Interface Utilities                               *
      *                                                                *
      * Abstract:    This module parses a JSON array to extract one    *
      *              or more values. It is a partial implementation    *
      *              of the JSON standard as described below.          *
      *                                                                *
      * Environment: OpenVMS Alpha 8.4, COBOL, User Mode, Interactive  *
      *              and Batch                                         *
      *                                                                *
      * Definitions: Input text consists of a JSON array. This is an   *
      *              ordered set of zero, one or more values enclosed  *
      *              in square brackets. Each value and group of       *
      *              values is separated by a comma. Arrays can be     *
      *              single- or multi-dimensional.                     *
      *                                                                *
      * Notes:       This program is useful for parsing arrays of one  *
      *              or two dimensions into a table of row/col values, *
      *              grouped by open and close brackets. Examples:     *
      *                 [] - Empty array                               *
      *                 ["abcdefg"] - Single value array type String   *
      *                 [65536,"abc",true] - Triple value array type   *
      *                     Boolean, Numeric and String                *
      *                 [[["name","Rudy"],[true,12345,"z"]],"abc",4321]*
      *                     - Two value groups followed by two single  *
      *                     values. This will be parsed as:            *
      *                     Row 1 Col 1 value = name                   *
      *                     Row 1 Col 2 value = Rudy                   *
      *                     Row 2 Col 1 value = true                   *
      *                     Row 2 Col 2 value = 12345                  *
      *                     Row 2 Col 3 value = z                      *
      *                     Row 3 Col 1 value = abc                    *
      *                     Row 4 Col 1 value = 4321                   *
      *                 Max 24 items per arrau                         *
      *                 Max length of input string 2048 characters     *
      *                 Max length of array value 256 characters       *
      *                                                                *
      * Disclaimer:  Program is for educational use only. No liability *
      *              is assumed for usage of this program.             *
      ******************************************************************
       Environment Division.
       Configuration Section.
       Source-Computer. AXP.
       Object-Computer. AXP.

       Data Division.
      ******************************************************************
      * Working Storage Variables                                      *
      ******************************************************************
       Working-Storage Section.

       01  Ws-Alphanumeric-Variables.
           03 Ws-Char-Work                       Pic X.
           03 Ws-Next-Char-Work                  Pic X.
           03 Ws-Temp-Val                        Pic X(256).

       01  Ws-Numeric-Variables.
           03 Ws-Num-Display                     Pic 9(04).
           03 Ws-Counter                         Pic 9(04)  Comp.
           03 Ws-Row-Counter                     Pic 9(04)  Comp.
           03 Ws-Col-Counter                     Pic 9(04)  Comp.
           03 Ws-Char-Counter                    Pic 9(04)  Comp.
           03 Ws-Item-Counter                    Pic 9(04)  Comp.
           03 Ws-Next-Char-Work-Posn             Pic 9(04)  Comp.
           03 Ws-Array-Work-Len                  Pic 9(04)  Comp.
           03 Ws-Temp-Val-Len                    Pic 9(04)  Comp.

       01  Ws-Array-Work                         Pic X(2048).

       01  Ws-JSON-Parsed-Array.
           03 Ws-JSON-Num-Items                  Pic 9(04).
           03 Ws-JSON-Item-Table       Occurs 24 Times
                                       Indexed By Ws-JSON-Item-Idx.
              05 Ws-JSON-Row-Num                 Pic 9(04).
              05 Ws-JSON-Col-Num                 Pic 9(04).
              05 Ws-JSON-Val-Len                 Pic 9(04).
              05 Ws-JSON-Val-Type                Pic X(04).
              05 Ws-JSON-Val                     Pic X(256).

       01  Ws-Switches-And-Indicators.
           03 Ws-Processing-Value-Switch         Pic X.
              88 Processing-Value      Value 'Y'.
              88 Not-Processing-Value  Value Space.
           03 Ws-Val-Read-Switch                 Pic X.
              88 Reading-A-Val         Value 'Y'.
              88 Not-Reading-A-Val     Value Space.
           03 Ws-Array-Val-Ind                   Pic X.
              88 Array-Val-Quoted      Value 'Q'.
              88 Array-Val-Unquoted    Value 'N'.
           03 Ws-Group-Val-Switch                Pic X.
              88 Group-Val             Value 'Y'.
              88 No-Group-Val          Value Space.

       01  Ws-Test-Arrays.
           03 Ws-Array-1.
              05 Filler                          Pic X(02) Value '[]'.
           03 Ws-Array-2.
              05 Filler                          Pic X(11) Value
                 '["abcdefg"]'.
           03 Ws-Array-3.
              05 Filler                          Pic X(18) Value
                 '[65536,"abc",true]'.
           03 Ws-Array-4.
              05 Filler                          Pic X(47) Value
                 '[[["name","Rudy"],[true,12345,"z"]],"abc",4321]'.
           03 Ws-Array-1-Len               Pic 9(4)  Comp Value 0002.
           03 Ws-Array-2-Len               Pic 9(4)  Comp Value 0011.
           03 Ws-Array-3-Len               Pic 9(4)  Comp Value 0018.
           03 Ws-Array-4-Len               Pic 9(4)  Comp Value 0047.

       01  Ws-Constants-And-Literals.
           03 Ws-Null-Code             Pic X     Value X'00'.
           03 Ws-Double-Quote          Pic X     Value '"'.
           03 Ws-Comma                 Pic X     Value ','.
           03 Ws-Open-Bracket          Pic X     Value '['.
           03 Ws-Close-Bracket         Pic X     Value ']'.
           03 Ws-Val-Type-String       Pic X(04) Value 'STRG'.
           03 Ws-Val-Type-Numeric      Pic X(04) Value 'NUMB'.
           03 Ws-Val-Type-Boolean      Pic X(04) Value 'BOOL'.
           03 Ws-Val-Type-Null         Pic X(04) Value 'NULL'.
           03 Ws-Val-Type-Unknown      Pic X(04) Value 'UNKN'.
           03 Ws-Val-Type-Empty        Pic X(04) Value 'EMTY'.

      ******************************************************************
      * Mainline Program                                               *
      ******************************************************************
       Procedure Division.
       Main-Routine.

           Initialize Ws-Alphanumeric-Variables
                      Ws-Numeric-Variables
                      Ws-Array-Work
                      Ws-JSON-Parsed-Array
                      Ws-Switches-And-Indicators

           Display 'Program Start'
           Display ' '

           Move Ws-Array-1     To Ws-Array-Work
           Move Ws-Array-1-Len To Ws-Array-Work-Len

           Perform Parse-Array
           Perform Display-Array-Items

           Display ' '
           Display 'Program End'

           Stop Run
           .

      ******************************************************************
      * Parse Array                                                    *
      ******************************************************************
       Parse-Array.

           Move    1 To Ws-Row-Counter
           Move Zero To Ws-Col-Counter

           Perform Varying Ws-Char-Counter From 1 By 1
                     Until Ws-Char-Counter > Ws-Array-Work-Len

             Move Ws-Array-Work(Ws-Char-Counter:1)
               To Ws-Char-Work

             Compute Ws-Next-Char-Work-Posn = Ws-Char-Counter + 1

             If Ws-Next-Char-Work-Posn > Ws-Array-Work-Len Then
                Move Ws-Null-Code To Ws-Next-Char-Work
             Else
                Move Ws-Array-Work(Ws-Next-Char-Work-Posn:1)
                  To Ws-Next-Char-Work
             End-If

             Evaluate Ws-Char-Work
                 When Ws-Open-Bracket
                      Perform Process-Open-Bracket-Char
                 When Ws-Close-Bracket
                      Perform Process-Close-Bracket-Char
                 When Ws-Double-Quote
                      Perform Process-Double-Quote-Char
                 When Ws-Comma
                      Perform Process-Comma-Char
                 When Other 
                      Perform Process-Other-Char
             End-Evaluate
           End-Perform

           Move Ws-Item-Counter To Ws-JSON-Num-Items
           .

      ******************************************************************
      * Routine:  Process Open Bracket Char                            *
      * Purpose:  Open bracket indicates start of a new array group.   *
      *           Sets group flag for open brackets other than the     *
      *           first open bracket (where char counter = 1).         *
      ******************************************************************
       Process-Open-Bracket-Char.

           If Ws-Char-Counter > 1 Then
              Set Group-Val To True
           End-If

           If Ws-Next-Char-Work Not = Ws-Open-Bracket Then
              Set Processing-Value To True

              If Ws-Next-Char-Work = Ws-Double-Quote Then
                 Set Array-Val-Quoted To True
              Else
                 Set Array-Val-Unquoted To True
              End-If
           End-If
           .

      ******************************************************************
      * Routine:  Process Close Bracket Char                           *
      * Purpose:  Clears group flag and triggers save of an array      *
      *           unquoted value                                       *
      ******************************************************************
       Process-Close-Bracket-Char.

           If Processing-Value And Array-Val-Unquoted Then
              Perform Process-Save-Array-Value
              Set Not-Processing-Value To True
           Else
              Set Not-Processing-Value To True
           End-If

           Set No-Group-Val To True
           .

      ******************************************************************
      * Routine:  Process Comma Char                                   *
      * Purpose:  Comma character separates multiple array groups and  *
      *           multiple items within an array group.                *
      ******************************************************************
       Process-Comma-Char.

           If Processing-Value And Array-Val-Unquoted Then
              Perform Process-Save-Array-Value
              Set Not-Processing-Value To True
           End-If

           Evaluate Ws-Next-Char-Work
               When Ws-Double-Quote 
                    Continue
               When Ws-Open-Bracket 
                    Continue
               When Ws-Null-Code
                    Continue
               When Other
                    Set Processing-Value   To True
                    Set Array-Val-Unquoted To True
           End-Evaluate

           If No-Group-Val Then
              Add 1     To Ws-Row-Counter
              Move Zero To Ws-Col-Counter
           End-If
           .

      ******************************************************************
      * Routine:  Process Double Quote Char                            *
      * Purpose:  Double quote character encloses a non-primitive value*
      ******************************************************************
       Process-Double-Quote-Char.

           Perform Process-Quoted-Value
           .

      ******************************************************************
      * Routine:  Process Quoted Value                                 *
      * Purpose:  If previously not reading a value prepare to work on *
      *           a new value; otherwise add the parsed value to the   *
      *           table and prepare to work on a new value.            *
      ******************************************************************
       Process-Quoted-Value.

           If Not-Reading-A-Val Then
              Set Reading-A-Val To True
           Else
              Perform Process-Save-Array-Value

              Set Not-Reading-A-Val    To True
              Set Not-Processing-Value To True
           End-If
           .

      ******************************************************************
      * Routine:  Process Other Char                                   *
      * Purpose:  Add character to value work variables                *
      ******************************************************************
       Process-Other-Char.

           Add 1 To Ws-Temp-Val-Len

           String Ws-Temp-Val  Delimited By Spaces
                  Ws-Char-Work Delimited By Size
             Into Ws-Temp-Val
           End-String
           .

      ******************************************************************
      * Routine:  Process Save Array Value                             *
      * Purpose:  Save array value details to table. This routine sets *
      *           the type of value (string, numeric, boolean, etc.).  *
      *           Temp value variables are cleared for next use.       *
      ******************************************************************
       Process-Save-Array-Value.

           Add 1 To Ws-Item-Counter
                    Ws-Col-Counter

           Set Ws-JSON-Item-Idx To Ws-Item-Counter

           Move Ws-Row-Counter   To Ws-JSON-Row-Num(Ws-JSON-Item-Idx)
           Move Ws-Col-Counter   To Ws-JSON-Col-Num(Ws-JSON-Item-Idx)
           Move Ws-Temp-Val      To Ws-JSON-Val(Ws-JSON-Item-Idx)
           Move Ws-Temp-Val-Len  To Ws-JSON-Val-Len(Ws-JSON-Item-Idx)

           If Ws-Temp-Val-Len = Zero Then
              Move Ws-Val-Type-Empty 
                To Ws-JSON-Val-Type(Ws-JSON-Item-Idx)
           Else
              If Ws-Temp-Val(1:Ws-Temp-Val-Len) Is Numeric
                 Move Ws-Val-Type-Numeric 
                   To Ws-JSON-Val-Type(Ws-JSON-Item-Idx)
              Else
                 If Array-Val-Unquoted Then
                    Evaluate True 
                        When Ws-Temp-Val(1:4) = 'true'
                             Move Ws-Val-Type-Boolean
                               To Ws-JSON-Val-Type(Ws-JSON-Item-Idx)
                        When Ws-Temp-Val(1:5) = 'false'
                             Move Ws-Val-Type-Boolean
                               To Ws-JSON-Val-Type(Ws-JSON-Item-Idx)
                        When Ws-Temp-Val(1:4) = 'null'
                             Move Ws-Val-Type-Null
                               To Ws-JSON-Val-Type(Ws-JSON-Item-Idx)
                        When Other
                             Move Ws-Val-Type-String
                               To Ws-JSON-Val-Type(Ws-JSON-Item-Idx)
                    End-Evaluate
                 Else
                    Move Ws-Val-Type-String
                      To Ws-JSON-Val-Type(Ws-JSON-Item-Idx)
                 End-If
              End-If
           End-If

           Move Spaces To Ws-Temp-Val   
           Move Zero   To Ws-Temp-Val-Len 
           .

      ******************************************************************
      * Routine:  Display Array Items                                  *
      * Purpose:  Quick and dirty display of parsed array items        *
      ******************************************************************
       Display-Array-Items.

           Display 'Array Text: ' Ws-Array-Work(1:Ws-Array-Work-Len)
           Display ' '
           Display 'Array Parsed Item Count: ' Ws-JSON-Num-Items 
           Display ' '

           Perform Varying Ws-Counter From 1 By 1
                     Until Ws-Counter > Ws-JSON-Num-Items
              Set Ws-JSON-Item-Idx To Ws-Counter

              Move Ws-Counter To Ws-Num-Display

              Move Ws-JSON-Val(Ws-JSON-Item-Idx)     To Ws-Temp-Val
              Move Ws-JSON-Val-Len(Ws-JSON-Item-Idx) To Ws-Temp-Val-Len

              Display 'Item: '   Ws-Num-Display
                      ' Row: '   Ws-JSON-Row-Num(Ws-JSON-Item-Idx)
                      ' Col: '   Ws-JSON-Col-Num(Ws-JSON-Item-Idx)
                      ' Len: '   Ws-JSON-Val-Len(Ws-JSON-Item-Idx)
                      ' Type: '  Ws-JSON-Val-Type(Ws-JSON-Item-Idx)

              Display '  Value: ' Ws-Temp-Val(1:Ws-Temp-Val-Len)
           End-Perform
           .

      ******************************************************************
      *                   End Module JSON_ARRAY_PARSER                 *
      ******************************************************************