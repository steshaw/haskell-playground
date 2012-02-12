G53CMP Coursework, Part I: Scanning and Parsing
===============================================

This document describes Part I of the G53CMP coursework. An extended, properly
typeset version with more instructions, in particular regarding what to
submit, will be available shortly. But the tasks for part I are going
to be as detailed here, so this suffice to get started.

Task I.1
--------

Task:
  Extend MiniTriangle with a repeat-loop. Informally, the loop construct
  has the following syntax:

    repeat
        <cmd>
    until
        <boolExp>   

  The semantics is that the command <cmd> is repeated (at least once) until
  <boolExp> evaluates to true.

  For example, the following should be a valid MiniTriangle program fragment:

    repeat
        x := x + 1
    until x > 42

  First add the relevant production to the MiniTriangle grammar,
  then extend the provided HMTC code accordingly.

  You will have to modify:
    * Token.hs
    * Scanner.hs
    * AST.hs
    * Parser.y (NOT "Parser.hs": this file is generated from "Parser.y".)
    * PPAST.hs

Task I.2
--------

Task:
  Extend MiniTriangle with C/Java-style conditional expressions. Informally,
  the conditional expression should have the following syntax:

    <boolExp> ? <exp1> : <exp1>

  The semantics is that <boolExp> is evaluated first. If its value is true,
  then <exp1> gets evaluated and that result becomes the overall result of
  the conditional expression. Otherwise <exp2> gets evaluated and that result
  becomes the overall result.

  First add the relevant production to the MiniTriangle grammar,
  then extend the provided HMTC code accordingly. You will have to modify
  the same files as for task I.1.

  The "?" should be a new, distinct, token (do not treat it as an operator),
  and conditional expressions should be a new, distinct, type of expressions
  in the abstract syntax (do not attempt to use "ExpApp"). The conditional
  operator should have the lowest precedence of all operators, and it
  should be right associative.

Task I.3
--------

Task:
  Extend the syntax of MiniTriangle if-command so that:
    - the else-branch is optional
    - zero or more Ada-style "elsif ... then ..." are allowed after the
      then-branch but before the (now optional) else-branch.

  For example:

    if x > 1 then
        y := 1 
    
  and

    if x < 10 then
        y := 1
    elsif x == 10 then
        y := 2
    elsif x > 10 then
        y := 3
    else
        y := 0

  should both be valid MiniTriangle fragments.

  First add the relevant production(s) to the MiniTriangle grammar,
  then extend the provided HMTC code accordingly. You will have to modify
  the same files as for task I.1.

  The definition of the constructor "CmdIf" in AST needs to be changed to
  accommodate the richer syntax. The pretty-printing utility functions
  "ppOpt" and "ppSeq" will likely be useful to you when you extend the
  pretty printer. For example, the second MiniTriangle fragment above
  should be printed along the lines:

    CmdIf <line 1, column 1>
      ExpApp <line 1, column 4>
        ExpVar "<"
        ExpVar "x"
        ExpLitInt 10
      CmdAssign <line 2, column 5>
        ExpVar "y"
        ExpLitInt 1
      ExpApp <line 3, column 7>
        ExpVar "=="
        ExpVar "x"
        ExpLitInt 10
      CmdAssign <line 4, column 5>
        ExpVar "y"
        ExpLitInt 2
      ExpApp <line 5, column 7>
        ExpVar ">"
        ExpVar "x"
        ExpLitInt 10
      CmdAssign <line 6, column 5>
        ExpVar "y"
        ExpLitInt 3
      CmdAssign <line 8, column 5>
        ExpVar "y"
        ExpLitInt 0

  Hint: Do the optional else first. Make sure this works properly. Then
  attempt "elsif". Introduce extra non-terminals and productions as needed.

Task I.4
--------

Task:
  Extend MiniTriangle with character literals:

    Character-Literal -> ' (Graphic | Character-Escape) '
    Graphic           -> any non-control character except ' and \
    Character Escape  -> \ (n | r | t | \ ')

  You will again have to modify the same files as for Task I.1.
