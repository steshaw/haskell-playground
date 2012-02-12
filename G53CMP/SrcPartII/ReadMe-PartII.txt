G53CMP Coursework, Part II: Type Checking and Code Generation
=============================================================

This document details Part II of the assessed coursework for the module G53CMP
Compilers. An extended, properly typeset version with more instructions, in
particular regarding what to submit, will be available shortly. But the first
two tasks for part II are as detailed here, so this suffice to get started.

As in Part I, the coursework is centred around the compiler HMTC for the small
language MiniTriangle. In this part, you will continue the work on the
extensions of the MiniTriangle language by extending the type checker and the
code generator to work with the new language constructs. As a starting point,
you will be given a new version of the MiniTriangle compiler that includes a
type checker and a code generator for the unextended MiniTriangle language, as
well as a scanner and parser already extended to handle the new features from
Part I. Thus you can also use this version of MiniTriangle as a model solution
for Part I, against which you can compare your own solution in the light of
the feedback you have received on your work on Part I.

The type system for the version of MiniTriangle we are using is explained and
specified (using typing rules) in the file "MHTTypeSystem.txt". You will need
to familiarise yourself with this. However, this file goes into a lot more
detail than what you will need for this coursework, so it is best to treat it
as a reference. In particular, you will not need to understand how the
suptyping and the references work in detail.

The target language of the compiler is Triangle Abstract Machine (TAM) code. 
The TAM is a simple stack machine, a simplified version of the one introduced
in Watt & Brown's book. The code you have been given also includes a TAM
interpreter, and you can instruct the compiler to run the generated code
directly via command line options "--run" and "--run-traced". The latter
variant turns on tracing, enabling you to see how the machine works. You can
also run the interpreter directly from withing GHCi by loading the module
"TAMInterpreter" can calling the function "runTAM" on a list of instructions. 
This is very helpful for getting familiar with how the TAM works and for
developing and debugging TAM code, e.g. for task II.3 below. There is no
complete written description of this version of the TAM yet. But the lecture
slides do go into a fair amount of detail, and the file "TAMCode.hs" gives a
complete albeit brief description of all TAM instructions. Of course, to
understand exactly what each instruction does, you can always refer to the
source code of the interpreter in "TAMInstructions.hs" which should be fairly
easy to follow.

Task II.1
---------

(Weight: 15 %)

In Part I of the coursework, you added the following new constructs to the
MiniTriangle language:

  * Repeat-until loops
  * Conditional expressions
  * Extended syntax for the if-command: elsif and optional else branch.
  * Character literals

However, Part I was only concerned with extending the scanner and the parser. 
The next step is to extend the type checker. As a preparatory step for this,
extend the HMT type system as detailed in "HMTTypeSystem.txt" by adding new
rules or modifying existing ones to cover the new constructs.

Use vector notation where appropriate as discussed in the lectures. In the
textual version of the rules, vectors are indicated by a suffix "s". You can
use the same convention, or typeset things properly using the overbar vector
notation as per the lecture slides.

The relevant abstract syntax productions are:

  c ::=
    ...
    |   repeat c until e					Repeat loop
    |   if e then c1 (elsif es then cs) (else c2 | epsilon)	Exteneded if

Note that it is understood that the lengths of es and cs agree, and that both
may be empty, thus making the elsifs optional.

  c :=
    ...
    |   chr			Literal character
    |   e ? e : e		Conditional expression

Task II.2
---------

(Weight: 35 %)

Now extend the actual type checker to cope with the new constructs according
to the extended type system you specified in Task II.1. This also means
you will have to add a new Character type, and that you will need to extend
the MiniTriangle standard environment correspondingly to make this type
available to the programmer. Furthermore, while the new language constructs
have been added to the initial abstract syntax AST (defined in "AST.hs"), the
type checker, besides checking the types, also translates this version
into a version of the abstract syntax called MTIR (Mini Triangle Intermediate
Representation) that has been annotated with semantic information. The
latter is defined in "MTIR.hs". You will need to extend this to cope
with the new constructs.

You will have to modify at least the following files:

  * Type.hs
  * MTStdEnv.hs
  * MTIR.hs
  * PPMTIR.hs
  * TypeChecker.hs

Task I.3
--------

Exact details to be determined. The task is concerned with implementing
TAM programs to gain familiarity with the TAM.

Task I.4
--------

Exact details to be determined. The goal is to extend the code generator
so that code is generated properly for the new language constructs.
