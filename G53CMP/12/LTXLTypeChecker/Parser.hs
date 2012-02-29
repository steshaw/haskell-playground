-- parser produced by Happy Version 1.14

module Parser (parse) where

import Char

import AbstractSyntax
import Type

-- Top-level parsing function
parse :: String -> Exp
parse = parser . scanner

data HappyAbsSyn t4 t5 t6 t7 t8 t9
	= HappyTerminal Token
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9

action_0 (10) = happyShift action_4
action_0 (11) = happyShift action_5
action_0 (18) = happyShift action_6
action_0 (21) = happyShift action_7
action_0 (22) = happyShift action_8
action_0 (24) = happyShift action_9
action_0 (27) = happyShift action_10
action_0 (4) = happyGoto action_11
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 _ = happyFail

action_1 (10) = happyShift action_4
action_1 (11) = happyShift action_5
action_1 (18) = happyShift action_6
action_1 (21) = happyShift action_7
action_1 (22) = happyShift action_8
action_1 (24) = happyShift action_9
action_1 (27) = happyShift action_10
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail

action_2 (12) = happyShift action_21
action_2 (13) = happyShift action_22
action_2 (14) = happyShift action_23
action_2 (15) = happyShift action_24
action_2 (16) = happyShift action_25
action_2 (17) = happyShift action_26
action_2 (18) = happyShift action_27
action_2 (19) = happyShift action_28
action_2 (20) = happyShift action_29
action_2 _ = happyReduce_1

action_3 _ = happyReduce_11

action_4 _ = happyReduce_12

action_5 _ = happyReduce_13

action_6 (10) = happyShift action_4
action_6 (11) = happyShift action_5
action_6 (18) = happyShift action_6
action_6 (21) = happyShift action_7
action_6 (22) = happyShift action_8
action_6 (24) = happyShift action_9
action_6 (27) = happyShift action_10
action_6 (6) = happyGoto action_20
action_6 _ = happyFail

action_7 (10) = happyShift action_4
action_7 (11) = happyShift action_5
action_7 (18) = happyShift action_6
action_7 (21) = happyShift action_7
action_7 (22) = happyShift action_8
action_7 (24) = happyShift action_9
action_7 (27) = happyShift action_10
action_7 (6) = happyGoto action_19
action_7 _ = happyFail

action_8 (10) = happyShift action_4
action_8 (11) = happyShift action_5
action_8 (18) = happyShift action_6
action_8 (21) = happyShift action_7
action_8 (22) = happyShift action_8
action_8 (24) = happyShift action_9
action_8 (27) = happyShift action_10
action_8 (5) = happyGoto action_18
action_8 (6) = happyGoto action_3
action_8 _ = happyFail

action_9 (10) = happyShift action_4
action_9 (11) = happyShift action_5
action_9 (18) = happyShift action_6
action_9 (21) = happyShift action_7
action_9 (22) = happyShift action_8
action_9 (24) = happyShift action_9
action_9 (27) = happyShift action_10
action_9 (5) = happyGoto action_17
action_9 (6) = happyGoto action_3
action_9 _ = happyFail

action_10 (29) = happyShift action_15
action_10 (30) = happyShift action_16
action_10 (7) = happyGoto action_12
action_10 (8) = happyGoto action_13
action_10 (9) = happyGoto action_14
action_10 _ = happyFail

action_11 (33) = happyAccept
action_11 _ = happyFail

action_12 (28) = happyShift action_43
action_12 _ = happyFail

action_13 (32) = happyShift action_42
action_13 _ = happyReduce_20

action_14 (11) = happyShift action_41
action_14 _ = happyFail

action_15 _ = happyReduce_22

action_16 _ = happyReduce_23

action_17 (12) = happyShift action_21
action_17 (13) = happyShift action_22
action_17 (14) = happyShift action_23
action_17 (15) = happyShift action_24
action_17 (16) = happyShift action_25
action_17 (17) = happyShift action_26
action_17 (18) = happyShift action_27
action_17 (19) = happyShift action_28
action_17 (20) = happyShift action_29
action_17 (25) = happyShift action_40
action_17 _ = happyFail

action_18 (12) = happyShift action_21
action_18 (13) = happyShift action_22
action_18 (14) = happyShift action_23
action_18 (15) = happyShift action_24
action_18 (16) = happyShift action_25
action_18 (17) = happyShift action_26
action_18 (18) = happyShift action_27
action_18 (19) = happyShift action_28
action_18 (20) = happyShift action_29
action_18 (23) = happyShift action_39
action_18 _ = happyFail

action_19 _ = happyReduce_14

action_20 _ = happyReduce_15

action_21 (10) = happyShift action_4
action_21 (11) = happyShift action_5
action_21 (18) = happyShift action_6
action_21 (21) = happyShift action_7
action_21 (22) = happyShift action_8
action_21 (24) = happyShift action_9
action_21 (27) = happyShift action_10
action_21 (5) = happyGoto action_38
action_21 (6) = happyGoto action_3
action_21 _ = happyFail

action_22 (10) = happyShift action_4
action_22 (11) = happyShift action_5
action_22 (18) = happyShift action_6
action_22 (21) = happyShift action_7
action_22 (22) = happyShift action_8
action_22 (24) = happyShift action_9
action_22 (27) = happyShift action_10
action_22 (5) = happyGoto action_37
action_22 (6) = happyGoto action_3
action_22 _ = happyFail

action_23 (10) = happyShift action_4
action_23 (11) = happyShift action_5
action_23 (18) = happyShift action_6
action_23 (21) = happyShift action_7
action_23 (22) = happyShift action_8
action_23 (24) = happyShift action_9
action_23 (27) = happyShift action_10
action_23 (5) = happyGoto action_36
action_23 (6) = happyGoto action_3
action_23 _ = happyFail

action_24 (10) = happyShift action_4
action_24 (11) = happyShift action_5
action_24 (18) = happyShift action_6
action_24 (21) = happyShift action_7
action_24 (22) = happyShift action_8
action_24 (24) = happyShift action_9
action_24 (27) = happyShift action_10
action_24 (5) = happyGoto action_35
action_24 (6) = happyGoto action_3
action_24 _ = happyFail

action_25 (10) = happyShift action_4
action_25 (11) = happyShift action_5
action_25 (18) = happyShift action_6
action_25 (21) = happyShift action_7
action_25 (22) = happyShift action_8
action_25 (24) = happyShift action_9
action_25 (27) = happyShift action_10
action_25 (5) = happyGoto action_34
action_25 (6) = happyGoto action_3
action_25 _ = happyFail

action_26 (10) = happyShift action_4
action_26 (11) = happyShift action_5
action_26 (18) = happyShift action_6
action_26 (21) = happyShift action_7
action_26 (22) = happyShift action_8
action_26 (24) = happyShift action_9
action_26 (27) = happyShift action_10
action_26 (5) = happyGoto action_33
action_26 (6) = happyGoto action_3
action_26 _ = happyFail

action_27 (10) = happyShift action_4
action_27 (11) = happyShift action_5
action_27 (18) = happyShift action_6
action_27 (21) = happyShift action_7
action_27 (22) = happyShift action_8
action_27 (24) = happyShift action_9
action_27 (27) = happyShift action_10
action_27 (5) = happyGoto action_32
action_27 (6) = happyGoto action_3
action_27 _ = happyFail

action_28 (10) = happyShift action_4
action_28 (11) = happyShift action_5
action_28 (18) = happyShift action_6
action_28 (21) = happyShift action_7
action_28 (22) = happyShift action_8
action_28 (24) = happyShift action_9
action_28 (27) = happyShift action_10
action_28 (5) = happyGoto action_31
action_28 (6) = happyGoto action_3
action_28 _ = happyFail

action_29 (10) = happyShift action_4
action_29 (11) = happyShift action_5
action_29 (18) = happyShift action_6
action_29 (21) = happyShift action_7
action_29 (22) = happyShift action_8
action_29 (24) = happyShift action_9
action_29 (27) = happyShift action_10
action_29 (5) = happyGoto action_30
action_29 (6) = happyGoto action_3
action_29 _ = happyFail

action_30 _ = happyReduce_10

action_31 _ = happyReduce_9

action_32 (19) = happyShift action_28
action_32 (20) = happyShift action_29
action_32 _ = happyReduce_8

action_33 (19) = happyShift action_28
action_33 (20) = happyShift action_29
action_33 _ = happyReduce_7

action_34 (17) = happyShift action_26
action_34 (18) = happyShift action_27
action_34 (19) = happyShift action_28
action_34 (20) = happyShift action_29
action_34 _ = happyReduce_6

action_35 (17) = happyShift action_26
action_35 (18) = happyShift action_27
action_35 (19) = happyShift action_28
action_35 (20) = happyShift action_29
action_35 _ = happyReduce_5

action_36 (17) = happyShift action_26
action_36 (18) = happyShift action_27
action_36 (19) = happyShift action_28
action_36 (20) = happyShift action_29
action_36 _ = happyReduce_4

action_37 (14) = happyShift action_23
action_37 (15) = happyShift action_24
action_37 (16) = happyShift action_25
action_37 (17) = happyShift action_26
action_37 (18) = happyShift action_27
action_37 (19) = happyShift action_28
action_37 (20) = happyShift action_29
action_37 _ = happyReduce_3

action_38 (13) = happyShift action_22
action_38 (14) = happyShift action_23
action_38 (15) = happyShift action_24
action_38 (16) = happyShift action_25
action_38 (17) = happyShift action_26
action_38 (18) = happyShift action_27
action_38 (19) = happyShift action_28
action_38 (20) = happyShift action_29
action_38 _ = happyReduce_2

action_39 _ = happyReduce_17

action_40 (10) = happyShift action_4
action_40 (11) = happyShift action_5
action_40 (18) = happyShift action_6
action_40 (21) = happyShift action_7
action_40 (22) = happyShift action_8
action_40 (24) = happyShift action_9
action_40 (27) = happyShift action_10
action_40 (5) = happyGoto action_47
action_40 (6) = happyGoto action_3
action_40 _ = happyFail

action_41 (31) = happyShift action_46
action_41 _ = happyFail

action_42 (29) = happyShift action_15
action_42 (30) = happyShift action_16
action_42 (7) = happyGoto action_45
action_42 (8) = happyGoto action_13
action_42 (9) = happyGoto action_14
action_42 _ = happyFail

action_43 (10) = happyShift action_4
action_43 (11) = happyShift action_5
action_43 (18) = happyShift action_6
action_43 (21) = happyShift action_7
action_43 (22) = happyShift action_8
action_43 (24) = happyShift action_9
action_43 (27) = happyShift action_10
action_43 (5) = happyGoto action_44
action_43 (6) = happyGoto action_3
action_43 _ = happyFail

action_44 (12) = happyShift action_21
action_44 (13) = happyShift action_22
action_44 (14) = happyShift action_23
action_44 (15) = happyShift action_24
action_44 (16) = happyShift action_25
action_44 (17) = happyShift action_26
action_44 (18) = happyShift action_27
action_44 (19) = happyShift action_28
action_44 (20) = happyShift action_29
action_44 _ = happyReduce_18

action_45 _ = happyReduce_19

action_46 (10) = happyShift action_4
action_46 (11) = happyShift action_5
action_46 (18) = happyShift action_6
action_46 (21) = happyShift action_7
action_46 (22) = happyShift action_8
action_46 (24) = happyShift action_9
action_46 (27) = happyShift action_10
action_46 (5) = happyGoto action_49
action_46 (6) = happyGoto action_3
action_46 _ = happyFail

action_47 (12) = happyShift action_21
action_47 (13) = happyShift action_22
action_47 (14) = happyShift action_23
action_47 (15) = happyShift action_24
action_47 (16) = happyShift action_25
action_47 (17) = happyShift action_26
action_47 (18) = happyShift action_27
action_47 (19) = happyShift action_28
action_47 (20) = happyShift action_29
action_47 (26) = happyShift action_48
action_47 _ = happyFail

action_48 (10) = happyShift action_4
action_48 (11) = happyShift action_5
action_48 (18) = happyShift action_6
action_48 (21) = happyShift action_7
action_48 (22) = happyShift action_8
action_48 (24) = happyShift action_9
action_48 (27) = happyShift action_10
action_48 (5) = happyGoto action_50
action_48 (6) = happyGoto action_3
action_48 _ = happyFail

action_49 (12) = happyShift action_21
action_49 (13) = happyShift action_22
action_49 (14) = happyShift action_23
action_49 (15) = happyShift action_24
action_49 (16) = happyShift action_25
action_49 (17) = happyShift action_26
action_49 (18) = happyShift action_27
action_49 (19) = happyShift action_28
action_49 (20) = happyShift action_29
action_49 _ = happyReduce_21

action_50 (12) = happyShift action_21
action_50 (13) = happyShift action_22
action_50 (14) = happyShift action_23
action_50 (15) = happyShift action_24
action_50 (16) = happyShift action_25
action_50 (17) = happyShift action_26
action_50 (18) = happyShift action_27
action_50 (19) = happyShift action_28
action_50 (20) = happyShift action_29
action_50 _ = happyReduce_16

happyReduce_1 = happySpecReduce_1 4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3 5 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinOpApp Or happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3 5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinOpApp And happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3 5 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinOpApp Less happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3 5 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinOpApp Equal happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3 5 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinOpApp Greater happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3 5 happyReduction_7
happyReduction_7 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinOpApp Plus happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3 5 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinOpApp Minus happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3 5 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinOpApp Times happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3 5 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinOpApp Divide happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1 5 happyReduction_11
happyReduction_11 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1 6 happyReduction_12
happyReduction_12 (HappyTerminal (T_LitInt happy_var_1))
	 =  HappyAbsSyn6
		 (LitInt happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1 6 happyReduction_13
happyReduction_13 (HappyTerminal (T_Id happy_var_1))
	 =  HappyAbsSyn6
		 (Var happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2 6 happyReduction_14
happyReduction_14 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (UnOpApp Not happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2 6 happyReduction_15
happyReduction_15 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (UnOpApp Neg happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 6 6 happyReduction_16
happyReduction_16 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_3 6 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 6 happyReduction_18
happyReduction_18 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Let happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_3 7 happyReduction_19
happyReduction_19 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1 7 happyReduction_20
happyReduction_20 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happyReduce 4 8 happyReduction_21
happyReduction_21 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T_Id happy_var_2)) `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ((happy_var_2, happy_var_1, happy_var_4)
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_1 9 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn9
		 (TpInt
	)

happyReduce_23 = happySpecReduce_1 9 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn9
		 (TpBool
	)

happyNewToken action sts stk [] =
	action 33 33 (error "reading EOF!") (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	T_LitInt happy_dollar_dollar -> cont 10;
	T_Id happy_dollar_dollar -> cont 11;
	T_Or -> cont 12;
	T_And -> cont 13;
	T_Less -> cont 14;
	T_Equal -> cont 15;
	T_Greater -> cont 16;
	T_Plus -> cont 17;
	T_Minus -> cont 18;
	T_Times -> cont 19;
	T_Divide -> cont 20;
	T_Not -> cont 21;
	T_LeftPar -> cont 22;
	T_RightPar -> cont 23;
	T_If -> cont 24;
	T_Then -> cont 25;
	T_Else -> cont 26;
	T_Let -> cont 27;
	T_In -> cont 28;
	T_IntType -> cont 29;
	T_BoolType -> cont 30;
	T_Is -> cont 31;
	T_SemiColon -> cont 32;
	_ -> happyError tks
	}

happyThen = \m k -> k m
happyReturn = \a -> a
happyThen1 = happyThen
happyReturn1 = \a tks -> a

parser tks = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq

-------------------------------------------------------------
-- Token type
-------------------------------------------------------------

data Token = T_LitInt Int
           | T_Id Id
           | T_Or
	   | T_And
           | T_Less
           | T_Equal
           | T_Greater
           | T_Plus
           | T_Minus
           | T_Times
           | T_Divide
           | T_Not
           | T_LeftPar
           | T_RightPar
           | T_Let
           | T_In
           | T_IntType
           | T_BoolType
           | T_Is
           | T_SemiColon
           | T_If
           | T_Then
           | T_Else
           deriving Show 


-------------------------------------------------------------
-- Scanner
-------------------------------------------------------------

scanner :: [Char] -> [Token]
-- End of input
scanner []          = []
-- Drop white space and comments
scanner (' '  : cs) = scanner cs
scanner ('\n' : cs) = scanner cs
scanner ('!'  : cs) = scanner (dropWhile (/='\n') cs)
-- Scan graphic tokens
scanner ('|'  : '|' : cs) = T_Or        : scanner cs
scanner ('&'  : '&' : cs) = T_And       : scanner cs
scanner ('<'  : cs)       = T_Less      : scanner cs
scanner ('='  : '=' : cs) = T_Equal     : scanner cs
scanner ('>'  : cs)       = T_Greater   : scanner cs
scanner ('+'  : cs)       = T_Plus      : scanner cs
scanner ('-'  : cs)       = T_Minus     : scanner cs
scanner ('*'  : cs)       = T_Times     : scanner cs
scanner ('/'  : cs)       = T_Divide    : scanner cs
scanner ('\\' : cs)       = T_Not       : scanner cs
scanner ('('  : cs)       = T_LeftPar   : scanner cs
scanner (')'  : cs)       = T_RightPar  : scanner cs
scanner ('='  : cs)       = T_Is        : scanner cs
scanner (';'  : cs)       = T_SemiColon : scanner cs
-- Scan literal integers, identifiers, and keywords
scanner (c : cs) | isDigit c =
                       T_LitInt (read (c :
                                       takeWhile isDigit cs))
                       : scanner (dropWhile isDigit cs)
                 | isAlpha c =
                       mkIdOrKwd (c :
                                  takeWhile isAlphaNum cs)
                       : scanner (dropWhile isAlphaNum cs)
                 | otherwise = error "Illegal character!"
    where
        mkIdOrKwd "if"   = T_If
        mkIdOrKwd "then" = T_Then
        mkIdOrKwd "else" = T_Else
        mkIdOrKwd "let"  = T_Let
        mkIdOrKwd "in"   = T_In
        mkIdOrKwd "int"  = T_IntType
        mkIdOrKwd "bool" = T_BoolType
        mkIdOrKwd cs     = T_Id cs


------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

happyError :: [Token] -> a
happyError _ = error "Parse error"
{-# LINE 1 "GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "GenericTemplate.hs" #-}
-- $Id$

{-# LINE 16 "GenericTemplate.hs" #-}
{-# LINE 28 "GenericTemplate.hs" #-}









































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

happyAccept j tk st sts (HappyStk ans _) = 

					   (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 151 "GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 235 "GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError


{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 300 "GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
