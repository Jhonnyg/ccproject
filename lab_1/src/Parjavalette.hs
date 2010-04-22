{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Parjavalette where
import Absjavalette
import Lexjavalette
import ErrM

-- parser produced by Happy Version 1.18.4

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Ident)
	| HappyAbsSyn5 (Integer)
	| HappyAbsSyn6 (Double)
	| HappyAbsSyn7 (String)
	| HappyAbsSyn8 (Program)
	| HappyAbsSyn9 (TopDef)
	| HappyAbsSyn10 ([TopDef])
	| HappyAbsSyn11 (Arg)
	| HappyAbsSyn12 ([Arg])
	| HappyAbsSyn13 (Stmt)
	| HappyAbsSyn14 (Block)
	| HappyAbsSyn15 ([Stmt])
	| HappyAbsSyn16 (Item)
	| HappyAbsSyn17 ([Item])
	| HappyAbsSyn18 (Type)
	| HappyAbsSyn19 ([Type])
	| HappyAbsSyn20 (Expr)
	| HappyAbsSyn27 ([Expr])
	| HappyAbsSyn28 (AddOp)
	| HappyAbsSyn29 (MulOp)
	| HappyAbsSyn30 (RelOp)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112 :: () => Int -> ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73 :: () => ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

action_0 (54) = happyShift action_7
action_0 (55) = happyShift action_8
action_0 (59) = happyShift action_9
action_0 (62) = happyShift action_10
action_0 (8) = happyGoto action_3
action_0 (9) = happyGoto action_4
action_0 (10) = happyGoto action_5
action_0 (18) = happyGoto action_6
action_0 _ = happyFail

action_1 (64) = happyShift action_2
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (69) = happyAccept
action_3 _ = happyFail

action_4 (54) = happyShift action_7
action_4 (55) = happyShift action_8
action_4 (59) = happyShift action_9
action_4 (62) = happyShift action_10
action_4 (9) = happyGoto action_4
action_4 (10) = happyGoto action_12
action_4 (18) = happyGoto action_6
action_4 _ = happyReduce_7

action_5 _ = happyReduce_5

action_6 (64) = happyShift action_2
action_6 (4) = happyGoto action_11
action_6 _ = happyFail

action_7 _ = happyReduce_34

action_8 _ = happyReduce_33

action_9 _ = happyReduce_32

action_10 _ = happyReduce_35

action_11 (31) = happyShift action_13
action_11 _ = happyFail

action_12 _ = happyReduce_8

action_13 (54) = happyShift action_7
action_13 (55) = happyShift action_8
action_13 (59) = happyShift action_9
action_13 (62) = happyShift action_10
action_13 (11) = happyGoto action_14
action_13 (12) = happyGoto action_15
action_13 (18) = happyGoto action_16
action_13 _ = happyReduce_10

action_14 (33) = happyShift action_19
action_14 _ = happyReduce_11

action_15 (32) = happyShift action_18
action_15 _ = happyFail

action_16 (64) = happyShift action_2
action_16 (4) = happyGoto action_17
action_16 _ = happyFail

action_17 _ = happyReduce_9

action_18 (34) = happyShift action_22
action_18 (14) = happyGoto action_21
action_18 _ = happyFail

action_19 (54) = happyShift action_7
action_19 (55) = happyShift action_8
action_19 (59) = happyShift action_9
action_19 (62) = happyShift action_10
action_19 (11) = happyGoto action_14
action_19 (12) = happyGoto action_20
action_19 (18) = happyGoto action_16
action_19 _ = happyReduce_10

action_20 _ = happyReduce_12

action_21 _ = happyReduce_6

action_22 (15) = happyGoto action_23
action_22 _ = happyReduce_26

action_23 (31) = happyShift action_37
action_23 (34) = happyShift action_22
action_23 (35) = happyShift action_38
action_23 (36) = happyShift action_39
action_23 (40) = happyShift action_40
action_23 (41) = happyShift action_41
action_23 (54) = happyShift action_7
action_23 (55) = happyShift action_8
action_23 (57) = happyShift action_42
action_23 (58) = happyShift action_43
action_23 (59) = happyShift action_9
action_23 (60) = happyShift action_44
action_23 (61) = happyShift action_45
action_23 (62) = happyShift action_10
action_23 (63) = happyShift action_46
action_23 (64) = happyShift action_2
action_23 (65) = happyShift action_47
action_23 (66) = happyShift action_48
action_23 (4) = happyGoto action_24
action_23 (5) = happyGoto action_25
action_23 (6) = happyGoto action_26
action_23 (13) = happyGoto action_27
action_23 (14) = happyGoto action_28
action_23 (18) = happyGoto action_29
action_23 (20) = happyGoto action_30
action_23 (21) = happyGoto action_31
action_23 (22) = happyGoto action_32
action_23 (23) = happyGoto action_33
action_23 (24) = happyGoto action_34
action_23 (25) = happyGoto action_35
action_23 (26) = happyGoto action_36
action_23 _ = happyFail

action_24 (31) = happyShift action_77
action_24 (37) = happyShift action_78
action_24 (38) = happyShift action_79
action_24 (39) = happyShift action_80
action_24 _ = happyReduce_39

action_25 _ = happyReduce_40

action_26 _ = happyReduce_41

action_27 _ = happyReduce_27

action_28 _ = happyReduce_14

action_29 (64) = happyShift action_2
action_29 (4) = happyGoto action_74
action_29 (16) = happyGoto action_75
action_29 (17) = happyGoto action_76
action_29 _ = happyFail

action_30 _ = happyReduce_49

action_31 _ = happyReduce_51

action_32 (45) = happyShift action_71
action_32 (46) = happyShift action_72
action_32 (47) = happyShift action_73
action_32 (29) = happyGoto action_70
action_32 _ = happyReduce_53

action_33 (40) = happyShift action_68
action_33 (44) = happyShift action_69
action_33 (28) = happyGoto action_67
action_33 _ = happyReduce_55

action_34 (42) = happyShift action_60
action_34 (48) = happyShift action_61
action_34 (49) = happyShift action_62
action_34 (50) = happyShift action_63
action_34 (51) = happyShift action_64
action_34 (52) = happyShift action_65
action_34 (53) = happyShift action_66
action_34 (30) = happyGoto action_59
action_34 _ = happyReduce_57

action_35 (43) = happyShift action_58
action_35 _ = happyReduce_59

action_36 (36) = happyShift action_57
action_36 _ = happyFail

action_37 (31) = happyShift action_37
action_37 (40) = happyShift action_40
action_37 (41) = happyShift action_41
action_37 (57) = happyShift action_42
action_37 (61) = happyShift action_45
action_37 (64) = happyShift action_2
action_37 (65) = happyShift action_47
action_37 (66) = happyShift action_48
action_37 (4) = happyGoto action_50
action_37 (5) = happyGoto action_25
action_37 (6) = happyGoto action_26
action_37 (20) = happyGoto action_30
action_37 (21) = happyGoto action_31
action_37 (22) = happyGoto action_32
action_37 (23) = happyGoto action_33
action_37 (24) = happyGoto action_34
action_37 (25) = happyGoto action_35
action_37 (26) = happyGoto action_56
action_37 _ = happyFail

action_38 _ = happyReduce_25

action_39 _ = happyReduce_13

action_40 (31) = happyShift action_37
action_40 (57) = happyShift action_42
action_40 (61) = happyShift action_45
action_40 (64) = happyShift action_2
action_40 (65) = happyShift action_47
action_40 (66) = happyShift action_48
action_40 (4) = happyGoto action_50
action_40 (5) = happyGoto action_25
action_40 (6) = happyGoto action_26
action_40 (20) = happyGoto action_55
action_40 _ = happyFail

action_41 (31) = happyShift action_37
action_41 (57) = happyShift action_42
action_41 (61) = happyShift action_45
action_41 (64) = happyShift action_2
action_41 (65) = happyShift action_47
action_41 (66) = happyShift action_48
action_41 (4) = happyGoto action_50
action_41 (5) = happyGoto action_25
action_41 (6) = happyGoto action_26
action_41 (20) = happyGoto action_54
action_41 _ = happyFail

action_42 _ = happyReduce_43

action_43 (31) = happyShift action_53
action_43 _ = happyFail

action_44 (31) = happyShift action_37
action_44 (36) = happyShift action_52
action_44 (40) = happyShift action_40
action_44 (41) = happyShift action_41
action_44 (57) = happyShift action_42
action_44 (61) = happyShift action_45
action_44 (64) = happyShift action_2
action_44 (65) = happyShift action_47
action_44 (66) = happyShift action_48
action_44 (4) = happyGoto action_50
action_44 (5) = happyGoto action_25
action_44 (6) = happyGoto action_26
action_44 (20) = happyGoto action_30
action_44 (21) = happyGoto action_31
action_44 (22) = happyGoto action_32
action_44 (23) = happyGoto action_33
action_44 (24) = happyGoto action_34
action_44 (25) = happyGoto action_35
action_44 (26) = happyGoto action_51
action_44 _ = happyFail

action_45 _ = happyReduce_42

action_46 (31) = happyShift action_49
action_46 _ = happyFail

action_47 _ = happyReduce_2

action_48 _ = happyReduce_3

action_49 (31) = happyShift action_37
action_49 (40) = happyShift action_40
action_49 (41) = happyShift action_41
action_49 (57) = happyShift action_42
action_49 (61) = happyShift action_45
action_49 (64) = happyShift action_2
action_49 (65) = happyShift action_47
action_49 (66) = happyShift action_48
action_49 (4) = happyGoto action_50
action_49 (5) = happyGoto action_25
action_49 (6) = happyGoto action_26
action_49 (20) = happyGoto action_30
action_49 (21) = happyGoto action_31
action_49 (22) = happyGoto action_32
action_49 (23) = happyGoto action_33
action_49 (24) = happyGoto action_34
action_49 (25) = happyGoto action_35
action_49 (26) = happyGoto action_99
action_49 _ = happyFail

action_50 (31) = happyShift action_77
action_50 _ = happyReduce_39

action_51 (36) = happyShift action_98
action_51 _ = happyFail

action_52 _ = happyReduce_20

action_53 (31) = happyShift action_37
action_53 (40) = happyShift action_40
action_53 (41) = happyShift action_41
action_53 (57) = happyShift action_42
action_53 (61) = happyShift action_45
action_53 (64) = happyShift action_2
action_53 (65) = happyShift action_47
action_53 (66) = happyShift action_48
action_53 (4) = happyGoto action_50
action_53 (5) = happyGoto action_25
action_53 (6) = happyGoto action_26
action_53 (20) = happyGoto action_30
action_53 (21) = happyGoto action_31
action_53 (22) = happyGoto action_32
action_53 (23) = happyGoto action_33
action_53 (24) = happyGoto action_34
action_53 (25) = happyGoto action_35
action_53 (26) = happyGoto action_97
action_53 _ = happyFail

action_54 _ = happyReduce_48

action_55 _ = happyReduce_47

action_56 (32) = happyShift action_96
action_56 _ = happyFail

action_57 _ = happyReduce_24

action_58 (31) = happyShift action_37
action_58 (40) = happyShift action_40
action_58 (41) = happyShift action_41
action_58 (57) = happyShift action_42
action_58 (61) = happyShift action_45
action_58 (64) = happyShift action_2
action_58 (65) = happyShift action_47
action_58 (66) = happyShift action_48
action_58 (4) = happyGoto action_50
action_58 (5) = happyGoto action_25
action_58 (6) = happyGoto action_26
action_58 (20) = happyGoto action_30
action_58 (21) = happyGoto action_31
action_58 (22) = happyGoto action_32
action_58 (23) = happyGoto action_33
action_58 (24) = happyGoto action_34
action_58 (25) = happyGoto action_35
action_58 (26) = happyGoto action_95
action_58 _ = happyFail

action_59 (31) = happyShift action_37
action_59 (40) = happyShift action_40
action_59 (41) = happyShift action_41
action_59 (57) = happyShift action_42
action_59 (61) = happyShift action_45
action_59 (64) = happyShift action_2
action_59 (65) = happyShift action_47
action_59 (66) = happyShift action_48
action_59 (4) = happyGoto action_50
action_59 (5) = happyGoto action_25
action_59 (6) = happyGoto action_26
action_59 (20) = happyGoto action_30
action_59 (21) = happyGoto action_31
action_59 (22) = happyGoto action_32
action_59 (23) = happyGoto action_94
action_59 _ = happyFail

action_60 (31) = happyShift action_37
action_60 (40) = happyShift action_40
action_60 (41) = happyShift action_41
action_60 (57) = happyShift action_42
action_60 (61) = happyShift action_45
action_60 (64) = happyShift action_2
action_60 (65) = happyShift action_47
action_60 (66) = happyShift action_48
action_60 (4) = happyGoto action_50
action_60 (5) = happyGoto action_25
action_60 (6) = happyGoto action_26
action_60 (20) = happyGoto action_30
action_60 (21) = happyGoto action_31
action_60 (22) = happyGoto action_32
action_60 (23) = happyGoto action_33
action_60 (24) = happyGoto action_34
action_60 (25) = happyGoto action_93
action_60 _ = happyFail

action_61 _ = happyReduce_68

action_62 _ = happyReduce_69

action_63 _ = happyReduce_70

action_64 _ = happyReduce_71

action_65 _ = happyReduce_72

action_66 _ = happyReduce_73

action_67 (31) = happyShift action_37
action_67 (40) = happyShift action_40
action_67 (41) = happyShift action_41
action_67 (57) = happyShift action_42
action_67 (61) = happyShift action_45
action_67 (64) = happyShift action_2
action_67 (65) = happyShift action_47
action_67 (66) = happyShift action_48
action_67 (4) = happyGoto action_50
action_67 (5) = happyGoto action_25
action_67 (6) = happyGoto action_26
action_67 (20) = happyGoto action_30
action_67 (21) = happyGoto action_31
action_67 (22) = happyGoto action_92
action_67 _ = happyFail

action_68 _ = happyReduce_64

action_69 _ = happyReduce_63

action_70 (31) = happyShift action_37
action_70 (40) = happyShift action_40
action_70 (41) = happyShift action_41
action_70 (57) = happyShift action_42
action_70 (61) = happyShift action_45
action_70 (64) = happyShift action_2
action_70 (65) = happyShift action_47
action_70 (66) = happyShift action_48
action_70 (4) = happyGoto action_50
action_70 (5) = happyGoto action_25
action_70 (6) = happyGoto action_26
action_70 (20) = happyGoto action_30
action_70 (21) = happyGoto action_91
action_70 _ = happyFail

action_71 _ = happyReduce_65

action_72 _ = happyReduce_66

action_73 _ = happyReduce_67

action_74 (37) = happyShift action_90
action_74 _ = happyReduce_28

action_75 (33) = happyShift action_89
action_75 _ = happyReduce_30

action_76 (36) = happyShift action_88
action_76 _ = happyFail

action_77 (31) = happyShift action_37
action_77 (40) = happyShift action_40
action_77 (41) = happyShift action_41
action_77 (57) = happyShift action_42
action_77 (61) = happyShift action_45
action_77 (64) = happyShift action_2
action_77 (65) = happyShift action_47
action_77 (66) = happyShift action_48
action_77 (67) = happyShift action_87
action_77 (4) = happyGoto action_50
action_77 (5) = happyGoto action_25
action_77 (6) = happyGoto action_26
action_77 (7) = happyGoto action_84
action_77 (20) = happyGoto action_30
action_77 (21) = happyGoto action_31
action_77 (22) = happyGoto action_32
action_77 (23) = happyGoto action_33
action_77 (24) = happyGoto action_34
action_77 (25) = happyGoto action_35
action_77 (26) = happyGoto action_85
action_77 (27) = happyGoto action_86
action_77 _ = happyReduce_60

action_78 (31) = happyShift action_37
action_78 (40) = happyShift action_40
action_78 (41) = happyShift action_41
action_78 (57) = happyShift action_42
action_78 (61) = happyShift action_45
action_78 (64) = happyShift action_2
action_78 (65) = happyShift action_47
action_78 (66) = happyShift action_48
action_78 (4) = happyGoto action_50
action_78 (5) = happyGoto action_25
action_78 (6) = happyGoto action_26
action_78 (20) = happyGoto action_30
action_78 (21) = happyGoto action_31
action_78 (22) = happyGoto action_32
action_78 (23) = happyGoto action_33
action_78 (24) = happyGoto action_34
action_78 (25) = happyGoto action_35
action_78 (26) = happyGoto action_83
action_78 _ = happyFail

action_79 (36) = happyShift action_82
action_79 _ = happyFail

action_80 (36) = happyShift action_81
action_80 _ = happyFail

action_81 _ = happyReduce_18

action_82 _ = happyReduce_17

action_83 (36) = happyShift action_107
action_83 _ = happyFail

action_84 (32) = happyShift action_106
action_84 _ = happyFail

action_85 (33) = happyShift action_105
action_85 _ = happyReduce_61

action_86 (32) = happyShift action_104
action_86 _ = happyFail

action_87 _ = happyReduce_4

action_88 _ = happyReduce_15

action_89 (64) = happyShift action_2
action_89 (4) = happyGoto action_74
action_89 (16) = happyGoto action_75
action_89 (17) = happyGoto action_103
action_89 _ = happyFail

action_90 (31) = happyShift action_37
action_90 (40) = happyShift action_40
action_90 (41) = happyShift action_41
action_90 (57) = happyShift action_42
action_90 (61) = happyShift action_45
action_90 (64) = happyShift action_2
action_90 (65) = happyShift action_47
action_90 (66) = happyShift action_48
action_90 (4) = happyGoto action_50
action_90 (5) = happyGoto action_25
action_90 (6) = happyGoto action_26
action_90 (20) = happyGoto action_30
action_90 (21) = happyGoto action_31
action_90 (22) = happyGoto action_32
action_90 (23) = happyGoto action_33
action_90 (24) = happyGoto action_34
action_90 (25) = happyGoto action_35
action_90 (26) = happyGoto action_102
action_90 _ = happyFail

action_91 _ = happyReduce_50

action_92 (45) = happyShift action_71
action_92 (46) = happyShift action_72
action_92 (47) = happyShift action_73
action_92 (29) = happyGoto action_70
action_92 _ = happyReduce_52

action_93 _ = happyReduce_56

action_94 (40) = happyShift action_68
action_94 (44) = happyShift action_69
action_94 (28) = happyGoto action_67
action_94 _ = happyReduce_54

action_95 _ = happyReduce_58

action_96 _ = happyReduce_46

action_97 (32) = happyShift action_101
action_97 _ = happyFail

action_98 _ = happyReduce_19

action_99 (32) = happyShift action_100
action_99 _ = happyFail

action_100 (31) = happyShift action_37
action_100 (34) = happyShift action_22
action_100 (36) = happyShift action_39
action_100 (40) = happyShift action_40
action_100 (41) = happyShift action_41
action_100 (54) = happyShift action_7
action_100 (55) = happyShift action_8
action_100 (57) = happyShift action_42
action_100 (58) = happyShift action_43
action_100 (59) = happyShift action_9
action_100 (60) = happyShift action_44
action_100 (61) = happyShift action_45
action_100 (62) = happyShift action_10
action_100 (63) = happyShift action_46
action_100 (64) = happyShift action_2
action_100 (65) = happyShift action_47
action_100 (66) = happyShift action_48
action_100 (4) = happyGoto action_24
action_100 (5) = happyGoto action_25
action_100 (6) = happyGoto action_26
action_100 (13) = happyGoto action_110
action_100 (14) = happyGoto action_28
action_100 (18) = happyGoto action_29
action_100 (20) = happyGoto action_30
action_100 (21) = happyGoto action_31
action_100 (22) = happyGoto action_32
action_100 (23) = happyGoto action_33
action_100 (24) = happyGoto action_34
action_100 (25) = happyGoto action_35
action_100 (26) = happyGoto action_36
action_100 _ = happyFail

action_101 (31) = happyShift action_37
action_101 (34) = happyShift action_22
action_101 (36) = happyShift action_39
action_101 (40) = happyShift action_40
action_101 (41) = happyShift action_41
action_101 (54) = happyShift action_7
action_101 (55) = happyShift action_8
action_101 (57) = happyShift action_42
action_101 (58) = happyShift action_43
action_101 (59) = happyShift action_9
action_101 (60) = happyShift action_44
action_101 (61) = happyShift action_45
action_101 (62) = happyShift action_10
action_101 (63) = happyShift action_46
action_101 (64) = happyShift action_2
action_101 (65) = happyShift action_47
action_101 (66) = happyShift action_48
action_101 (4) = happyGoto action_24
action_101 (5) = happyGoto action_25
action_101 (6) = happyGoto action_26
action_101 (13) = happyGoto action_109
action_101 (14) = happyGoto action_28
action_101 (18) = happyGoto action_29
action_101 (20) = happyGoto action_30
action_101 (21) = happyGoto action_31
action_101 (22) = happyGoto action_32
action_101 (23) = happyGoto action_33
action_101 (24) = happyGoto action_34
action_101 (25) = happyGoto action_35
action_101 (26) = happyGoto action_36
action_101 _ = happyFail

action_102 _ = happyReduce_29

action_103 _ = happyReduce_31

action_104 _ = happyReduce_44

action_105 (31) = happyShift action_37
action_105 (40) = happyShift action_40
action_105 (41) = happyShift action_41
action_105 (57) = happyShift action_42
action_105 (61) = happyShift action_45
action_105 (64) = happyShift action_2
action_105 (65) = happyShift action_47
action_105 (66) = happyShift action_48
action_105 (4) = happyGoto action_50
action_105 (5) = happyGoto action_25
action_105 (6) = happyGoto action_26
action_105 (20) = happyGoto action_30
action_105 (21) = happyGoto action_31
action_105 (22) = happyGoto action_32
action_105 (23) = happyGoto action_33
action_105 (24) = happyGoto action_34
action_105 (25) = happyGoto action_35
action_105 (26) = happyGoto action_85
action_105 (27) = happyGoto action_108
action_105 _ = happyReduce_60

action_106 _ = happyReduce_45

action_107 _ = happyReduce_16

action_108 _ = happyReduce_62

action_109 (56) = happyShift action_111
action_109 _ = happyReduce_21

action_110 _ = happyReduce_23

action_111 (31) = happyShift action_37
action_111 (34) = happyShift action_22
action_111 (36) = happyShift action_39
action_111 (40) = happyShift action_40
action_111 (41) = happyShift action_41
action_111 (54) = happyShift action_7
action_111 (55) = happyShift action_8
action_111 (57) = happyShift action_42
action_111 (58) = happyShift action_43
action_111 (59) = happyShift action_9
action_111 (60) = happyShift action_44
action_111 (61) = happyShift action_45
action_111 (62) = happyShift action_10
action_111 (63) = happyShift action_46
action_111 (64) = happyShift action_2
action_111 (65) = happyShift action_47
action_111 (66) = happyShift action_48
action_111 (4) = happyGoto action_24
action_111 (5) = happyGoto action_25
action_111 (6) = happyGoto action_26
action_111 (13) = happyGoto action_112
action_111 (14) = happyGoto action_28
action_111 (18) = happyGoto action_29
action_111 (20) = happyGoto action_30
action_111 (21) = happyGoto action_31
action_111 (22) = happyGoto action_32
action_111 (23) = happyGoto action_33
action_111 (24) = happyGoto action_34
action_111 (25) = happyGoto action_35
action_111 (26) = happyGoto action_36
action_111 _ = happyFail

action_112 _ = happyReduce_22

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn4
		 (Ident happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn5
		 ((read happy_var_1) :: Integer
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (PT _ (TD happy_var_1)))
	 =  HappyAbsSyn6
		 ((read happy_var_1) :: Double
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  8 happyReduction_5
happyReduction_5 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (Program happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happyReduce 6 9 happyReduction_6
happyReduction_6 ((HappyAbsSyn14  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (FnDef happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_1  10 happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 ((:[]) happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  10 happyReduction_8
happyReduction_8 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  11 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn11
		 (Arg happy_var_1 happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_0  12 happyReduction_10
happyReduction_10  =  HappyAbsSyn12
		 ([]
	)

happyReduce_11 = happySpecReduce_1  12 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 ((:[]) happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  12 happyReduction_12
happyReduction_12 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  13 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn13
		 (Empty
	)

happyReduce_14 = happySpecReduce_1  13 happyReduction_14
happyReduction_14 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (BStmt happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  13 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn13
		 (Decl happy_var_1 happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 4 13 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Ass happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_3  13 happyReduction_17
happyReduction_17 _
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn13
		 (Incr happy_var_1
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  13 happyReduction_18
happyReduction_18 _
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn13
		 (Decr happy_var_1
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  13 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Ret happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  13 happyReduction_20
happyReduction_20 _
	_
	 =  HappyAbsSyn13
		 (VRet
	)

happyReduce_21 = happyReduce 5 13 happyReduction_21
happyReduction_21 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Cond happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 7 13 happyReduction_22
happyReduction_22 ((HappyAbsSyn13  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (CondElse happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 5 13 happyReduction_23
happyReduction_23 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (While happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_2  13 happyReduction_24
happyReduction_24 _
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn13
		 (SExp happy_var_1
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  14 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (Block (reverse happy_var_2)
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_0  15 happyReduction_26
happyReduction_26  =  HappyAbsSyn15
		 ([]
	)

happyReduce_27 = happySpecReduce_2  15 happyReduction_27
happyReduction_27 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  16 happyReduction_28
happyReduction_28 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn16
		 (NoInit happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  16 happyReduction_29
happyReduction_29 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn16
		 (Init happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  17 happyReduction_30
happyReduction_30 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ((:[]) happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  17 happyReduction_31
happyReduction_31 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  18 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn18
		 (Int
	)

happyReduce_33 = happySpecReduce_1  18 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn18
		 (Doub
	)

happyReduce_34 = happySpecReduce_1  18 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn18
		 (Bool
	)

happyReduce_35 = happySpecReduce_1  18 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn18
		 (Void
	)

happyReduce_36 = happySpecReduce_0  19 happyReduction_36
happyReduction_36  =  HappyAbsSyn19
		 ([]
	)

happyReduce_37 = happySpecReduce_1  19 happyReduction_37
happyReduction_37 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn19
		 ((:[]) happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  19 happyReduction_38
happyReduction_38 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn19
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  20 happyReduction_39
happyReduction_39 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn20
		 (EVar happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  20 happyReduction_40
happyReduction_40 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn20
		 (ELitInt happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  20 happyReduction_41
happyReduction_41 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn20
		 (ELitDoub happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  20 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn20
		 (ELitTrue
	)

happyReduce_43 = happySpecReduce_1  20 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn20
		 (ELitFalse
	)

happyReduce_44 = happyReduce 4 20 happyReduction_44
happyReduction_44 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (EApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_45 = happyReduce 4 20 happyReduction_45
happyReduction_45 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (EAppS happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_46 = happySpecReduce_3  20 happyReduction_46
happyReduction_46 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  21 happyReduction_47
happyReduction_47 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (Neg happy_var_2
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_2  21 happyReduction_48
happyReduction_48 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (Not happy_var_2
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  21 happyReduction_49
happyReduction_49 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  22 happyReduction_50
happyReduction_50 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn29  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EMul happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  22 happyReduction_51
happyReduction_51 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  23 happyReduction_52
happyReduction_52 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn28  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EAdd happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  23 happyReduction_53
happyReduction_53 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  24 happyReduction_54
happyReduction_54 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (ERel happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  24 happyReduction_55
happyReduction_55 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  25 happyReduction_56
happyReduction_56 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EAnd happy_var_1 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  25 happyReduction_57
happyReduction_57 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  26 happyReduction_58
happyReduction_58 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EOr happy_var_1 happy_var_3
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  26 happyReduction_59
happyReduction_59 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_0  27 happyReduction_60
happyReduction_60  =  HappyAbsSyn27
		 ([]
	)

happyReduce_61 = happySpecReduce_1  27 happyReduction_61
happyReduction_61 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn27
		 ((:[]) happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  27 happyReduction_62
happyReduction_62 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn27
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  28 happyReduction_63
happyReduction_63 _
	 =  HappyAbsSyn28
		 (Plus
	)

happyReduce_64 = happySpecReduce_1  28 happyReduction_64
happyReduction_64 _
	 =  HappyAbsSyn28
		 (Minus
	)

happyReduce_65 = happySpecReduce_1  29 happyReduction_65
happyReduction_65 _
	 =  HappyAbsSyn29
		 (Times
	)

happyReduce_66 = happySpecReduce_1  29 happyReduction_66
happyReduction_66 _
	 =  HappyAbsSyn29
		 (Div
	)

happyReduce_67 = happySpecReduce_1  29 happyReduction_67
happyReduction_67 _
	 =  HappyAbsSyn29
		 (Mod
	)

happyReduce_68 = happySpecReduce_1  30 happyReduction_68
happyReduction_68 _
	 =  HappyAbsSyn30
		 (LTH
	)

happyReduce_69 = happySpecReduce_1  30 happyReduction_69
happyReduction_69 _
	 =  HappyAbsSyn30
		 (LE
	)

happyReduce_70 = happySpecReduce_1  30 happyReduction_70
happyReduction_70 _
	 =  HappyAbsSyn30
		 (GTH
	)

happyReduce_71 = happySpecReduce_1  30 happyReduction_71
happyReduction_71 _
	 =  HappyAbsSyn30
		 (GE
	)

happyReduce_72 = happySpecReduce_1  30 happyReduction_72
happyReduction_72 _
	 =  HappyAbsSyn30
		 (EQU
	)

happyReduce_73 = happySpecReduce_1  30 happyReduction_73
happyReduction_73 _
	 =  HappyAbsSyn30
		 (NE
	)

happyNewToken action sts stk [] =
	action 69 69 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS "(") -> cont 31;
	PT _ (TS ")") -> cont 32;
	PT _ (TS ",") -> cont 33;
	PT _ (TS "{") -> cont 34;
	PT _ (TS "}") -> cont 35;
	PT _ (TS ";") -> cont 36;
	PT _ (TS "=") -> cont 37;
	PT _ (TS "++") -> cont 38;
	PT _ (TS "--") -> cont 39;
	PT _ (TS "-") -> cont 40;
	PT _ (TS "!") -> cont 41;
	PT _ (TS "&&") -> cont 42;
	PT _ (TS "||") -> cont 43;
	PT _ (TS "+") -> cont 44;
	PT _ (TS "*") -> cont 45;
	PT _ (TS "/") -> cont 46;
	PT _ (TS "%") -> cont 47;
	PT _ (TS "<") -> cont 48;
	PT _ (TS "<=") -> cont 49;
	PT _ (TS ">") -> cont 50;
	PT _ (TS ">=") -> cont 51;
	PT _ (TS "==") -> cont 52;
	PT _ (TS "!=") -> cont 53;
	PT _ (TS "boolean") -> cont 54;
	PT _ (TS "double") -> cont 55;
	PT _ (TS "else") -> cont 56;
	PT _ (TS "false") -> cont 57;
	PT _ (TS "if") -> cont 58;
	PT _ (TS "int") -> cont 59;
	PT _ (TS "return") -> cont 60;
	PT _ (TS "true") -> cont 61;
	PT _ (TS "void") -> cont 62;
	PT _ (TS "while") -> cont 63;
	PT _ (TV happy_dollar_dollar) -> cont 64;
	PT _ (TI happy_dollar_dollar) -> cont 65;
	PT _ (TD happy_dollar_dollar) -> cont 66;
	PT _ (TL happy_dollar_dollar) -> cont 67;
	_ -> cont 68;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pProgram tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map prToken (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates/GenericTemplate.hs" #-}








{-# LINE 49 "templates/GenericTemplate.hs" #-}

{-# LINE 59 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

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
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 253 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

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

{-# LINE 317 "templates/GenericTemplate.hs" #-}
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
